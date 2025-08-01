import itertools
from typing import NamedTuple
import torch
from adversarial_image_classification.class_names import get_class_names
from adversarial_image_classification.helpers import render_tensor
from adversarial_image_classification.model import build_model
from torch.optim import Adam
from tqdm import tqdm
from PIL.Image import Image


class AdversarialNoise(NamedTuple):
    """
    Data pertaining to an adversarially altered image.

    Call `check()` to ensure the alteration satisfies some basic constraints.
    """

    target_class: str

    input_image: torch.Tensor
    noise: torch.Tensor

    @property
    def output_image(self) -> torch.Tensor:
        return (self.input_image + self.noise).clamp(0, 1)

    @property
    def initial_probability(self) -> float:
        return get_class_probability(self.input_image, self.target_class).item()

    @property
    def final_probability(self) -> float:
        return get_class_probability(self.output_image, self.target_class).item()

    def check(self) -> Image:
        # Image should be classified as the target class
        assert self.final_probability > 0.5

        # Image should be within reasonable bounds
        assert self.output_image.min() >= 0
        assert self.output_image.max() <= 1

        # The noise should have changed the image - otherwise something's gone badly
        # wrong
        assert self.noise.abs().sum() > 0

        # Return the image (automatically rendered inside notebooks) for a visual check
        # that the image looks like the original input image
        return render_tensor(self.output_image)


def get_class_probability(image: torch.Tensor, class_name: str) -> torch.Tensor:
    return build_model()(image)[get_class_names().index(class_name)]


def maximise_probability(
    image: torch.Tensor,
    class_name: str,
    desired_probability: float = 0.8,
    max_iterations: int | None = 200,
) -> AdversarialNoise:
    """
    Adversarially generate noise that makes an image "look like" something else.

    Noise is added to the input image such that it's visually indistinguishable from the
    original image to a human, but fools a computer vision model into thinking it's
    something else.

    The target class can be anything from the list of ImageNet classes - see
    `get_class_names`.
    """
    noise = torch.nn.parameter.Parameter(torch.zeros_like(image))
    optimiser = Adam(params=[noise], lr=3e-4)
    criterion = torch.nn.BCELoss()

    try:
        for step in tqdm(itertools.count()):
            if max_iterations is not None and step > max_iterations:
                break

            optimiser.zero_grad()
            probability = get_class_probability(
                (image + _constrain_noise(noise)).clamp(0, 1), class_name
            )
            loss = criterion(probability, torch.tensor(1.0))
            loss.backward()
            optimiser.step()

            if probability >= desired_probability:
                break
    except KeyboardInterrupt:
        pass

    return AdversarialNoise(class_name, image, _constrain_noise(noise.data))


def _constrain_noise(noise: torch.Tensor) -> torch.Tensor:
    limit = 0.1  # Higher values cause faster convergence
    return torch.nn.functional.tanh(noise) * limit
