from typing import NamedTuple
import torch
from adversarial_image_classification.class_names import get_class_names
from adversarial_image_classification.model import build_model
from torch.optim import Adam
from tqdm import tqdm


class AdversarialNoise(NamedTuple):
    target_class: str

    input_image: torch.Tensor
    noise: torch.Tensor

    @property
    def output_image(self) -> torch.Tensor:
        return self.input_image + self.noise

    @property
    def initial_probability(self) -> float:
        return get_class_probability(self.input_image, self.target_class).item()

    @property
    def final_probability(self) -> float:
        return get_class_probability(self.output_image, self.target_class).item()


def get_class_probability(image: torch.Tensor, class_name: str) -> torch.Tensor:
    return build_model()(image)[get_class_names().index(class_name)]


def maximise_probability(image: torch.Tensor, class_name: str) -> AdversarialNoise:
    noise = torch.nn.parameter.Parameter(torch.zeros_like(image))
    optimiser = Adam(params=[noise], lr=3e-4)
    criterion = torch.nn.BCELoss()

    for _ in tqdm(range(20)):
        optimiser.zero_grad()
        probability = get_class_probability(image + _constrain_noise(noise), class_name)
        loss = criterion(probability, torch.tensor(1.0))
        loss.backward()
        optimiser.step()

    print(noise.min(), noise.max())
    print(_constrain_noise(noise.data).min(), _constrain_noise(noise.data).max())

    return AdversarialNoise(class_name, image, _constrain_noise(noise.data))


def _constrain_noise(noise: torch.Tensor) -> torch.Tensor:
    return torch.nn.functional.tanh(noise) * 0.1
