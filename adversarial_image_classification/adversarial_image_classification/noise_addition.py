import torch
from adversarial_image_classification.class_names import get_class_names
from adversarial_image_classification.model import build_model
from torch.optim import Adam
from tqdm import tqdm


def get_class_probability(image: torch.Tensor, class_name: str) -> torch.Tensor:
    return build_model()(image)[get_class_names().index(class_name)]


def maximise_probability(image: torch.Tensor, class_name: str) -> torch.Tensor:
    parameter = torch.nn.parameter.Parameter(image.clone())
    optimiser = Adam(params=[parameter], lr=3e-4)
    criterion = torch.nn.BCELoss()

    for _ in tqdm(range(10)):
        optimiser.zero_grad()
        probability = get_class_probability(parameter, class_name)
        loss = criterion(probability, torch.tensor(1.0))
        print(probability.item())
        loss.backward()
        optimiser.step()

    return parameter
