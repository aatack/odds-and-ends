from functools import lru_cache
import torch
import torchvision


@lru_cache
def build_model() -> torch.nn.Sequential:
    model = torchvision.models.efficientnet_b0(
        weights=torchvision.models.EfficientNet_B0_Weights.IMAGENET1K_V1
    )
    model.eval()

    transform = torchvision.models.EfficientNet_B0_Weights.IMAGENET1K_V1.transforms()

    return torch.nn.Sequential(transform, model)
