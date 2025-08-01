from functools import lru_cache
import torch
import torchvision


@lru_cache
def build_model() -> torch.nn.Sequential:
    """Load the EfficientNet B0 model for classifying images."""
    model = torchvision.models.efficientnet_b0(
        weights=torchvision.models.EfficientNet_B0_Weights.IMAGENET1K_V1
    )
    model.eval()

    transform = torchvision.models.EfficientNet_B0_Weights.IMAGENET1K_V1.transforms()

    return torch.nn.Sequential(
        _Unsqueeze(), transform, model, _Squeeze(), torch.nn.Softmax()
    )


class _Unsqueeze(torch.nn.Module):
    def __init__(self) -> None:
        super().__init__()

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        return x.unsqueeze(0)


class _Squeeze(torch.nn.Module):
    def __init__(self) -> None:
        super().__init__()

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        return x.squeeze(0)
