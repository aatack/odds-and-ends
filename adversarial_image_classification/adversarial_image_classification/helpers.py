import torch
from torchvision.transforms.functional import pil_to_tensor
import pyperclipimg
from torchvision.transforms import ToPILImage
from PIL.Image import Image


def paste_image() -> torch.Tensor:
    """Paste an image, in 0-1 (3, W, H) format, as a torch tensor from the clipboard."""
    return pil_to_tensor(pyperclipimg.paste()) / 256


def render_tensor(image: torch.Tensor, normalise: bool = False) -> Image:
    if normalise:
        image -= image.min()
        image /= image.max()
    return ToPILImage()(image)
