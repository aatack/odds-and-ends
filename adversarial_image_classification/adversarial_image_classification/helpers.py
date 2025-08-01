import torch
from torchvision.transforms.functional import pil_to_tensor
import pyperclipimg


def paste_image() -> torch.Tensor:
    """Paste an image, in 0-1 (3, W, H) format, as a torch tensor from the clipboard."""
    return pil_to_tensor(pyperclipimg.paste()) / 256
