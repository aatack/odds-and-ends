from functools import lru_cache
import requests


@lru_cache
def get_class_names() -> list[str]:
    response = requests.get(
        "https://raw.githubusercontent.com/pytorch/hub/master/imagenet_classes.txt"
    )
    response.raise_for_status()
    return response.text.splitlines()
