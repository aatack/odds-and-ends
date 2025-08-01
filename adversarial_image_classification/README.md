# Adversarial image classification

## Setup

Install [`uv`](https://github.com/astral-sh/uv).

In this folder, run:

```
uv sync
uv venv
```

To run notebooks you may need to do:

```
uv pip install ipykernel
```

## Quick start

See `run.ipynb`.

```python
from adversarial_image_classification.helpers import paste_image, render_tensor
from adversarial_image_classification.noise_addition import maximise_probability
from adversarial_image_classification.helpers import render_tensor

# This will pull in an image from the clipboard
image = paste_image()

# Interrupt the process to stop early.  Should take ~300 iterations
results = maximise_probability(image, "fountain", max_iterations=1000)

# Return the output as a PIL image
render_tensor(results.output_image)
```
