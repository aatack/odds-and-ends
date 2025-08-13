# Log files

## Setup

1. Install `uv`.
2. Run `uv venv` and `uv sync` from this folder.
3. To generate log files, run:

   ```bash
   $ uv run python
   >>> from log_files.logs import generate_log_files
   >>> generate_log_files()
   ```

## Running

See `examples.ipynb` for a standalone demonstration of the statistics functions which runs without an API.

To run the API:

```bash
uv run uvicorn log_files.api:app --reload
```

Then, see the [docs page](http://127.0.0.1:8000/docs#/).
