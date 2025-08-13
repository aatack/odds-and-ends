from pydantic import BaseModel


class DurationStatistics(BaseModel):
    mean: float
    median: float
    mode: float
