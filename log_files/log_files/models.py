from datetime import date
from pydantic import BaseModel


class MethodDurations(BaseModel):
    method_name: str

    mean: float
    median: float
    mode: float


class UserActivity(BaseModel):
    user_id: str
    days_active: list[date]


class UserDurations(BaseModel):
    user_id: str

    total_time_seconds: float
    mean_time_seconds: float
