from datetime import date
from pydantic import AwareDatetime, BaseModel


class Log(BaseModel):
    timestamp: AwareDatetime
    user_id: str
    method_name: str
    duration_seconds: float


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


class BusiestPeriod(BaseModel):
    day: date

    start: AwareDatetime
    end: AwareDatetime
