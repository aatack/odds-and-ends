from datetime import date
from pydantic import AwareDatetime, BaseModel


class Log(BaseModel):
    timestamp: AwareDatetime
    user_id: str
    method_name: str
    duration_seconds: float


class MethodStatistics(BaseModel):
    method_name: str

    mean: float
    median: float
    mode: float


class UserActivity(BaseModel):
    user_id: str
    days_active: list[date]


class UserRuntime(BaseModel):
    user_id: str

    total_time_seconds: float
    mean_time_seconds: float


class Period(BaseModel):
    day: date

    start: AwareDatetime
    end: AwareDatetime
