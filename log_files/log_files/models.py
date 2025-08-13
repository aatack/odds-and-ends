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


class UserDurationsData(BaseModel):
    users: list[UserDurations]

    highest: list[UserDurations]
    lowest: list[UserDurations]


class LongestMethods(BaseModel):
    longest_total_time: list[str]
    longest_mean_time: list[str]
