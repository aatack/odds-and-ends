from datetime import date, datetime, timedelta
import random
from uuid import uuid4
from pydantic import AwareDatetime, BaseModel
import pytz


class Log(BaseModel):
    timestamp: AwareDatetime
    user_id: str
    method_name: str
    duration_seconds: float


METHOD_NAMES = [
    "create_workflow",
    "update_workflow_status",
    "delete_workflow_by_id",
    "get_user_by_email",
    "list_users",
    "assign_task_to_user",
    "get_task_by_id",
    "update_task_status",
    "search_workflows_by_tag",
    "archive_completed_tasks",
]


def _generate_log(day: date) -> Log:
    return Log(
        timestamp=pytz.utc.localize(
            datetime(day.year, day.month, day.day)
            + timedelta(seconds=random.uniform(0, 86400))
        ),
        user_id=f"user-{uuid4()}",
        method_name=random.choice(METHOD_NAMES),
        duration_seconds=abs(random.normalvariate(3, 1)),
    )
