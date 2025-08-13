from collections.abc import Iterator
from datetime import date, datetime, timedelta
from pathlib import Path
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


def _serialise_logs(logs: list[Log]) -> str:
    return "\n".join(
        f"{log.timestamp.timestamp():.0f}:{log.user_id}:"
        # Round to two decimal places so that mode statistics are meaningful
        f"{log.method_name}:{log.duration_seconds:.2f}"
        for log in sorted(logs, key=lambda log: log.timestamp)
    )


def generate_log_files(
    path: Path | str = Path(__file__).parent.parent / "data",
) -> None:
    path = Path(path)
    path.mkdir(parents=True, exist_ok=True)
    for day_number in range(3, 8):
        day = date(2025, 8, day_number)
        (path / str(day)).write_text(
            _serialise_logs(
                [_generate_log(day) for _ in range(random.randint(6000, 12000))]
            )
        )


def deserialise_logs(path: Path | str) -> Iterator[Log]:
    path = Path(path)
    if path.is_dir():
        for subpath in path.glob("**/*"):
            yield from deserialise_logs(subpath)

    else:
        for log in path.read_text().splitlines():
            timestamp, user_id, method_name, duration_seconds = log.split(":")
            yield Log(
                timestamp=pytz.utc.localize(datetime.fromtimestamp(int(timestamp))),
                user_id=user_id,
                method_name=method_name,
                duration_seconds=float(duration_seconds),
            )
