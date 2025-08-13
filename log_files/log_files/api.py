from datetime import date
from pathlib import Path
from typing import Annotated, Literal

from fastapi import Depends, FastAPI, Query

from log_files.logs import deserialise_logs
from log_files.models import (
    Period,
    Log,
    MethodStatistics,
    UserActivity,
    UserRuntime,
)
from log_files.statistics import (
    method_statistics,
    user_activity,
    user_runtimes,
    longest_running_methods,
    busiest_periods,
    busiest_day_by_active_users,
    busiest_day_by_total_time,
)


class Session:
    def get_logs(self, start_date: date, end_date: date) -> list[Log]:
        return list(deserialise_logs(Path(__file__).parent.parent / "data"))


def get_session() -> Session:
    return Session()


app = FastAPI()


@app.get("/method_statistics")
def get_method_statistics(
    start_date: Annotated[date, Query()],
    end_date: Annotated[date, Query()],
    session: Session = Depends(get_session),
) -> list[MethodStatistics]:
    return method_statistics(session.get_logs(start_date, end_date))


@app.get("/user_activity")
def get_user_activity(
    start_date: Annotated[date, Query()],
    end_date: Annotated[date, Query()],
    session: Session = Depends(get_session),
) -> list[UserActivity]:
    return user_activity(session.get_logs(start_date, end_date))


@app.get("/user_runtimes")
def get_user_runtimes(
    start_date: Annotated[date, Query()],
    end_date: Annotated[date, Query()],
    sort_by: Annotated[Literal["all", "highest", "lowest"], Query()] = "all",
    session: Session = Depends(get_session),
) -> list[UserRuntime]:
    return user_runtimes(session.get_logs(start_date, end_date), sort_by=sort_by)


@app.get("/longest_running_methods")
def get_longest_running_methods(
    start_date: Annotated[date, Query()],
    end_date: Annotated[date, Query()],
    by: Annotated[Literal["total", "mean"], Query()] = "total",
    session: Session = Depends(get_session),
) -> list[str]:
    return longest_running_methods(session.get_logs(start_date, end_date), by=by)


@app.get("/busiest_periods")
def get_busiest_periods(
    start_date: Annotated[date, Query()],
    end_date: Annotated[date, Query()],
    period_minutes: Annotated[int, Query()] = 30,
    session: Session = Depends(get_session),
) -> list[Period]:
    return busiest_periods(
        session.get_logs(start_date, end_date), period_minutes=period_minutes
    )


@app.get("/busiest_day")
def get_busiest_day(
    start_date: Annotated[date, Query()],
    end_date: Annotated[date, Query()],
    by: Annotated[Literal["active_users", "total_time"], Query()],
    session: Session = Depends(get_session),
) -> date:
    if by == "active_users":
        return busiest_day_by_active_users(session.get_logs(start_date, end_date))
    else:
        return busiest_day_by_total_time(session.get_logs(start_date, end_date))
