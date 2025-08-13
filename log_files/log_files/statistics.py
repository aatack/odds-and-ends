import bisect
from collections import defaultdict
from datetime import date, datetime, timedelta
from typing import Literal
import statistics

from log_files.models import (
    BusiestPeriod,
    Log,
    MethodDurations,
    UserActivity,
    UserDurations,
)


def method_durations(logs: list[Log]) -> list[MethodDurations]:
    method_durations: dict[str, list[float]] = defaultdict(list)
    for log in logs:
        method_durations[log.method_name].append(log.duration_seconds)

    return [
        MethodDurations(
            method_name=method_name,
            mean=statistics.mean(durations),
            median=statistics.median(durations),
            mode=statistics.mode(durations),
        )
        for method_name, durations in method_durations.items()
    ]


def user_activity(logs: list[Log]) -> list[UserActivity]:
    user_days: dict[str, set[date]] = defaultdict(set)
    for log in logs:
        user_days[log.user_id].add(log.timestamp.date())

    return [
        UserActivity(user_id=user, days_active=sorted(days))
        for user, days in user_days.items()
    ]


def user_durations(
    logs: list[Log], *, sort_by: Literal["all", "highest", "lowest"] = "all"
) -> list[UserDurations]:
    user_logs: dict[str, list[float]] = defaultdict(list)
    for log in logs:
        user_logs[log.user_id].append(log.duration_seconds)

    durations = sorted(
        [
            UserDurations(
                user_id=user,
                total_time_seconds=sum(durations),
                mean_time_seconds=statistics.mean(durations),
            )
            for user, durations in user_logs.items()
        ],
        key=lambda user: user.total_time_seconds,
    )

    if sort_by == "highest":
        return durations[-5:][::-1]
    elif sort_by == "lowest":
        return durations[:5]
    else:
        return durations


def longest_methods(logs: list[Log], *, by: Literal["total", "mean"]) -> list[str]:
    method_times: dict[str, list[float]] = defaultdict(list)
    for log in logs:
        method_times[log.method_name].append(log.duration_seconds)

    times = {
        method_name: (sum if by == "total" else statistics.mean)(times)
        for method_name, times in method_times.items()
    }

    return [
        name
        for name, _ in sorted(times.items(), key=lambda pair: pair[1], reverse=True)
    ][:3]


def busiest_periods(logs: list[Log], period_minutes: int = 30) -> list[BusiestPeriod]:
    period = timedelta(minutes=period_minutes)

    day_timestamps: dict[date, list[datetime]] = defaultdict(list)
    for log in logs:
        day_timestamps[log.timestamp.date()].append(log.timestamp)

    return [
        BusiestPeriod(
            day=day,
            start=(start := _busiest_period_start(timestamps, period)),
            end=start + period,
        )
        for day, timestamps in day_timestamps.items()
    ]


def _busiest_period_start(timestamps: list[datetime], period: timedelta) -> datetime:
    # Index the timestamps to make the whole operation much faster
    indexed_timestamps = sorted(timestamps)

    def timestamps_in_period(start: datetime) -> int:
        return (
            bisect.bisect_right(indexed_timestamps, start + period)
            - bisect.bisect_right(indexed_timestamps, start)
            + 1
        )

    return max(timestamps, key=timestamps_in_period)


def busiest_day_by_active_users(logs: list[Log]) -> date:
    day_users: dict[date, set[str]] = defaultdict(set)
    for log in logs:
        day_users[log.timestamp.date()].add(log.user_id)
    return max(day_users.items(), key=lambda pair: len(pair[1]))[0]


def busiest_day_by_total_time(logs: list[Log]) -> date:
    day_time: dict[date, float] = defaultdict(lambda: 0)
    for log in logs:
        day_time[log.timestamp.date()] += log.duration_seconds
    return max(day_time.items(), key=lambda pair: pair[1])[0]
