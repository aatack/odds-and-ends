from collections import defaultdict
from datetime import date
from log_files.logs import Log
import statistics

from log_files.models import (
    MethodDurations,
    UserActivity,
    UserDurations,
    UserDurationsData,
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


def user_durations(logs: list[Log]) -> UserDurationsData:
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

    return UserDurationsData(
        users=durations, highest=durations[-5:][::-1], lowest=durations[:5]
    )
