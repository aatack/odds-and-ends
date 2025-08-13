from collections import defaultdict
from log_files.logs import Log
from log_files.models import DurationStatistics
import statistics


def method_duration_statistics(logs: list[Log]) -> dict[str, DurationStatistics]:
    method_durations: dict[str, list[float]] = defaultdict(list)
    for log in logs:
        method_durations[log.method_name].append(log.duration_seconds)

    return {
        method_name: DurationStatistics(
            mean=statistics.mean(durations),
            median=statistics.median(durations),
            mode=statistics.mode(durations),
        )
        for method_name, durations in method_durations.items()
    }
