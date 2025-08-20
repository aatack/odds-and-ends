from collections import defaultdict
import itertools


class Cooc:
    def __init__(self):
        self.lines: list[set[str]] = []

    def update(self, line: str):
        self.lines.append(set(line.split()))

    def get(self, a: str, b: str) -> int:
        return len([line for line in self.lines if (a in line) and (b in line)])


class CachedCooc:
    def __init__(self):
        self.index: dict[tuple[str, str], int] = defaultdict(lambda: 0)

    def update(self, line: str):
        unique_strings = set(line.split())
        for a, b in itertools.combinations(unique_strings, unique_strings):
            self.index[tuple(sorted([a, b]))] += 1

    def get(self, a: str, b: str) -> int:
        return self.index[a, b]
