# alice bob clair
# bob clair dan

# c(alice, bob) == 1
# c(alice, dan) == 0


class Cooc:
    def __init__(self):
        self.lines: list[set[str]] = []

    def update(self, line: str):
        self.lines.append(set(line.split()))

    def get(self, a: str, b: str) -> int:
        return len([line for line in self.lines if (a in line) and (b in line)])
