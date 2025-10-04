class BaseIntegerColumn[T]:
    def __init__(self, nullable: bool):
        self.nullable = nullable

    def serialise(self, value: T) -> int:
        raise NotImplementedError()

    def deserialise(self, value: int) -> T:
        raise NotImplementedError()


class IntegerColumn(BaseIntegerColumn[int]):
    def serialise(self, value: int) -> int:
        return value

    def deserialise(self, value: int) -> int:
        return value


class BaseRealColumn[T]:
    def __init__(self, nullable: bool):
        self.nullable = nullable

    def serialise(self, value: T) -> float:
        raise NotImplementedError()

    def deserialise(self, value: float) -> T:
        raise NotImplementedError()


class RealColumn(BaseRealColumn[float]):
    def serialise(self, value: float) -> float:
        return value

    def deserialise(self, value: float) -> float:
        return value


class BaseTextColumn[T]:
    def __init__(self, nullable: bool):
        self.nullable = nullable

    def serialise(self, value: T) -> str:
        raise NotImplementedError()

    def deserialise(self, value: str) -> T:
        raise NotImplementedError()


class TextColumn(BaseTextColumn[str]):
    def serialise(self, value: str) -> str:
        return value

    def deserialise(self, value: str) -> str:
        return value


class BaseBlobColumn[T]:
    def __init__(self, nullable: bool):
        self.nullable = nullable

    def serialise(self, value: T) -> bytes:
        raise NotImplementedError()

    def deserialise(self, value: bytes) -> T:
        raise NotImplementedError()


class BlobColumn(BaseBlobColumn[bytes]):
    def serialise(self, value: bytes) -> bytes:
        return value

    def deserialise(self, value: bytes) -> bytes:
        return value


Column = BaseIntegerColumn | BaseRealColumn | BaseTextColumn | BaseBlobColumn


def parse_column(column_type: dict | str) -> Column:
    return column_type  # Not correct
