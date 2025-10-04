import json
from typing import Any


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


class JsonColumn(BaseTextColumn[dict | list | int | float | str | bool]):
    def serialise(self, value: dict | list | int | float | str | bool) -> str:
        return json.loads(value)

    def deserialise(self, value: str) -> dict | list | int | float | str | bool:
        return json.dumps(value)


def parse_column(column_type: Any) -> Column:
    default_columns = {
        "integer": IntegerColumn,
        "real": RealColumn,
        "text": TextColumn,
        "blob": BlobColumn,
    }

    if column_type in default_columns:
        return default_columns[column_type]()

    if isinstance(column_type, dict) and "array" in column_type:
        return JsonColumn()
