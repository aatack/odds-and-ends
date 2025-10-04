class BaseIntegerColumn[T]:
    def __init__(self, nullable: bool):
        self.nullable = nullable

    def serialise(self, value: T) -> int:
        raise NotImplementedError()

    def deserialise(self, value: int) -> T:
        raise NotImplementedError()


class BaseRealColumn[T]:
    def __init__(self, nullable: bool):
        self.nullable = nullable

    def serialise(self, value: T) -> float:
        raise NotImplementedError()

    def deserialise(self, value: float) -> T:
        raise NotImplementedError()


class BaseTextColumn[T]:
    def __init__(self, nullable: bool):
        self.nullable = nullable

    def serialise(self, value: T) -> str:
        raise NotImplementedError()

    def deserialise(self, value: str) -> T:
        raise NotImplementedError()


class BaseBlobColumn[T]:
    def __init__(self, nullable: bool):
        self.nullable = nullable

    def serialise(self, value: T) -> bytes:
        raise NotImplementedError()

    def deserialise(self, value: bytes) -> T:
        raise NotImplementedError()
