from typing import Any, NamedTuple


class NullableColumn(NamedTuple):
    type: "Column"

    @property
    def sql(self) -> str:
        return self.type.sql.removesuffix(" not null")


class TextColumn(NamedTuple):

    @property
    def sql(self) -> str:
        return "text not null"


class IntegerColumn(NamedTuple):

    @property
    def sql(self) -> str:
        return "integer not null"


class RealColumn(NamedTuple):

    @property
    def sql(self) -> str:
        return "real not null"


class BlobColumn(NamedTuple):

    @property
    def sql(self) -> str:
        return "blob not null"


class TimestampColumn(NamedTuple):

    @property
    def sql(self) -> str:
        return "text not null"


class JsonColumn(NamedTuple):

    @property
    def sql(self) -> str:
        return "text not null"


class ArrayColumn(NamedTuple):

    @property
    def sql(self) -> str:
        return "text not null"


class PointerColumn(NamedTuple):

    @property
    def sql(self) -> str:
        return "text not null"


Column = (
    NullableColumn
    | TextColumn
    | IntegerColumn
    | RealColumn
    | BlobColumn
    | TimestampColumn
    | JsonColumn
    | PointerColumn
    | ArrayColumn
)


def parse_column(column_type: Any) -> Column:
    if column_type == "text":
        return TextColumn()

    elif column_type == "integer":
        return IntegerColumn()

    elif column_type == "real":
        return RealColumn()

    elif column_type == "blob":
        return BlobColumn()

    elif column_type == "timestamp":
        return TimestampColumn()

    elif column_type == "json":
        return JsonColumn()

    elif isinstance(column_type, str):
        return PointerColumn(column_type)

    elif isinstance(column_type, dict):
        if "nullable" in column_type:
            return NullableColumn(parse_column(column_type["nullable"]))

        if "array" in column_type:
            return ArrayColumn(parse_column(column_type["array"]))

    else:
        raise ValueError(f"Could not parse column type: {column_type}")
