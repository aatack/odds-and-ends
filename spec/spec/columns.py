from typing import Any, NamedTuple


class NullableColumn(NamedTuple):
    type: "Column"


class TextColumn(NamedTuple):
    pass


class IntegerColumn(NamedTuple):
    pass


class RealColumn(NamedTuple):
    pass


class BlobColumn(NamedTuple):
    pass


class TimestampColumn(NamedTuple):
    pass


class JsonColumn(NamedTuple):
    pass


class ArrayColumn(NamedTuple):
    type: "Column"


class PointerColumn(NamedTuple):
    table: str


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
