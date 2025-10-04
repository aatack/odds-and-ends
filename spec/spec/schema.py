from collections import defaultdict
import json
from pathlib import Path
from typing import NamedTuple

from spec.columns import Column, parse_column


class Table(NamedTuple):
    columns: dict[str, Column]
    indices: dict[str, list[str]]


def generate_schema(migrations: Path | str):
    tables: dict[str, Table] = defaultdict(lambda: Table({}, {}))

    for file in Path(migrations).glob("**/*.json"):
        for table, columns in json.loads(file.read_text()).items():
            for column_name, column_type in columns.items():
                if column_name == "__index":
                    for index_name, index_columns in column_type.items():
                        assert isinstance(index_name, str)

                        if index_columns is None:
                            tables[table].indices.pop(index_name, None)
                        else:
                            assert isinstance(index_columns, list)
                            assert all(isinstance(c, str) for c in index_columns)

                            tables[table].indices[index_name] = index_columns

                else:
                    if column_type is None:
                        tables[table].columns.pop(column_name, None)
                    else:
                        tables[table].columns[column_name] = parse_column(column_type)

    return dict(tables)
