from os.path import isdir, isfile
from typing import List
from os import listdir


def include_file(file_name: str) -> bool:
    """Implement this function to determine which files will be included."""
    return file_name.endswith(".py")


def list_all_children(
    folder_or_file: str, exclude_hidden_folders: bool = True
) -> List[str]:
    """List all the children of a folder, or return the input if it is a file."""
    if isfile(folder_or_file):
        return [folder_or_file]
    elif isdir(folder_or_file):
        if folder_or_file[-1] == "/":
            folder_or_file = folder_or_file[:-1]

        this_folder = folder_or_file.split("/")[-1]
        if this_folder.startswith(".") and len(this_folder) > 1:
            if exclude_hidden_folders:
                print(f"[INFO]: ignoring {folder_or_file}")
                return []

        if folder_or_file[-1] != "/":
            folder_or_file += "/"

        children = listdir(folder_or_file)
        output = []
        for child in children:
            output.extend(list_all_children(folder_or_file + child))
        return output
    else:
        raise Exception(f"invalid path: {folder_or_file}")


def count_cumulative_lines(files: List[str]) -> int:
    """Count the total lines in the given list of files."""
    count = 0
    for file_name in files:
        with open(file_name, "r") as open_file:
            count += len(open_file.readlines())
    return count


if __name__ == "__main__":
    print(
        "[INFO]: total number of lines is {}".format(
            count_cumulative_lines(
                [f for f in list_all_children("./") if include_file(f)]
            )
        )
    )
