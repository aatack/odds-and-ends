import os


def has_correct_format(file_name):
    return len(file_name.split('.')) == 2


def strip_extension(file_name):
    if has_correct_format(file_name):
        return file_name.split('.')[0]


def get_extension(file_name):
    if has_correct_format(file_name):
        return file_name.split('.')[1]


def get_files_in_folder():
    return os.listdir(os.getcwd())


def get_files_of_type(extension):
    files = []
    for f in get_files_in_folder():
        if get_extension(f) == extension:
            files.append(f)
    return files


def join_files_on(type_one, type_two):
    files_one = get_files_of_type(type_one)
    files_two = get_files_of_type(type_two)
    for f1 in files_one:
        for f2 in files_two:
            if strip_extension(f1) == strip_extension(f2):
                os.system('python merge_images.py {} {}'.format(f1, f2))


join_files_on('tif', 'jpg')
