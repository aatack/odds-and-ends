import os


def file_extension(file_name):
    if len(file_name.split('.')) != 2:
        return ""
    else:
        return file_name.split('.')[1]


def ending(n):
    if n == 1:
        return ''
    else:
        return 's'


files = os.listdir(os.getcwd())
jpgs = []
for f in files:
    if file_extension(f) == "jpg":
        jpgs.append(f)

print("Found {} jpg file{} to delete:".format(len(jpgs), ending(len(jpgs))))
for s in jpgs:
    print("|  " + s)

for f in jpgs:
    os.remove(f)
