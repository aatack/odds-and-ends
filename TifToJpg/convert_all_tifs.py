import os


def is_file_tif(file_name):
    if len(file_name.split('.')) == 2:
        return file_name.split('.')[1] == 'tif'
    else:
        return False


def ending(n):
    if n == 1:
        return ''
    else:
        return 's'


python_path = 'C:\\Users\\laptop\\Anaconda3\\python.exe'

files = os.listdir(os.getcwd())
tif_files = []
for f in files:
    if is_file_tif(f):
        tif_files.append(f)

print("Found {} .tif file{}:".format(str(len(tif_files)), ending(len(tif_files))))
for s in tif_files:
    print('|  ' + s)

for f in tif_files:
    os.system(python_path + ' convert_tif.py ' + f)
