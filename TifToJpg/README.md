# TifToJpg
Python scrips converting GeoTiff images to a JPG format for image processing in machine learning algorithms.

## `python convert_tif.py image_name.tif`

Given the name of a `.tif` image in the current directory, creates a `.jpg` image in the same directory with the name `image_name.jpg` which is a copy of the `.tif` image.

## `python convert_all_tifs.py`

Creates a `.jpg` image for each `.tif` image in the current directory.

## `python delete_all_jpgs.py`

Deletes all files with the `.jpg` extension in the current directory.

## `python merge_images.py image_one image_two`

Given two files which are assumed to be of equal size and in a `.jpg format`, create a third image in the same folder which is the result of joining `image_one` and `image_two` side by side.

## `python merge_all.py`

(WIP) Joins images of the `.tif` and `.jpg` type which have the same name (minus the extension), then creates a new image which is the merger of those two.
