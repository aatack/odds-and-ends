from PIL import Image
import sys


def strip_extension(file_name):
    return file_name.split('.')[0]


image_one_name = sys.argv[1]
image_two_name = sys.argv[2]

print("merging {} with {}".format(image_one_name, image_two_name))

with Image.open(image_one_name) as image_one, Image.open(image_two_name) as image_two:
    if image_one.size != image_two.size:
        raise Exception('Images must be of the same size')
    w = image_one.size[0]
    h = image_one.size[1]

    new_image = Image.new('RGB', (w * 2, h))

    new_image.paste(im=image_one, box=(0, 0))
    new_image.paste(im=image_two, box=(w, 0))

    new_image.save(strip_extension(image_one_name) + "_merge_" + strip_extension(image_two_name) + ".jpg")
