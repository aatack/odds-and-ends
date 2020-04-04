from osgeo import gdal
import sys

# Make destination name: sample.tif -> sample.jpg
src_filename = sys.argv[1]
name = src_filename.split('.')[0]
dst_filename = name + ".jpg"

print("converting {} to .jpg".format(src_filename))

src_ds = gdal.Open(src_filename)

format = "GTiff"
driver = gdal.GetDriverByName(format)

dst_ds = driver.CreateCopy(dst_filename, src_ds, 0)

# Close datasets by dereferencing
dst_ds = None
src_ds = None
