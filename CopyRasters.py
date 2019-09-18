import arcpy, os
from arcpy import env
env.workspace = r"K:\WhiteRiver\Data\Imagery"
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")
outWorkspace = r"K:\WhiteRiver\Data\Imagery"
rasts = arcpy.ListRasters("*.tif")
arcpy.env.snapRaster = r"K:\WhiteRiver\Data\Imagery\landsat_post_clouds_replaced.img"
arcpy.env.extent = r"K:\WhiteRiver\Data\Imagery\landsat_post_clouds_replaced.img"
#arcpy.env.snapRaster = r"K:\WhiteRiver\Data\IMG\wrnf_post_clouds_replaced.img"
arcpy.env.overwriteOutput = True
for rast in rasts:
    rastFileName = os.path.splitext(rast)[0]
    rastOutName = rastFileName + ".img"                      
    arcpy.CopyRaster_management(rast,outWorkspace + "//" + rastOutName,"#","#","-2147483647","NONE","NONE","#","NONE","NONE")
    print rastOutName
