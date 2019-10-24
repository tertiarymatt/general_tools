import arcpy, os

## Processing variables - EDIT THESE
workspace = r"D:\GIS_Database\AK_TCC\DEM\slope" 
#where all the quarter quads are stored

#new folder to put the ouptput resampled images into
outFolder = "D:\GIS_Database\AK_TCC\DEM\Tanana_slope"
if not os.path.isdir(outFolder):
	os.makedirs(outFolder)

#Dummy raster
#Dummyrast = arcpy.sa.Raster(r'D:\Region2\ImageBound12') 
#create this and make sure bigger than extent

# Project area to limit unneccessary processing 
ProjectArea = r'D:\GIS_Database\AK_TCC\Boundaries\Tanana.shp'

## Arcpy setup
arcpy.env.workspace = workspace
arcpy.env.overwriteOutput = True
arcpy.CheckOutExtension("Spatial")
arcpy.env.pyramid = "NONE"

###############################################################################
##
##
## PROCESSING 
##
##
###############################################################################

## Get reference extents
##ext1 = Dummyrast.extent
##xminS = ext1.XMin
##xmaxS = ext1.XMax
##yminS = ext1.YMin
##ymaxS = ext1.YMax

extA = arcpy.Describe(ProjectArea).extent
xminA = extA.XMin
xmaxA = extA.XMax
yminA = extA.YMin
ymaxA = extA.YMax

#Get list of images
allRasters = arcpy.ListRasters()

for rast in allRasters:
	#Get quarterquad raster extent
	quartquad = arcpy.sa.Raster(rast)
	ext2 = quartquad.extent
	xmin = ext2.XMin
	xmax = ext2.XMax
	ymin = ext2.YMin
	ymax = ext2.YMax

## Only process those rasters that intersect the study area
	if ((xmin >= xminA and xmin <= xmaxA) or (xmax >= xminA and xmax <= xmaxA)) 
	and ((ymin >= yminA and ymin <= ymaxA) or (ymax >= yminA and ymax <= ymaxA)):
		print 'working on', rast
	
                #concatonate the name of the out raster with the out folder
		outRast = outFolder + "\\" + "slope_" + rast
		
		#copy over
		arcpy.CopyRaster_management(rast, outRast, pixel_type = "32_BIT_FLOAT", 
		nodata_value = "-9999")
	
	else:
		print rast, 'is not in your study area!'

print 'done with all the rasters in:', workspace
