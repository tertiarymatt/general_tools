#' ---
#' title: "Functions for Performing Raster Operations"
#' author: "MS Patterson, matthewpatterson@usda.gov"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'

#' ### Required packages
#+ Packages
library(raster)
require(doParallel)
require(foreach)

#### These functions are for inputting and manipulating rasters ----------------

ingest_rasters <- function(location=getwd,  ext=".tif$", layernames=NULL,
                           sparecores=2) {
  #input a list of rasters and prep them for doing regression analysis 
  #and/or prediction
  require(raster)
  require(doParallel)
  require(foreach)
  
  #Read in list of files from directory, filter to ext files. 
  olddir <- getwd()
  setwd(location)
  files <- list.files()
  files <- files[grepl(ext, files)==T]
  
  # set up cluster and data for parallel operation
  ifelse(length(files) > detectCores() - sparecores, 
         ncores <- detectCores()- sparecores,
         ncores <-length(files))
  
  clus <- makeCluster(ncores)
  clusterEvalQ(clus, library(raster))
  opts <- tmpDir()
  env <- environment()
  clusterExport(clus, "opts", envir = env)
  clusterEvalQ(clus, rasterOptions(tmpdir=opts))
  registerDoParallel(clus)
  
  #Read in and output files. 
  Rasters <- foreach(i=1:length(files)) %dopar% brick(files[i])
  on.exit(stopCluster(clus))
  
  
  #name rasters, and rename raster layers to standard names
  names(Rasters) <- sub(ext, "", files)
  
  for (i in 1:length(Rasters)) {
    names(Rasters[[i]]) <- layernames
  } 
  
  setwd(olddir)
  return(Rasters)
}

slice_raster <- function(object=NULL, slices=4, sparecores=2) {
  # this function takes an input raster and slices into a grid of tiles,
  # it outputs a list of rasters. 
  
  #Load required packages
  require(raster)
  require(foreach)
  require(doParallel)
  
  #find x min, x max, divide range by slices
  Oext <- extent(object)
  Xdist <- xmax(Oext) - xmin(Oext)
  Ydist <- ymax(Oext) - ymin(Oext)
  Xincr <- Xdist/slices
  Yincr <- Ydist/slices
  
  #check, is increment an integer?
  if(Xincr%%1==0 & Yincr%%1==0) { 
    print(paste("Extent is evenly divisble into", slices^2, "pieces.")) 
  }
  else {
    stop("Extent is not evenly divisble into desired slices.")
  }
  
  #Create new extents
  NewExtents <- as.list(NULL)
  count <- 1
  for(row in 1:slices) {
    #need one set of extents for each row, which will have same Y extents
    for (col in 1:slices) {
      NewExtents[[count]] <- extent(xmin(Oext)+Xincr*(col-1), 
                                    xmin(Oext)+Xincr*col, 
                                    ymin(Oext)+Yincr*(row-1), 
                                    ymin(Oext)+Yincr*row)
      count <- count + 1
    }
  }
  #Crop input raster with each of the new extents, output that into a list.
  OutRasters <- as.list(NULL)
  
  # for(area in 1:length(NewExtents)) {
  #   OutRasters[[area]] <- crop(object, NewExtents[[area]])
  # }
  
  # set up cluster and data for parallel operation
  ncores = detectCores()- sparecores
  clus <- makeCluster(ncores)
  clusterEvalQ(clus, library(raster))
  opts <- tmpDir()
  env <- environment()
  clusterExport(clus, "opts", envir = env)
  clusterEvalQ(clus, rasterOptions(tmpdir=opts))
  registerDoParallel(clus)
  OutRasters <- foreach(i=1:slices^2) %dopar% crop(object, NewExtents[[i]])
  on.exit(stopCluster(clus))
  
  return(OutRasters)
}

cut_tiles <- function(source=NULL, rasterlist=NULL, sparecores=2){
  #Take a big raster, and turn it into a bunch of smaller rasters that 
  #correspond to tiles of interest
  #rasterlist should be a list of rasters
  require(raster)
  require(doParallel)
  require(foreach)
  
  sourceraster <- stack(source)
  
  #Crop each file to area of reference for each raster
  #set up cluster and data for parallel operation
  ifelse(length(rasterlist) > detectCores() - sparecores, 
         ncores <- detectCores()- sparecores,
         ncores <-length(rasterlist))
  clus <- makeCluster(ncores)
  clusterEvalQ(clus, library(raster))
  opts <- tmpDir()
  env <- environment()
  clusterExport(clus, "opts", envir = env)
  clusterEvalQ(clus, rasterOptions(tmpdir=opts))
  registerDoParallel(clus)
  
  tiles <- foreach(i=1:length(rasterlist)) %dopar% 
    crop(sourceraster, rasterlist[[i]])

  on.exit(stopCluster(clus))
  
  return(tiles)
}

resample_tiles <- function(rasterlist=NULL, snap=NULL, sparecores=2){
  #Take a list of small rasters that and resample to match the resolution
  #of another, larger raster
  #rasterlist should be a list of rasters
  require(raster)
  require(doParallel)
  require(foreach)
  
  snapraster <- stack(snap)
  
  #Crop each file to area of reference for each raster
  #set up cluster and data for parallel operation
  ifelse(length(rasterlist) > detectCores() - sparecores, 
         ncores <- detectCores()- sparecores,
         ncores <-length(rasterlist))
  clus <- makeCluster(ncores)
  clusterEvalQ(clus, library(raster))
  opts <- tmpDir()
  env <- environment()
  clusterExport(clus, "opts", envir = env)
  clusterEvalQ(clus, rasterOptions(tmpdir=opts))
  registerDoParallel(clus)
  
  tiles <- foreach(i=1:length(rasterlist)) %dopar% 
    resample(rasterlist[[i]], crop(snapraster, rasterlist[[i]]))
  
  on.exit(stopCluster(clus))
  
  return(tiles)
}

#### These functions manipulate shapefiles -------------------------------------

ingest_shapes <- function(location=getwd(), sparecores=2) {
  # Outputs a list of shapefiles, using a parallel operation to read them in. 
  require(raster)
  require(doParallel)
  require(foreach)
  
  
  #Read in list of files from directory, filter to .shp files. 
  olddir <- getwd()
  setwd(location)
  files <- list.files()
  files <- files[grepl(".shp", files)==T]
  
  # set up cluster and data for parallel operation
  ifelse(length(files) > detectCores() - sparecores, 
         ncores <- detectCores()- sparecores,
         ncores <-length(files))
  clus <- makeCluster(ncores)
  clusterEvalQ(clus, library(raster))
  opts <- tmpDir()
  env <- environment()
  clusterExport(clus, "opts", envir = env)
  clusterEvalQ(clus, rasterOptions(tmpdir=opts))
  registerDoParallel(clus)
  
  #Read in and output files. 
  OutShape <- foreach(i=1:length(files), .packages=c("raster")) %dopar% 
    shapefile(files[i])
  on.exit(stopCluster(clus))
  
  names(OutShape) <- sub(".shp", "", files)
  
  setwd(olddir)
  return(OutShape)
}

make_rasters <- function(files=NULL,  ref=NULL, field=NULL, background=0, 
                         sparecores=2) {
  #take a list of shapefiles (files) and turn them into a list of cropped 
  #rasters aligned to ref with a background value
  require(raster)
  require(doParallel)
  require(foreach)
  
  # set up cluster and data for parallel operation
  ifelse(length(files) > detectCores() - sparecores, 
         ncores <- detectCores()- sparecores,
         ncores <-length(files))
  clus <- makeCluster(ncores)
  clusterEvalQ(clus, library(raster))
  opts <- tmpDir()
  env <- environment()
  clusterExport(clus, "opts", envir = env)
  clusterEvalQ(clus, rasterOptions(tmpdir=opts))
  registerDoParallel(clus)
  
  
  #Add field to each file for making raster, if necessary
  if(is.null(field) ==T) { 
    for (i in 1:length(files)) {
      files[[i]]$rasterize <- 1
    } 
  }
  
  names <- names(files)
  message("Read in the files ", paste0(names, collapse=" "), ".")
  message("Starting cropping...")
  
  #Crop each file to area of reference
  howmanyin <- length(files)
  files <- foreach (i=1:length(files)) %dopar% crop(files[[i]], ref)
  names(files) <- names
  
  message("Checking for null outputs...")
  
  #Check for NULL outputs
  nullist <- NULL
  for (i in 1:howmanyin) {
    nullist[i] <- is.null(files[[i]])
  } 
  
  #Drop nulls
  dropped <- files[nullist]
  files <- files[!nullist]
  
  if(length(files)==0) {message("No shapefile data in area of interest!")}
  message(sum(nullist), " empty shapefiles. Any empty shapefiles will be 
                          replaced with a zeroed out raster.")
  
  message("Generating rasters. This will take a minute...")
  
  #Generate output rasters
  if(length(files)!=0) {OutRasters <- foreach(i=1:length(files)) %dopar% 
    rasterize(x = files[[i]], y = ref, field="rasterize", background=0)}
  
  #Generate 'empty' rasters
  if(length(dropped)!=0) {
    message("Generating zeroed rasters. This will take a minute...")
    Empties <- foreach(i=1:length(dropped), .packages=c("raster")) %dopar% 
      raster(x = extent(ref), resolution=res(ref), vals=0)
    
    #Assemble output list
    if (exists("OutRasters")==T) {OutRasters <-c(OutRasters, Empties)}
    else { OutRasters <- Empties}
  }
  
  on.exit(stopCluster(clus))
  
  #ensure CRS is correct
  for (i in 1:length(OutRasters)) {
    crs(OutRasters[[i]]) <- crs(ref)
  } 
  
  #add layer names back
  names(OutRasters) <- c(names(files), names(dropped))
  
  return(OutRasters)
}
