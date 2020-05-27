#############################################################################
# MSc Earth Observation Exercise 8
# [Derenthal, Schmitt, Streif, James]
#############################################################################

#############################################################################
library(raster)
library(lubridate)
library(ggplot2)
source('O:/Student_Data/Schmitt_Lara/S08/code/parametric_compositing.R')

# Change raster options to store large rasters in temp files on disk
rasterOptions(maxmemory = 1e6)

######## Define the folder that contains your data...
data.path <- '<PATH_TO_YOUR_DATA_FOLDER>'

#############################################################################
# 1)
#############################################################################

sr <- list.files(paste0(data.path, '/sr_data'), pattern=".tif$", full.names=T, recursive=F)
fmask <- list.files(paste0(data.path, '/fmask'), pattern=".tif$", full.names=T, recursive=F)
cloud_dist <- list.files(paste0(data.path, '/cloud_dist'), pattern=".tif$", full.names=T, recursive=F)

sta <- nchar(paste0(data.path,'/sr_data/LT05228082')) + 1
end <- sta + 7
dates <- substr(sr, sta, end)

sr.sorted <- sr[sort(dates, index.return=T)$ix] 
cd.sorted <- cloud_dist[sort(dates, index.return=T)$ix]

img_list <- data.frame("image_files"=as.character(sr.sorted), "cloud_dist_files"=as.character(cd.sorted) ,"date"=ymd(sort(dates)), "DOY"=yday(ymd(sort(dates))), "year"=year(ymd(sort(dates))))

#############################################################################
# 4) Relying on your insights from mapping soy cultivation in the Pampas 
# (Exercise 07), define two suitable target days of the year (DOY) to characterize 
# different stages of the soybean growing season, such as growing season onset
# or the peak of the growing season. These will be used as target_date parameters later on.
#############################################################################

target_date_1 <- ymd('20090124')
target_date_2 <- ymd('20090209')

#############################################################################
# 5) Make a decision concerning the compositing parameters explained in 1c, 
# 1d and 1e.
# Next, run the parametric_compositing() function with your parameters and 
# write the result to disk. Include the target DOY in the filename. While the 
# function executes, proceed with the next exercise.
#############################################################################

W_DOY <- 0.5
W_year <- 0.3
W_cloud_dist <- 0.2 
  
max_DOY_offset <- 21
max_year_offset <- 2

min_cloud_dist <- 10
max_cloud_dist <- 30

composite_20090124 <- parametric_compositing(img_list, target_date_1, 
                                       W_DOY, W_year, W_cloud_dist, 
                                       max_DOY_offset, max_year_offset, 
                                       min_cloud_dist, max_cloud_dist)

writeRaster(composite_20090124, filename = "Composite_20090124",format= "ENVI", overwrite=T)

#############################################################################
# 6) Make the compositing function more user friendly. Insert a couple of plot 
# and print commands to enable the users to follow the progress of the 
# compositing while the function is running.
#############################################################################
# We implemented plots of the composite layers in line 121 (parametric_compositing)

#############################################################################
# 7) Repeat the compositing for the second target DOY you specified in 3) and 
# write the results to disk.
#############################################################################

composite_20090209 <- parametric_compositing(img_list, target_date_2, 
                                             W_DOY, W_year, W_cloud_dist, 
                                             max_DOY_offset, max_year_offset, 
                                             min_cloud_dist, max_cloud_dist)

writeRaster(composite_20090209, filename = "Composite_20090209",format= "ENVI", overwrite=T)

#############################################################################
# 8) Visually inspect the quality of your compositing results in QGIS. What
# worked out well, what did not? How could the quality of the composites be 
# improved? Also look at the bands containing the DOY and year flags (band 7 and 8).
#############################################################################

# Our compositing parameters (#5) were set rather weak, so two images were enough
# to cover up the required pixels. More strict parameters, e.g. increased max_cloud_dist,
# could improve the quality of our results.
# 
# Based on the selection of only two images, the resulting composite seem to be inaccurate and fragmented. 
# It is therfore difficult to create a smooth composite, representing the onset or the peak of the growing season.

