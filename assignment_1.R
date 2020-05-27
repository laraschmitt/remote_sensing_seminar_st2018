#############################################################################
# MSc Earth Observation Exercise 01
# [Schmitt]
#############################################################################

# Handling raster datasets in R 
# the Landsat images provided come as single-band files and should be stacked
# for further analysis
# for stacking, all input files must have a matching extent and the 
# identical projection

#############################################################################

# Load packages, use install.packages('packagename') to install if needed
library(raster)
library(rgdal)

# Change raster options to store large rasters in temp files on disk

rasterOptions(maxmemory = 1e-6)

# Define the folder that contains your data...
data.path <- 'L:/STUDIUM_Global_Change_Geography/M5_Earth_Observation/Seminar/S01/data/'

#############################################################################
# 1)	Create a stack for each of the 3 Landsat images. Include only the 
# following bands: blue, green, red, nIR, swIR 1, swiR 2 (in this order)
#############################################################################

#bands needed: blue, green, red, nIR, swIR1, swIR2
#band numbers: 2, 3, 4, 5, 6, 7

# list all folders in data.path
paths <-list.dirs(data.path, full.names = T)

# or using regular expressions
bands <-list.files(paste0(data.path, paths[1]),pattern="B[2-7].tif$", full.names=T) # $ (dollar sign): means that there should not be anything after TIF
stack <-stack(bands)


folders <- dir(data.path)
paste0(data.path, folders[1])
list.files(paste0(data.path, folders[1]), full.names = TRUE)[4:9]

stack01 <- stack(list.files(paste0(data.path, folders[1]), full.names = TRUE)[4:9])
stack02 <- stack(list.files(paste0(data.path, folders[2]), full.names = TRUE)[4:9])
stack03 <- stack(list.files(paste0(data.path, folders[3]), full.names = TRUE)[4:9])

#############################################################################
# 2) Each image has a slightly different extent. Find an efficient way to 
# identify the common extent (xmin, xmax, ymin, ymax in projected coordinates)
# of all three images. 
#############################################################################

e1 <- extent(x = stack01)
e2 <- extent(x = stack02)
e3 <- extent(x = stack03)

xmin<-max(e1[1],e2[1],e3[1])
xmax<-min(e1[2],e2[2],e3[2])
ymin<-max(e1[3],e2[3],e3[3])
ymax<-min(e1[4],e2[4],e3[4])

common_ex <-extent(xmin,xmax,ymin,ymax)

#############################################################################
# 3a)	Crop each of the image stacks from 1) to the common extent and create 
# a multi-temporal stack of the 3 images. Order them by acquisition day of 
# the year. 
#############################################################################
extent_a <-common_ex

stack01_crop_a <- crop(stack01, extent_a)
stack02_crop_a <- crop(stack02, extent_a)
stack03_crop_a <- crop(stack03, extent_a)

stack_a <- stack(stack01_crop_a,stack02_crop_a,stack03_crop_a)

#############################################################################
# 3b)	Crop each of the image stacks from 1) to the following custom extent:
# xmin = 250000, xmax = 375000, ymin = 5589000, ymax = 5653000
#############################################################################

extent_b <- extent(c(250000,375000,5589000,5653000))

stack01_crop_b <- crop(stack01, extent_b)
stack02_crop_b <- crop(stack02, extent_b)
stack03_crop_b <- crop(stack03, extent_b)

stack_b <- stack(stack01_crop_b,stack02_crop_b,stack03_crop_b)

#############################################################################
# 4) Write the cropped stacks from 3a and 3b to your personal folder. Use the 
# ENVI" format and the appropriate datatype. 
#############################################################################

setwd('L:/STUDIUM_Global_Change_Geography/M5_Earth_Observation/Seminar/S01/')
writeRaster(stack_a, "Stack_A", format = "ENVI", overwrite=T)
writeRaster(stack_b, "Stack_B", format = "ENVI", overwrite=T)

#############################################################################
# 5) Open the cropped images (3b) you wrote to disk in QGIS and create a 
# true-color and a false-color representation (swIR1, nIR, red) of all three
# images. 
#############################################################################

# Landsat 8 
# Band 1 - Ultra Blue (coastal/aerosol)	
# Band 2 - Blue	
# Band 3 - Green	
# Band 4 - Red	
# Band 5 - Near Infrared (NIR)
# Band 6 - Shortwave Infrared (SWIR) 
# Band 7 - Shortwave Infrared (SWIR) 
# Band 8 - Panchromatic	
# Band 9 - Cirrus	
# Band 10 - Thermal Infrared (TIRS) 
# Band 11 - Thermal Infrared (TIRS) 

# our stack: blue, green, nIR, swIR 1, swiR 2 
# bands for the 1st image: 
# band 1  - blue
# band 2 - green
# band 3 - red
# band 4 - nIR
# band 5 - swIR 1
# band 6 - swIR 2

# true color = RGB = 3,2,1
# false color = swIR1, nIR, red = 5,4,3  

#############################################################################
# 6) Use the plotRGB() function in R to create a true color and
# a false-color RGB (e.g. swIR1, nIR, red) plot of each of the image stacks 
# you generated in 3b).
#############################################################################
#true color
plotRGB(stack_b, r=3, g=2, b=1)

#false color
plotRGB(stack_b, r=5, g=4, b=3)

#############################################################################
# 7) Extract a spectral profile of coordinate x = 343225; y = 5605500 from 
# the multitemporal stack. 
#############################################################################

# several alternatives:

point <-data.frame('x'=343225, 'y'=5605500)
values <-extract(stack_a, point)

# or get the cell number of your coordinate in the stack
cell <-cellFromXY(stack_a, data.frame('x'=343225, 'y'=5605500))

# extract using the extract function or...
values <-extract(stack_a, cell)
# just use indexing
values <-stack_a[cell]


#############################################################################
# 8) Visualize the results from the extraction. Can you create one plot that
# represents a total of three spectral profiles accounting for the band
# wavelength and acquisition date?
#############################################################################

# create dataframe suited for plotting containing spectral values, DOY day and wavelength
df = data.frame(value = values[1:18],
                day = c(rep(69,6), rep(232,6),rep(309,6)),
                wavelength = rep(c(0.4825,0.5625,0.655,0.865,1.61,2.2),3))

# plot the spectral profile
library(ggplot2)

ggplot(data = df, aes(x = wavelength , y = value, col = as.factor(day)))+
  geom_line()+
  labs(title = "spectral profiles",
       x = "Wavelength [??m]",
       y = "DN",
       col = "aquisition DOY")
