#############################################################################
# MSc Earth Observation Exercise 2
# [Lara Schmitt]
#############################################################################

# Load all packages you need here...
library(raster)

# Change raster options to store large rasters in temp files on disk
rasterOptions(maxmemory = 1e-6)

# Define the folder that contains your data...
data.path <- '<PATH_TO_YOUR_DATA_FOLDER>'

#############################################################################
# 2)
#############################################################################

# Read MTL file
mtl <- list.files(data.path, pattern="MTL.txt$", recursive=T, full.names=T)
# (read the Landsat metadata file using "read.delim()")
mtl.txt <- read.delim(mtl[1], sep = '=', stringsAsFactors = F)

# bands needed: only the six reflective bands of the OLI sensor (3xVIS, nIR, 2xswIR)


#read DN files
bands <-list.files(data.path, pattern = "B[2-7].TIF$", recursive = T, full.names =T)
dn_stack <- stack(bands)

# use the pattern matching function grep() to find )
# extract thr necessary information for the top-opf-atmosphere conversion: REFLECTANCE_MULTI_BAND, 
# REFLECTANCE_ADD_BAND and SUN_ELEVATION:
# use the pattern matching function grep() to find the corresponding entries in the metadate file 

#Extract numeric values
REFLECTANCE_MULT_BAND <- as.numeric(mtl.txt[grep("REFLECTANCE_MULT_BAND",mtl.txt$GROUP),][2:7,2])
REFLECTANCE_ADD_BAND <- as.numeric(mtl.txt[grep("REFLECTANCE_ADD_BAND",mtl.txt$GROUP),][2:7,2])
SUN_ELEVATION <- as.numeric(mtl.txt[grep("SUN_ELEVATION",mtl.txt$GROUP),][,2])

#############################################################################
# 3) Convert the quantized pixel values (digital numers, DN) to top of atmosphere reflectance (TOA) following
# the "Conversion to TOA Reflectance (https://landsat.usgs.gov/using-usgs-landsat-8-product)
# equation: TOA_planetary_reflectance <- band-specific multiplicative rescaling factor from the 
# metadata (REFLECTANCE_MULTI_BAND_x) * DN + band-specific additive rescaling factor from the 
# metadata (REFLECTANCE_ADD_BAND_x)
#############################################################################

#the sun elevation angle provided in the metadata file is reported in degrees, but R's sin() function
# expects angles in radians

# Helper-function to convert degrees to radians
deg2rad <- function(deg){ (deg * pi) / (180) }

#convert Sun elevation in radians 
SUN_ELEVATION_RAD <- deg2rad(SUN_ELEVATION)


#Conversion to TOA Reflectance; apply the conversion to all bands in a stack simultaneously: 
TOA_1 <- REFLECTANCE_MULT_BAND*dn_stack + REFLECTANCE_ADD_BAND

# Conversion to TOA Reflectance with correctopm for solar angle
TOA_corr <- TOA_1/sin(SUN_ELEVATION_RAD)

# the landsat data is provided as integer values
# when applying the equation, the data will be cast to float

# re-convert to integer using a reflectance scaling factor of 10,000 during the conversion:
# scaling by factor 10,000

TOA_corr <- TOA_corr*10000

#############################################################################
# 4) Write the result of your TOA conversion to disk in ENVI format- Make sure to use an integer data 
# type to save disk space. 
#############################################################################

# save data
writeRaster((TOA_corr), "TOA", format="ENVI", overwrite = T)

SR_bands <- list.files(data.path, pattern="sr_band[2-7].tif$", recursive=T, full.names=T)
SR <- stack(SR_bands)
writeRaster(SR, "SR", format = "ENVI", overwrite = T)

#############################################################################
# 6) Landsat Collection 1 Band (BQA). Load the BQA band and find the three most frequent values
# usint the fre() function. 
#############################################################################

BQA<- list.files(data.path, pattern="BQA.TIF$", recursive=T, full.names=T)
BQA <- raster(BQA)
freq(BQA)

# Most frequent value: 1
# Second most frequent value: 2720
# Third most frequent value: 2800,0

#############################################################################
# 7) The band contains integer values. By decoding the integer values into 16 bit binary strings, we can read 
# the quality information for each pixel-
# Use tje intToBits()function to decode the three BQA values from 6) and decipher their meaning
#using the Landsat quality band documentation (https://landsat.usgs.gov/collectionqualityband)
#############################################################################

# Function to decode integer values into bits
rev(intToBits(1)[1:16])
rev(intToBits(2720)[1:16])
rev(intToBits(2800)[1:16])

# What do the most frequent values mean? Decode each integer value into bits and
# describe their meaning here:

# Most frequent value:
# Designated Fill - yes

# Second most frequent value:
# Cloud Confidence - low
# Cloud Shadow Confidence - low
# Snow/Ice Confidence - low
# Cirrus Confidence - low

# Third most frequent value:
# Cloud Confidence - high
# Cloud Shadow Confidence - high
# Snow/Ice Confidence - low
# Cirrus Confidence - low

#############################################################################
#8) By converting integer values into binary bits, we can extract specific attributes from the BQA.
# In a systematic manner by defining a function, which yields TRUE for binary codes with 0=1:
# fill_pixels <- function(x) {((intToBits(x)[1] == T))}
#Use indexing and combined Boolean expressions to define functions which return TRUE for

# 8a) high confidence clouds, high confidence cloud shadows and fill values 
#############################################################################

fill_pixels_a <- function(x) {((intToBits(x)[1] == T)) |
  ((intToBits(x)[6] == T) &
  (intToBits(x)[7] == T)) |
  ((intToBits(x)[8] == T) &
  (intToBits(x)[9] == T))} 

fill_pixels(676)

#############################################################################
# 8b) high and medium confidence clouds, high and medium confidence shadows and fill values
#############################################################################

fill_pixels_b <- function(x) {(intToBits(x)[1] == T) |
    (intToBits(x)[7] == T) |
    (intToBits(x)[9] == T)}
    
fill_pixels_b(704)

#############################################################################
# 9) Create a mask using the functions from 8a and 8b using calc(). Plot the mask and check 
# if clouds are labeled as 1 and non-clouds as 0. Write both masks to disk and open them in QGIS
# together with and RGB representation of the image. Which mask is more accurate?
#############################################################################

calc(BQA, fun = fill_pixels_a, filename = "mask_a.tif", format = "ENVI")
calc(BQA, fun = fill_pixels_b, filename = "mask_b.tif", format = "ENVI")

mask_a <-raster("mask_a.envi")
mask_b <-raster("mask_b.envi")
plot(mask_a)
plot(mask_b)

# Mask b shows the better results, small cloud snippets are covered as well.

#############################################################################
# 10) Load the SR data as a stack (VIS, nIR,  swIR) and use the mask() function to mask clouds,
# cloud shadows and fill values from the image. 
# Use the cloud mask which you found to be more accurate in 9). Make sure to specify the maskvalue
# argument accordingly. Write the masked SR stack to disk in the ENVI format. 
#############################################################################

# Create mask for SR
mask(SR, mask_b, maskvalue = T, filename = "mask_SR", format = "ENVI")
