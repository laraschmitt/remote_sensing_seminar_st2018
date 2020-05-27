#############################################################################
MSc Earth Observation Exercise 3
[Lara Schmitt]
#############################################################################

#Load all packages you need here...
library(raster)
library(ggplot2)
library(reshape2)
library(reshape)
library(rgdal)

# Change raster options to store large rasters in temp files on disk
rasterOptions(maxmemory = 1e6)

# Define the folder that contains your data...
data.path <- '<PATH_TO_YOUR_DATA_FOLDER>'

#############################################################################
# 4) Load your shape using readOGR(). Extract the spatial values at your point
# locations from the March image using the extract() function. Specify sp = TRUE
# to append the spectral values to the point shapefile.
# Make sure the result of this task is an object of typedata.frame named sr.march. 
# Your sample points should be represented as rows and the measured variables
# as columns (i.e. point id, classID, confiID, and 6 spectral bands).
#############################################################################

training_data <-readOGR(dsn= ''<PATH_TO_YOUR_DATA_FOLDER>', layer="<name_of_your_layer")
plot(training_data)

sr.march <- stack('<PATH_TO_YOUR_file.envi>')
spatial_values <- extract(sr.march, training_data, sp = TRUE)
sr.march <- as.data.frame(spatial_values)

#############################################################################
# 5) Create boxplots of your surface reflectance measurements for all spectral
# bands, grouped according to the class number. We provided the necessary code
# in the code template. Make sure you understand what the melt() function is  
# doing. Feel free to adjust the plot layout.
#############################################################################

# Make sure the outcome of task 4) is a 9-column data.frame named sr.march
# The columns should be the point id, classID, confID, and 6 spectral bands

# note: we have 8 columns due to deleted "default" column in QGIS

sr.march.melt <- melt(sr.march, id.vars=c(1:2), measure.vars=c(3:8))

ggplot(sr.march.melt, aes(x=variable, y=value, color=classID)) +
  geom_boxplot() 

#############################################################################
# 6) For the extracted values of 4), calculate the NDVI and EVI. (given function)
# Remember, the values in our image are surface reflectance scaled by 10,000. 
# Next, perform the tasseled cap transformation of the extracted values. 
# (given coefficients). Consider matrix multiplication, using %*%
#############################################################################

# calculate NDVI and EVI
sr.march$Band.1a <- sr.march$Band.1 / 10000
sr.march$Band.2a <- sr.march$Band.2 / 10000
sr.march$Band.3a <- sr.march$Band.3 / 10000
sr.march$Band.4a <- sr.march$Band.4 / 10000
sr.march$Band.5a <- sr.march$Band.5 / 10000
sr.march$Band.6a <- sr.march$Band.6 / 10000

sr.march$NDVI_march <- (sr.march$Band.4 - sr.march$Band.3)/(sr.march$Band.4 + sr.march$Band.3)
sr.march$EVI <- 2.5 * ((sr.march$Band.4-sr.march$Band.3)/(sr.march$Band.4 + 6 *sr.march$Band.3 -7.5 * sr.march$Band.1 +1))

# perform the tasseled cap transformation of the extracted values
tcc <- matrix(c( 0.2043,  0.4158,  0.5524, 0.5741,  0.3124,  0.2303, 
                -0.1603, -0.2819, -0.4934, 0.7940, -0.0002, -0.1446,
                 0.0315,  0.2021,  0.3102, 0.1594, -0.6806, -0.6109), 
                dimnames = list(c('blue', 'green', 'red', 'nIR', 'swIR1', 'swIR2'), c('bright', 'green', 'wet')),
                ncol = 3)

# matrix multiplication 
tcc.march <- as.matrix(sr.march[3:8])%*% tcc
tcc.march.df <- as.data.frame(tcc.march)
sr.march <- merge(sr.march, tcc.march.df)

#############################################################################
# 7) Create boxplots of the vegetation indices and TC components calculated 
# in 6) similiar to 5).
#############################################################################

sr.march_ind_melt <- melt(sr.march, id.vars = c(1:2), measure.vars = c(17:18))
sr.march_TC_melt <- melt(sr.march, id.vars = c(1:2), measure.vars = c(19:21))

ggplot(sr.march_ind_melt, aes(x= variable, y = value))+
  geom_boxplot()
ggplot(sr.march_TC_melt, aes(x= variable, y = value))+
  geom_boxplot()

