#############################################################################
# MSc Earth Observation Exercise 5
# [Schmitt]
#############################################################################
# Selected approach: post-classification comparison (you perform a 
# classification of land cover for each composite dataset. You then use raster 
# algebra in R to convert the indivudal results into a change map.)
#############################################################################
# Load all packages you need here...
library(raster)
library(rgdal)
library(tidyverse)
library(randomForest)

# change raster options to store large rasters in temp files on disk
rasterOptions(maxmemory = 1e6)

# define the folder that contains your data...
data.path <- '<PATH_TO_YOUR_DATA_FOLDER>'

folders <- dir(data.path)
list1 <- list.files(paste0(data.path, folders[1]), full.names = TRUE)

#############################################################################
# 2) Prepare your data
# a) Mask the cloudy regions in your image. We provided cloud mask for each
# of the image stacks.
# b) Derive additional features from your data. Do you want to classify based
# on the spectral bands? What about vegetation indices, principal compents, 
# tasseled cap components? 
#############################################################################
# a) mask cloudy regions

im00_sta <- stack(list1[1])
im05_sta <- stack(list1[3])
im10_sta <- stack(list1[5])

# rename band columns
names(im00_sta) <- c("1", "2", "3", "4", "5", "6")
names(im05_sta) <- c("1", "2", "3", "4", "5", "6")
names(im10_sta) <- c("1", "2", "3", "4", "5", "6")

# mask 2000 image
mask00_sta <- stack(list1[2])
cf_2000 <- mask(im00_sta, mask00_sta, filename = "cf_2000", 
                 maskvalue = T, format = "ENVI", overwrite = T)

# plot images and mask layers
plot(im00_sta)
plot(mask00_sta)

# mask layers are empty, no clouds visible
# masking of images not necessary

# b) additional features - perform tasseled cap
# create data frames of the images

tcc <- matrix(c( 0.2043,  0.4158,  0.5524, 0.5741,  0.3124,  0.2303,
                 -0.1603, -0.2819, -0.4934, 0.7940, -0.0002, -0.1446,
                 0.0315,  0.2021,  0.3102, 0.1594, -0.6806, -0.6109),
              dimnames = list(c('blue', 'green', 'red', 'nIR', 'swIR1', 'swIR2'), c('bright', 'green', 'wet')),
              ncol = 3)

# # perform tasseled cap on the three Landsat stacks
tc_00 <- stack(sum(im00_sta * tcc[,1]), sum(im00_sta * tcc[,2]), sum(im00_sta * tcc[,3]))
tc_05 <- stack(sum(im05_sta * tcc[,1]), sum(im05_sta * tcc[,2]), sum(im05_sta * tcc[,3]))
tc_10 <- stack(sum(im10_sta * tcc[,1]), sum(im10_sta * tcc[,2]), sum(im10_sta * tcc[,3]))


#############################################################################
# 3) Training Data Collection
# a. Consider that the way you collect your training data varies with the 
# approach you choose in 1).
# b. As you noticed during the last exercises, collecting training data is 
# time consuming. Ancillary data can help you during the process.
# Do you have any ideas about other ancillary data that suits your classification 
# problem?
#############################################################################

# Random stratified sampling on the Landsat image from 2000 in QGIS
# 40 non-forest: ClassID 0
# 40 forest: ClassID 1 

td_00 <- readOGR(dsn = "'<PATH_TO_YOUR_DATA_FOLDER>'" , layer = "<LAYER_NAME>")

#############################################################################
# 4) Model Training & Classification
# a. Choose a model: Feel free to use any classifier. In case you�re interested 
# in the differences between classifiers, test more than one!
# b. Parametrize the model: Depending on your choice of classification model,
# think about how to make an informed decision about the model parameters. 
# Perform a sensitivity analysis wherever possible.
#############################################################################

# create data frame
imtd_00 <- raster::extract(im00_sta, td_00, df = T, sp =T) 

imtd_train <- cbind(td_00@data, imtd_00@data [,3:8])

# transform to factors
imtd_train$classID <- as.factor(imtd_train$classID)

# delete id column
imtd_train <- imtd_train[,-1]

# train classification model - 2000
rf_2000 <- randomForest(data = imtd_train, classID~., type = classification, ntree = 100)
rf_2000$err.rate
# plot(rf.2000)

# apply model to images
classified_00 <- predict(im00_sta, rf_2000, "classified_2000", format = "ENVI", overwrite = T)
# plot(classified.00)
classified_05 <- predict(im05_sta, rf_2000, "classified_2005", format = "ENVI", overwrite = T)
# plot(classified.05)
classified_10 <- predict(im10_sta, rf_2000, "classified_2010", format = "ENVI", overwrite = T)
# plot(classified.10)

# combine raster layers and write as file
all_stack <- stack((classified_00 * 100) + (classified_05 * 10) + classified_10)
plot(all_stack)
freq(all_stack)

writeRaster(all_stack, "'<PATH_TO_YOUR_FILE.envi'", format = "ENVI", datatype="INT2S", overwrite = T)
#############################################################################
# 5) Change Map Validation & Area Estimates
# a. We provided reference data for the study area as a shapefile. Use it to
# perform an area-adjusted accuracy assessment and calculate area estimates 
# from the reference data with the Excel Spreadsheet we provided in the ./code/ folder.
# b. Compare the area estimates produced from your map (pixel counts) with the area 
# estimates from 5a). What are the differences?
#############################################################################

# load reference data
ref_td <- readOGR(dsn = '<PATH_TO_YOUR_DATA_FOLDER>', 
                 layer = "<LAYER_NAME>" )

# create data frame
ref_td_0 <- raster::extract(all_stack, ref_td, sp =T)

# create numeric id
ref_td_0_df = transform(ref_td_0@data, id=paste0("classified_00", "classified_05", "classified_10"))

# transform numeric id into class
ref_td_0_df<- ref_td_0_df %>%
  mutate(class = ifelse(layer.1 == "111",1, #Stable Forest
                        ifelse(layer.1 == "555",5, #Stable Non-Forest
                               ifelse(layer.1 == "551",4, #Forest Gain
                                      ifelse(layer.1 == "511",4, #Forest Gain
                                             ifelse(layer.1 == "155",2, #Deforest. 2000-2005
                                                    ifelse(layer.1 == "115",3,  #Deforest. 2005-20010
                                                           ifelse(layer.1 == "515",0,  #Forest Gain/Loss?
                                                                  ifelse(layer.1 == "151",2,0)))))))))  #Deforest. 2000-2005
                                                              
# # remove all 0s
ref_td_0_df = ref_td_0_df[which(ref_td_0_df$class!=0),]

# transform to factor
ref_td_0_df$layer <- as.factor(ref_td_0_df$layer)

# transform to factor
ref_td_0_df$class <- as.factor(ref_td_0_df$class)

# accuracy assessment
confma <- table(ref_td_0_df$layer, ref_td_0_df$class)

UserAcc <- diag(confma)/rowSums(confma)
ProdAcc <- diag(confma)/colSums(confma)

sums <- vector()
for (i in 1:dim(confma)[1]){
  sums[i] <- confma[i,i]
}

# overall accuracy
OverAcc <- sum(sums)/sum(confma)

# class proportions
class_prop <- raster::freq(all_stack, useNA = "no")
class_prop$layer

