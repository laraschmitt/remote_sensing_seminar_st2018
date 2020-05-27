#############################################################################
# MSc Earth Observation Exercise 3
# Lara Schmitt
#############################################################################

# Load all packages you need here...
library(raster)
library(rgdal)
library(randomForest)

require(tidyr)
require(ggplot2)
require(rasterVis)


# Change raster options to store large rasters in temp files on disk
rasterOptions(maxmemory = 1e6)

# Define the folder that contains your data...
data.path <- 'L:/STUDIUM_Global_Change_Geography/Earth_Observation/S04/data/training_data_shapef'

#############################################################################
# 1) Load the training data points that you collected during the last exercise 
# and the March image in R. Use the extract() function to create a data frame 
# where each training point represents one row. Class labels (classID) as well
# as the spectral bands represents the columns. For the randomForest() function
# to recognize we want to train a classification and not a regression model, the
# dependent variable must be of type factor. 
#############################################################################

#Load training data points and the March image 
td <- readOGR('L:/STUDIUM_Global_Change_Geography/Earth_Observation/S04/data/training_data_shapef')
sr_m <- stack ("L:/STUDIUM_Global_Change_Geography/Earth_Observation/S04/data/sr_data/LC081890252014031001T1-SC20170927101754/LC081890252014031001T1-SC20170927101754_sr_masked_crop.envi")

#Create data frame
# extract(x= raster object, y = points represented by a two-column matrix or data.frame or spatial points,
# sp = boolean. add extracted values to the data.frame)
td_m= raster::extract(sr_m, td, sp =T)
td_m.df = td_m@data

#Transform to factor (dependent variable = classID)
td_m.df$classID <- as.factor(td_m.df$classID)
#Test
class(td_m.df$classID)

#############################################################################
# 2) Train a randomForest() classification model with the data.frame from 1).
# The algorithm cannot deal with NoData() values. Remove NAs from the data.frame 
# in case of errors. 
#############################################################################

#randomForest(x=dataframe or matrix of predictors, y = response vector)
td_m.df <- na.omit(td_m.df)
td_m.rf <- randomForest(x = td_m.df[,3:8], y = td_m.df[,1])

#############################################################################
# 3) The model object resulting from 2) contains a wealth of information on the
# model parameters and performance. See the function's help page for a detailed 
# list and explanations. Choose three components of the randomForest object which
# you believe are interesting for further investigation of model performance. 
#############################################################################

#interesting components
#1. ntree: Number of trees
#2. oob.times: number of time cases are "out of bag" (and thus used in computing
# OBB error estimate)
#3. forest: a list that contains the entire forest; NULL if randomFreost is run
# in unsupervised mode or if keep.forest = FALSE

#############################################################################
# 4) Repeat task 1 and 2 to train two additional randomForest models based on 
# multi-temporal image stacks of:
# a) the March and August images
# b) the March, August and November images
#############################################################################

# create stacks 
sr_m_aug <-stack("L:/STUDIUM_Global_Change_Geography/Earth_Observation/S04/data/sr_data/LC081890252014031001T1-SC20170927101754/LC081890252014031001T1-SC20170927101754_sr_masked_crop.envi",
                        "L:/STUDIUM_Global_Change_Geography/Earth_Observation/S04/data/sr_data/LC081890252014110501T1-SC20170927102137/LC081890252014110501T1-SC20170927102137_sr_masked_crop.envi"
                        )

sr_m_aug_nov <- stack("L:/STUDIUM_Global_Change_Geography/Earth_Observation/S04/data/sr_data/LC081890252014031001T1-SC20170927101754/LC081890252014031001T1-SC20170927101754_sr_masked_crop.envi",
                             "L:/STUDIUM_Global_Change_Geography/Earth_Observation/S04/data/sr_data/LC081890252014110501T1-SC20170927102137/LC081890252014110501T1-SC20170927102137_sr_masked_crop.envi",
                             "L:/STUDIUM_Global_Change_Geography/Earth_Observation/S04/data/sr_data/LC081890252015082001T1-SC20170927120710/LC081890252015082001T1-SC20170927120710_sr_masked_crop.envi"
)

#Create data frame
td_m_aug = raster::extract(sr_m_aug, td, sp =T)
td_m_aug.df = td_m_aug@data

td_m_aug_nov = raster::extract(sr_m_aug_nov, td, sp =T)
td_m_aug_nov.df = td_m_aug_nov@data

#Transform to factor (dependent variable = classID)
td_m_aug.df$classID <- as.factor(td_m_aug.df$classID)
td_m_aug_nov.df$classID <- as.factor(td_m_aug_nov.df$classID)

# apply randomForest function
td_m_aug.df <- na.omit(td_m_aug.df)
td_m_aug.rf <- randomForest(x = td_m_aug.df[,3:14], y = td_m_aug.df[,1])

td_m_aug_nov.df <- na.omit(td_m_aug_nov.df)
td_m_aug_nov.rf <- randomForest(x = td_m_aug_nov.df[,3:20], y = td_m_aug_nov.df[,1])

#############################################################################
# 5)	Assess the out of bad (OOB) error estimates of the three trained models
# (2, 4a, 4b).
# a) Which model has the lowest OOB error?
# b) How does the OOB behave when increasing the number of trees in your model 
# (ntrees)? You can access the OOB per number of trees included via err.rate.
# Can you use this information to determine a suitable ntrees?
# c) Which of the four classes has the highest OOB errors? Why is this the case?
#############################################################################

plot(td_m.rf)
td_m.rf$err.rate[500,]

plot(td_m_aug.rf)
td_m_aug.rf$err.rate[500,]

plot(td_m_aug_nov.rf)
td_m_aug_nov.rf$err.rate[500,]

# a) Which model has lowest OOB error rate? march-august-nov

barplot(
  c(
    td_m.rf$err.rate[ncol(td_m.rf$err.rate), 1],
    td_m_aug.rf$err.rate[ncol(td_m_aug.rf$err.rate), 1],
    td_m_aug_nov.rf$err.rate[ncol(td_m_aug_nov.rf$err.rate), 1]
  ) * 100,
  names.arg = c("March", "March & August", "All months")
)


# b) Could you determine a suitable ntrees? ntree < 100, around 150 

# for march:
diffs <- diff(td_m.rf$err.rate) %>% as.data.frame(.)
diffs$ID <- seq(nrow(diffs))
diffs <- gather(diffs, key = "col", value = "value", -ID)
ggplot(
  diffs,
  aes(
    x = ID,
    y = value,
    color = col
  )
) +
  geom_line() +
  theme_minimal()


# c) Which class has the highest OOB errors? class3

boxplot(
  td_m.rf$err.rate * 100,
  xlab = "Class",
  ylab = "Error [%]",
  main = "OOB errors"
)

#############################################################################
### 6) Train a final model with the best combination of images (5a) and 
# ntrees (5b). Perform a classification of the image stack using the
# predict() function. Write the resulting map to disk in ENVI format.
# When doing so, consider choosing the appropiate datatype argument. 
#############################################################################

best.rf = randomForest(x = td_m_aug_nov.df[,3:8], y = td_m_aug_nov.df[,1], ntree = 150)
plot(best.rf)
best.rf$err.rate[150,]

best.rf_raster <- predict(sr_m_aug_nov,best.rf)
predict(sr_m_aug_nov, best.rf, "classified_map", format = "ENVI", overwrite=T)

#############################################################################
### 7) We provided a set of validation points. Load them and extract the map
# values at these points. Create a confusion matrix from the resulting data.frame.
#############################################################################

# load validation points
valid_points <- readOGR("L:/STUDIUM_Global_Change_Geography/Earth_Observation/S04/data/validation")

#Extract values 
map_values = raster::extract(sr_m_aug_nov, valid_points, sp =T)
map_values.df = map_values@data

map_values.df$classID <- as.factor(map_values.df$classID)

#Train classification model  
map_values.df = na.omit(map_values.df)
map_values.rf = randomForest(x = map_values.df[,2:19], y = map_values.df[,1] )

predict(sr_m_aug_nov,map_values.rf, "valid_map", format = "ENVI", overwrite = T)

#Load raster
class_raster <- raster("L:/STUDIUM_Global_Change_Geography/Earth_Observation/S04/classified_map.envi")

#Extract values 
combined <- raster::extract(class_raster, valid_points, sp =T) 
combined.df =combined@data

combined.df$classID = as.factor(combined.df$classID)
combined.df$classified_map = as.factor(combined.df$classified_map)

#Create confusion matrix

map_values.df = map_values@data
map_values.df$classID <- as.factor(map_values.df$classID)

ConfMa <- confusionMatrix(combined.df$classified_map, map_values.df$classID) 


########### short solution:: 

valid_values <- raster::extract(best.rf_raster, valid, df = T)
valid_values <- cbind(valid@data, valid_values[,2])


colnames(valid_values) <- c("true", "predicted")

valid_values$true <- as.factor(valid_values$true)
valid_values$predicted <- as.factor(valid_values$predicted)

require(caret)
conf_matrix <- caret::confusionMatrix(
  data = valid_values$predicted,
  reference = valid_values$true
)

#############################################################################
### 8) Assess the class proportions using the freq() function. Make sure 
# exclude NAs, so the proportions of your 4 classes sum up to 1. 
#############################################################################

best.rf_raster_freq <- freq(best.rf_raster, useNA = "no") %>% as.data.frame(.)

