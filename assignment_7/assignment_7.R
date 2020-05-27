######################################################################
# MSc Earth Observation Exercise 7
# [Schmitt]
######################################################################

######################################################################
# Load all packages you need here...
library(raster)
library(rgdal)
library(caret)
library(tidyverse)
library(randomForest)
library (ggplot2)
library (reshape2)

# Change raster options to store large rasters in temp files on disk
rasterOptions(maxmemory = 1e6)

# Define the folder that contains your data...
data.path <- '<PATH_TO_YOUR_DATA_FOLDER>'

folders <- dir(data.path)
list_ts_stacks = list.files(paste0(data.path, folders[3]), full.names = TRUE)

# stack raster images

TCB <- stack(list_ts_stacks[2]) 
TCG <- stack(list_ts_stacks[4])
TCW <- stack(list_ts_stacks[6])

######################################################################
# 1) Extract the values at your training locations from the temporal 
# stacks (TCB, TCG, TCW). Compare the phenological behavior of the 
# classes with boxplots or line plots.
######################################################################

# load training data

td <- readOGR(dsn = "O:/Student_Data/Schmitt_Lara/S07/data/field_data", 
              layer = "pampas_2009_train")
plot(td)
# extract values at training locations

td_TCB <- raster:: extract (TCB, td, sp = T)
td_TCG <- raster:: extract (TCG, td, sp = T)
td_TCW <- raster:: extract (TCW, td, sp = T)

#Create dataframe
td_TCB_df = td_TCB@data
td_TCG_df = td_TCG@data
td_TCW_df = td_TCW@data

td_TCB_df = melt(data = td_TCB_df,id.vars =c(1:2),measure.vars=c(3:27)) 
td_TCB_df$class = as.factor(td_TCB_df$class)

td_TCG_df = melt(data = td_TCG_df,id.vars =c(1:2),measure.vars=c(3:27)) 
td_TCG_df$class = as.factor(td_TCG_df$class)

td_TCW_df = melt(data = td_TCW_df,id.vars =c(1:2),measure.vars=c(3:27)) 
td_TCW_df$class = as.factor(td_TCW_df$class)

# plot 

ggplot(td_TCB_df, aes(x=variable, y= value)) +
  geom_boxplot(aes(color= class))

ggplot(td_TCG_df, aes(x=variable, y= value)) +
  geom_boxplot(aes(color= class))

ggplot(td_TCW_df, aes(x=variable, y= value)) +
  geom_boxplot(aes(color= class))

# a) phenological characteristics


# b) differences between the classes

# metrics for soy vs other crops
# TCG max 1- 14 (20090711 - 20091116)

# metrics for soy vs grassland
# TCG 16-19 median (20100119 - 20100308)

# metrics for soy vs forest
# median TCB 1-25

# metrics for soy vs unvegetated
# TCB median 13-21 (nov-mar)(20091108 - 20100324)

# metrics for soy vx other crops
# TCG median 1-3 (jul-aug)



# c) seasons or periods, where the differences are most pronounced

# season july-nov (winter/spring) (TCG)-> max values of other crops are 
# significantly higher than max values of soybean

# season jan-mar (summer) (TCG)-> median values of grassland are significantly
# lower than median values of soybean

# whole growing period (TCB)-> median forest is significantly lower than
# median forest 

# season nov-mar (summer) (TCB) -> median values of unvegetated areas are
# higher than median values of soybean 
######################################################################
# 2) Compute at least two seasonal spectral-temporal metrics from the 
# time series stacks. We do this by subsetting relevant time periods  
# (i.e. bands) of interest from the stacks.
######################################################################

# create seasonal subsets

TCG_jul_nov <- TCG[[c(1:14)]]
TCG_jan_mar <- TCG[[c(6:19)]]
TCB_jul_jun <- TCG[[c(1:25)]]
TCB_nov_mar <- TCG[[c(13:21)]]
TCG_jul_aug <- TCG[[c(1:3)]]



# create matrix

TCG_jul_nov_M <-as.matrix(TCG_jul_nov)
TCG_jan_mar_M <-as.matrix(TCG_jan_mar)
TCB_jul_jun_M <-as.matrix(TCB_jul_jun)
TCB_nov_mar_M <-as.matrix(TCB_nov_mar)
TCG_jul_aug_M <-as.matrix(TCG_jul_aug)

# calculate spectemp

TCG_jul_nov_MAX <-apply(TCG_jul_nov_M, 1, FUN=max, na.rm=T)
TCG_jan_mar_MED<- apply(TCG_jan_mar_M, 1, FUN=median, na.rm=T)
TCB_jul_jun_MED <-apply(TCB_jul_jun_M, 1, FUN=median, na.rm=T)
TCB_nov_mar_MED <-apply(TCB_nov_mar_M, 1, FUN=median, na.rm=T)
TCG_jul_aug_MED <-apply(TCG_jul_aug_M, 1, FUN=median, na.rm=T)

# transform to raster

TCG_jul_nov_MAX_ras <- raster(nrows=TCG_jul_nov@nrows,
                           ncols=TCG_jul_nov@ncols,
                           crs=TCG_jul_nov@crs,
                           vals=TCG_jul_nov_MAX,
                           ext = extent(TCG_jul_nov)) # ext to change default extent

TCG_jan_mar_MED_ras <- raster(nrows=TCG_jan_mar@nrows,
                              ncols=TCG_jan_mar@ncols,
                              crs=TCG_jan_mar@crs,
                              vals=TCG_jan_mar_MED,
                              ext = extent(TCG_jan_mar))

TCB_jul_jun_MED_ras <- raster(nrows=TCB_jul_jun@nrows,
                              ncols=TCB_jul_jun@ncols,
                              crs=TCB_jul_jun@crs,
                              vals=TCB_jul_jun_MED,
                              ext = extent(TCB_jul_jun))

TCB_nov_mar_MED_ras <- raster(nrows=TCB_nov_mar@nrows,
                              ncols=TCB_nov_mar@ncols,
                              crs=TCB_nov_mar@crs,
                              vals=TCB_nov_mar_MED,
                              ext = extent(TCB_nov_mar))

TCG_jul_aug_MED_ras <- raster(nrows=TCG_jul_aug@nrows,
                              ncols=TCG_jul_aug@ncols,
                              crs=TCG_jul_aug@crs,
                              vals=TCG_jul_aug_MED,
                              ext = extent(TCG_jul_aug))

spectemp <- stack(TCG_jul_nov_MAX_ras, TCG_jan_mar_MED_ras, TCB_jul_jun_MED_ras,TCB_nov_mar_MED_ras, TCG_jul_aug_MED_ras)
plot(spectemp)

td_spectemp<-raster::extract(spectemp, td, sp=T)
td_spectemp_df <- td_spectemp@data

######################################################################
# 3) If you wish, select additional spectral-temporal metrics from the
# .spectemp/ folder, which you believe emphasize the differences between 
# the target classes. Create a stack of all metrics you computed / selected. 
# This stack will be used for classification in the next step.
# Try to keep the number of metrics low, the team with the highest 
# overall accuracy wins, but in case of similar accuracies, the lowest 
# number of metrics used for classification wins the competition.
######################################################################



######################################################################
# 4) Train a Random Forest model using the training point shapefile 
# (pampas_2009_train) and your spectral-temporal metrics. As always,
# you might want to make an informed decision about the ntree and mtry 
# parameters. If you�re motivated, feel free to try another 
# classification algorithm.
######################################################################

#Train random Forest

#Transform to factor
td_spectemp_df$class<- as.factor(td_spectemp_df$class)
#Subset, delete id column
td_spectemp_df <- td_spectemp_df[,-1]

#remove NAs
td_spectemp_df <-na.omit(td_spectemp_df)

#Train random Forest
rf_spectemp = randomForest(data=td_spectemp_df, class~.,ntree = 100)

plot(rf_spectemp)
rf_spectemp$err.rate[100,]

summary(td_spectemp_df)
######################################################################
# 5) Generate a map from your Random Forest model and write it to disk.
# Investigate the result in QGIS. Does the map appear plausible? How 
# about the spatial patterns and consistency of the results?
######################################################################


#Application of RandomForest model####
classified <- raster::predict(spectemp, rf_spectemp, inf.rm=T)

plot(classified)


#Write Raster

writeRaster(classified, filename = "Classified_spectemp",format= "ENVI", overwrite=T)



# map plausible?


######################################################################
# 6) Assess the overall and class-specific user�s and producer�s 
# accuracies of the map using the validation point shapefile 
# (pampas_2009_test). Create your confusion matrix in R, for instance 
# by using the table() function, and derive the overall and class-wise
# accuracy measures.

# Keep in mind: the validation points do not fulfill the criteria to 
# perform an area adjusted accuracy assessment (probability sample),
# since they were collected while driving alongside a road. We are thus 
# constrained to the unadjusted measures of accuracy.
######################################################################

# load Validation points
ref_td = readOGR(dsn = "O:/Student_Data/Schmitt_Lara/S07/data/field_data", 
                 layer = "pampas_2009_test")

#Create data frame
ref_td = raster::extract(classified, ref_td, sp =T)
ref_td_df = ref_td@data

ref_td_df = ref_td_df[,-1]
colnames(ref_td_df) = c("true","predicted")

#Transform into factors
ref_td_df$true = as.factor(ref_td_df$true)
ref_td_df$predicted = as.factor(ref_td_df$predicted)

# remove NAs
ref_td_df <- na.omit(ref_td_df)

#Accuracy assessment
confma <- table(ref_td_df$true, ref_td_df$predicted,exclude = 6)

UserAcc = diag(confma)/rowSums(confma)
ProdAcc = diag(confma)/colSums(confma)

sums <- vector()
for (i in 1:dim(confma)[1]){
  sums[i] <- confma[i,i]
}

OverAcc = sum(sums)/sum(confma)




