#############################################################################
# MSc Earth Observation Exercise 6
# [Schmitt]
#############################################################################

# Load all packages you need here...
library(raster)
library(rgdal)
library(tidyverse)
library(randomForest)

# Change raster options to store large rasters in temp files on disk
rasterOptions(maxmemory = 1e6)

# Define the folder that contains your data...
data.path <- '<PATH_TO_YOUR_DATA_FOLDER>'

#Load classified map from 2010 
folders <- dir(data.path)
list1 = list.files(paste0(data.path, folders[1], "/"), full.names = TRUE)
classes2010 = stack(list1[1])
plot(classes2010)

#Load NDVI image
list2 = list.files(paste0(data.path, folders[6], "/"), full.names = TRUE)

NDVI = stack(list2[2])
TC = stack(list2[4],list2[6],list2[8])

#Clipping 
NDVI_mask = mask(x = NDVI,
                 mask = classes2010, 
                 maskvalue = 1,
                 filename= "NDVI_Mask", 
                 format = "ENVI", 
                 overwrite = T)

TC_mask = mask(x = TC,
               mask = classes2010, 
               maskvalue = 1,
               filename= "TC_Mask", 
               format = "ENVI", 
               overwrite = T)

plot(TC_mask)

#############################################################################
# 2)
#############################################################################
td <- readOGR(dsn = '<PATH_TO_YOUR_DATA_FOLDER>', 
                 layer = '<LAYER_NAME>')

#############################################################################
# 3) 
#############################################################################
NDVI = stack(list2[2])
TC = stack(list2[4],list2[6],list2[8])
classes2010 = stack('<PATH_TO_YOUR_DATA_FOLDER>')

#Extract the values at training sites from NDVI and TC (all 30 images)
td_NDVI = raster::extract(NDVI, td, sp =T)
td_TC = raster::extract(TC, td, sp =T)
#Create dataframe
td_NDVI_df = td_NDVI@data
td_TC_df = td_TC@data

#Rename columns
b = rep("B", 30)
z = 1:30
namen = paste0(b,z)
names(td_NDVI_df)[3:32]   <- namen

b = rep("B", 30)
z = 1:30
B = rep("B", 30)
G =  rep("G", 30)
W = rep("W", 30)

v = c(B,G,W)

namen = paste0(b,z,v)
names(td_TC_df)[3:92]   <- namen

td_NDVI_df = melt(data = td_NDVI_df,id.vars =c(1:2),measure.vars=c(3:32)) 
td_NDVI_df$class_id = as.factor(td_NDVI_df$class_id)

td_TCB_df = melt(data = td_TC_df,id.vars =c(1:2),measure.vars=c(3:32)) 
td_TCB_df$class_id = as.factor(td_TC_df$class_id)

td_TCG_df = melt(data = td_TC_df,id.vars =c(1:2),measure.vars=c(33:62)) 
td_TCG_df$class_id = as.factor(td_TC_df$class_id)

td_TCW_df = melt(data = td_TC_df,id.vars =c(1:2),measure.vars=c(63:92)) 
td_TCW_df$class_id = as.factor(td_TC_df$class_id)


ggplot(td_NDVI_df, aes(x=variable, y= value)) +
  geom_boxplot(aes(color= class_id))

ggplot(td_TCB_df, aes(x=variable, y= value, color= class_id)) +
  geom_boxplot()

ggplot(td_TCG_df, aes(x=variable, y= value, color= class_id)) +
  geom_boxplot()

ggplot(td_TCW_df, aes(x=variable, y= value, color= class_id)) +
  geom_boxplot()


sensor_date <- read.table(paste0(data.path, 'ts_stacks/Landsat_2010_Sensor_Date.csv'), sep=";", header=T)


#Choose spectral temporal metrics 
#Mean 
classes2010 = stack(list1[1])

#############################################################################
# 4) 
#############################################################################
# Name your selected metrics here:
#NDVI_mean
#NDVI_iqr
#TCB_p75
#TCB_p25
#TCG_std
#TCG_min
#TCW_max
#TCW_p50

#Import spectemp images
list_ndvi = list.files(paste0(data.path, folders[3], "/"), full.names = TRUE)[1]
list_tcb = list.files(paste0(data.path, folders[3], "/"), full.names = TRUE)[2]
list_tcg = list.files(paste0(data.path, folders[3], "/"), full.names = TRUE)[3]
list_tcw = list.files(paste0(data.path, folders[3], "/"), full.names = TRUE)[4]

ndvi_mean = stack('<PATH_TO_YOUR_DATA_FOLDER>')
ndvi_iqr = stack('<PATH_TO_YOUR_DATA_FOLDER>')

tcb_p75 = stack('<PATH_TO_YOUR_DATA_FOLDER>')
tcb_p25 = stack('<PATH_TO_YOUR_DATA_FOLDER>')

tcg_std = stack('<PATH_TO_YOUR_DATA_FOLDER>')
tcg_min = stack('<PATH_TO_YOUR_DATA_FOLDER>')

tcw_max = stack('<PATH_TO_YOUR_DATA_FOLDER>')
tcw_p50 = stack('<PATH_TO_YOUR_DATA_FOLDER>')

spectemp = stack(ndvi_mean,ndvi_iqr,tcb_p75,tcb_p25,tcg_std,tcg_min,tcw_max,tcw_p50)

#Extract the values at training sites from chosen spectemp images
td_spectemp = raster::extract(spectemp, td, sp =T)
#Create dataframe
td_spectemp_df = td_spectemp@data

#############################################################################
# 5)
#############################################################################

ndvi.stack <- stack(paste0(data.path, '/ts_stacks/NDVI_stack.img'))
ndvi.matrix <- as.matrix(ndvi.stack)

ndvi.median <- apply(ndvi.matrix, 1, FUN=median, na.rm=T)

ndvi.median.raster <- raster(nrows=ndvi.stack@nrows, ncols=ndvi.stack@ncols, crs=ndvi.stack@crs, vals=ndvi.median)
plot(ndvi.median.raster)

#############################################################################
# 6)
#############################################################################

#Train random Forest
#Transform to factor
td_spectemp_df$class_id <- as.factor(td_spectemp_df$class_id)
#Subset, delete id column
td_spectemp_df <- td_spectemp_df[,-1]

#Rename in order to ensure same column names
names(td_spectemp_df)[2:9] = c("Band.1", "Band.2", "Band.3", "Band.4", "Band.5", "Band.6", "Band.7", "Band.8")

#Train random Forest
rf_spectemp = randomForest(data=td_spectemp_df, class_id~.,ntree = 200, mtry=8)

plot(rf_spectemp)
rf_spectemp$err.rate[200,]

#Based on visual interprtation of the plot and the OOB errors, we identified 200 trees as appropiate
#We further use all variables at each split, as OOB errors are  minimised when selecting all variables. 

#############################################################################
# 7)
#############################################################################

#Clip spectemp with classified map from 2010
spectemp_mask = mask(x = spectemp,
                 mask = classes2010, 
                 maskvalue = 1,
                 filename= "SpecTemp_Mask", 
                 format = "ENVI", 
                 overwrite = T)

plot(spectemp_mask)



#Application of RandomForest model####
classified_0 = predict(spectemp_mask,rf_spectemp, "Classified_Spectemp", format = "ENVI", overwrite=T)
plot(classified_0)

#Combine 
classified_spectemp = merge(x = classified_0,y = (classes2010*4))

plot(classified_spectemp)

#Write Raster
writeRaster(classified_spectemp,filename = "Classified_SpecTemp_Forest",format= "ENVI", overwrite=T)



#############################################################################
# 8)
#############################################################################
#Validation
ref_td = readOGR(dsn = '<PATH_TO_YOUR_DATA_FOLDER>', 
                 layer = '<LAYER_NAME' )

#Create data frame
ref_td = raster::extract(classified_0, ref.td, sp =T)
ref_td_df = ref.td.0@data

colnames(ref_td_df) = c("true","predicted")

#Transform into factors
ref_td_df$true = as.factor(ref_td_df$true)
ref_td_df$predicted = as.factor(ref_td_df$predicted)



#Accuracy assessment
confma <- table(ref_td_df$true, ref_td_df$predicted)

UserAcc = diag(confma)/rowSums(confma)
ProdAcc = diag(confma)/colSums(confma)

sums <- vector()
for (i in 1:dim(confma)[1]){
  sums[i] <- confma[i,i]
}

OverAcc = sum(sums)/sum(confma)

raster::freq(classified_0, useNA = "no")

plot(class_all_sta)


