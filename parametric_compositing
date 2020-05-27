#############################################################################
# MSc Earth Observation Exercise 8
# Function for creating cloud-free composites of multiple Landsat images
# Requires an input data.frame (here img_list) and eight compositing 
# parameters. Please see exercise sheet for further details.
#############################################################################
# This function requires one input data.frame and 8 input parameters:

# target_date: the target date for your composite in Date format (YYYY-MM-DD)

# W_DOY, W_year, W_cloud_dist: weights for the three available parameters DOY, 
# year and distance to clouds. Must be scaled between 0 and 1 and sum up to 1, 
# the higher the weight, the higher the importance of the criterion.

# max_DOY_offset, max_year_offset: Thresholds for the maximum allowed differences 
# between target DOY and acquisition DOY, as well target year and acquisition year.
# Images exceeding these thresholds (further away in time) will be fully ignored. 
# For instance, maxyearoffset=0 will not allow observations from a year other than 
# the target year. By choosing these parameters, you will determine whether you prefer 
# seasonal consistency (close to target DOY but from different years) over annual 
# consistency (observations from same year but potentially distant DOYs). Discuss the 
# parametrization in your group.


# min_cloud_dist, max_cloud_dist: The minimum and maximum distance to clouds. 
# min_cloud_dist=10 will exclude all observations which are less than 10 pixels 
# away from a cloud. The cloud scores are linearly scaled between the minimum 
# (score = 0) and maximum cloud distance (score = 1). Pixels with distances 
# above max_cloud_dist will receive a score of 1.



#############################################################################
# Loading required packages here...
library(raster)
library(lubridate)

# Change raster options to store large rasters in temp files on disk
rasterOptions(maxmemory = 1e6)

#############################################################################
# Function definition starts here
parametric_compositing <- function(img_list, target_date, 
                                   W_DOY, W_year, W_cloud_dist, 
                                   max_DOY_offset, max_year_offset, 
                                   min_cloud_dist, max_cloud_dist) {
  # Process duration
  tic <- Sys.time()
  print(paste('Start of compositing process: ', tic))
  print(paste('Target date: ', target_date))

  # Extract target DOY and year from target_date
  target_DOY <- yday(target_date)
  target_year <- year(target_date)
  
  # Ensure that weights are not greater than 1 
  if(sum(W_DOY, W_year, W_cloud_dist)!=1) { stop('Error: something wrong.') }
  
  #############################################################################
  # Calculate the scores for the DOY, year, and cloud distance criteria
  print('Calculating compositing scores')
  
  # Calculate DOY offset
  obs_DOY <- img_list$DOY
  DOY_score <- 1 - (abs(target_DOY - img_list$DOY) / max_DOY_offset)
  DOY_score[DOY_score<0] <- NA
  
  # Calculate year offset
  obs_year <- img_list$year
  year_score <- 1-(abs(target_year - obs_year) / max_year_offset)
  
  if (max_year_offset == 0) {year_score[obs_year==target_year] <- 1}
  year_score[year_score<0] <- NA
  
  # Get candidate images within max_DOY_offset and max_year_offset
  ix <- which(!is.na(DOY_score) & !is.na(year_score)) # filter NAs 
  if (length(ix)>1) { print(paste(length(ix),  'candidate images selected, calculating scores.')) }
  if (length(ix)<2) { stop('Another error because something is wrong.') }
  
  #Show time consumed by Selection of candidate images
  print(paste('Selection of candidate images time: ', Sys.time()))
  
  # Stack cloud distance layers of candidate images and reclassify 
  # values < min_cloud_dist to NA, and values > max_cloud_dist to max_cloud_dist
  cloud_dist <- stack(as.character(img_list$cloud_dist_files[ix]))
  cloud_dist <- reclassify(cloud_dist, rcl=c(0, min_cloud_dist, NA), right=NA, datatype='INT2S')
  cloud_dist <- reclassify(cloud_dist, rcl=c(max_cloud_dist, sqrt(nrow(cloud_dist)^2 + ncol(cloud_dist)^2), max_cloud_dist), right=NA, datatype='INT2S')
  
  # Calculating the cloud score based on set min and max distances
  cloud_score <- (cloud_dist - min_cloud_dist) / (max_cloud_dist - min_cloud_dist)
  
  # Calculating the overall score based on the set weights
  obs_score <- DOY_score[ix] * W_DOY + year_score[ix] * W_year + cloud_score * W_cloud_dist
  
  # Selecting the pixel with highest scores
  select <- which.max(obs_score)
  
  # Choosing only one, in case multiple pixels have the highest score
  candidates <- unique(select)
  
  #############################################################################
  # Fill composite image with pixels from the candidate images
  for (i in candidates){
    
    # 
    fill_image <- brick(as.character(img_list$image_files[ix[i]]), datatype='INT2S')
    
    # Create Composite "frame" 
    if (i == min(candidates)) { 
      composite <- brick(fill_image, values=FALSE) 
      dataType(composite) <- 'INT2S'
      values(composite) <- 0
    }
    
    print(paste0('Filling raster with acquisition from date ', img_list$date[ix[i]]))
    fill_image.masked <- mask(fill_image, select, maskvalue=i, inverse=T, updatevalue=0, datatype='INT2S')
    fill_image.masked[is.na(fill_image.masked)] <- 0
    composite <- composite + fill_image.masked
    
    #Plot each intermediate plot
    plot(composite)
    
    
    #Show time consumed by composite filling
    print(paste('Composite filling time: ', Sys.time()))
    
    
  }
  
  # Mask all values with NAs 
  composite_na <- mask(composite, select, maskvalue=NA, datatype='INT2S')
  
  #############################################################################
  # Print the amount of NA implemented
  print(paste('NAs: ', round(freq(composite[[1]], value=NA)/ncell(composite[[1]])*100, digits=3), ' %'))
  
  # We have difficulties understanding this part of the code
  # We need Simon back to help us out :)
  rcl_DOY <- matrix(ncol=2, data=c(candidates, obs_DOY[ix[candidates]]))
  select_DOY <- reclassify(select, rcl_DOY, datatype = 'INT2S')
  
  #...
  rcl_year <- matrix(ncol=2, data=c(candidates, obs_year[ix[candidates]]))
  select_year <- reclassify(select, rcl_year)

  #...
  output <- stack(composite_na, select_DOY, select_year)
  print(paste('End of compositing process: ', Sys.time()))
  
  #...
  return(output)
  
}

