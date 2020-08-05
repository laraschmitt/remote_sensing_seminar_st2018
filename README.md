# Seminar Remote Sensing
### Institute of Geography, HU Berlin, Summer Term 2018

This repository includes my assignments from the introductory course in **Remote Sensing in R** from the Institute of Geography at Humboldt University Berlin.

* ### Lab 1: Handling raster datasets in R
     * create raster stack
     * find common extent of several raster files 
     * crop raster stacks to common extent 
     * write raster files to disk 
     * create true-color and false-color representation of image stacks 
     * plotRGB function 
     * extract spectral profile at given coordinates
     * plot spectral profiles with ggplot2

* ### Lab 2: DN; TOA, BQA band & creating masks
     * pattern matching function grep()
     * convert quantitzed pixel values (DN) to top of atmosphere reflectance (TOA)
     * write raster file to disk in ENVI format
     * find the most frequent values in  a raster using the freq() function on the quality assessment band (BQA)
     * decode BQA values (integers) into 16-bit binary strings unsing the intToBits() function to access the quality information for a pixel 
     * create a mask unsing the calc() function 
     * apply mask on the SR stack 

* ### Lab 3: Load shapefile, extract spatial values, create boxplots
     * load shapefile with readOGR()
     * extract spatial values of raster image at point locations with extract()
     * create boxplots of SR measuments
 
* ### Lab 4: RandomForest classification model
     * create dataframe for training points
     * train classification model
     * explore model performance
     * assess out of bag error 
     * validation with confusion matrix 
     
* ### Lab 5:Post-classification comparision 
    * mask clouds
    * perform tasseled cap 
    * apply trained model to images with predict()
    
* ### Lab 6: Spectral-temporal metrics
    * NDVI, tasseled cap
    
* ### Lab 7: Seasonal subsets
    * phenological characteristics
    * generate map and export

* ### Lab 8: Parametric compositing
    * apply parametric compositing 
