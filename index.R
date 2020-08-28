library(shinyWidgets)
library(raster)
library(rgdal)
library(gdalUtils)
library(ggplot2)
library(leaflet) 
library(stars)
library(plotly)
library(readr)
library(ggnewscale) # Multiple Fill and Color Scales in 'ggplot2'
library(scico) # Colour Palettes Based on the Scientific Colour-Maps 
library(ggrepel) # Automatically Position Non-Overlapping Text Labels with 'ggplot2'


## PROCESSING
swir.file<-"/archivio/home/pirotti/Google Drive/RAD_3451-1_SWIR_384me_SN3155_22000us_2020-02-11T144756_raw_rad.img"
vnir.file<-"/archivio/home/pirotti/Google Drive/RAD_3451-1_VNIR_1800_SN00852_22000us_2020-02-11T144756_raw_rad.img"

swir.file.out<-swir.file
swir.one.band<-raster::raster(swir.file)
raster::extension(swir.file.out)<-"tif"
gdalUtils::gdal_translate(swir.file, swir.file.out, srcwin = c(0, 0, 
                                                               xmax(swir.one.band)-1, 
                                                               ymax(swir.one.band)/2 ) )


vnir.file.out<-vnir.file
vnir.one.band<-raster::raster(vnir.file)
raster::extension(vnir.file.out)<-"tif"

gdalUtils::gdal_translate(vnir.file, vnir.file.out, srcwin = c(0, 0, 
                                                               xmax(vnir.one.band)-1, 
                                                               ymax(vnir.one.band)/2 ) )

