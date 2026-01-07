environment(.libPaths)$.lib.loc=c("//hdrive/home/u141/nakhavali/R_libs/library/", environment(.libPaths)$.lib.loc)
.lib.loc = "//hdrive/home$/u141/nakhavali/R_libs/library/"
.libPaths("//hdrive/home$/u141/nakhavali/R_libs/library/")
library(sp, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/" )

library(raster, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/" )
library(dplyr, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/" )
library(sf, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/" )
library(ncdf4, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/" )
library(backports, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/" )
library(tzdb, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/" )
library(withr, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/" )
library(crayon, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/" )
library(tidyverse, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/" )
library(rgdal, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/" )
library(exactextractr, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/" )
library(terra, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/")

args <- commandArgs(trailingOnly=TRUE)

scenario <- as.integer(args)
station<- list()
x<-0
for (i in 1:71){
  if(i >1){x <- x+3000}
  station[[i]]<- (1+x):(3000+x)
}
station[[71]] <- 210001 : 212707


# ex_s_raster <- raster("//hdrive/home$/u141/nakhavali/Globio_grid/SimU_GLOBIOM.tif")
# ex_s_polygons <- rasterToPolygons(ex_s_raster, fun = function(x) {x > 0}, na.rm = TRUE)
# ex_s_sf <- st_as_sf(ex_s_polygons)
# st_write(ex_s_sf, "//hdrive/home$/u141/nakhavali/Globio_grid/SimU_GLOBIOM.shp")

# ex_s<- st_read("//hdrive/home$/u141/nakhavali/Future_climate_models/Base_grid1e6/Grd5_Europe_lsrfc10k.shp")
# exs_s<-st_read("//hdrive/home$/u141/nakhavali/Future_climate_models/Base_grid/Grd5_Europe_lsrfc10k.shp")
# ex_s <- st_read("//hdrive/home$/u141/nakhavali/Globio_grid/SimU_GLOBIOM.shp")
# exs_s <- st_read("//hdrive/home$/u141/nakhavali/Globio_grid/SimU_GLOBIOM.shp")
#
# l1<-0.136601
# l2<-0.218493
# l3<-0.226531
# l4<-0.239226
# l5<-0.179149
#
# ##### clay
#
# is_clay_0_5<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/CLYPPT_M_sl2_1km_ll.tif")
# is_clay_5_15<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/CLYPPT_M_sl3_1km_ll.tif")
# is_clay_15_30<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/CLYPPT_M_sl4_1km_ll.tif")
# is_clay_30_60<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/CLYPPT_M_sl5_1km_ll.tif")
# is_clay_60_100<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/CLYPPT_M_sl6_1km_ll.tif")
#
# exs <- st_transform(exs_s, st_crs(is_clay_0_5))
#
#
#
# r_is_clay_0_5<-crop(is_clay_0_5,exs)
# isric_clay_0_5<- raster::as.data.frame(r_is_clay_0_5,xy=TRUE)
#
# r_is_clay_5_15<-crop(is_clay_5_15,exs)
# isric_clay_5_15<- raster::as.data.frame(r_is_clay_5_15,xy=TRUE)
#
# r_is_clay_15_30<-crop(is_clay_15_30,exs)
# isric_clay_15_30<- raster::as.data.frame(r_is_clay_15_30,xy=TRUE)
#
# r_is_clay_30_60<-crop(is_clay_30_60,exs)
# isric_clay_30_60<- raster::as.data.frame(r_is_clay_30_60,xy=TRUE)
#
# r_is_clay_60_100<-crop(is_clay_60_100,exs)
# isric_clay_60_100<- raster::as.data.frame(r_is_clay_60_100,xy=TRUE)
#
# isric_clay <- isric_clay_15_30
# isric_clay[,1]<- isric_clay[,1]
# isric_clay[,2]<- isric_clay[,2]
# x<-NA
# x2<-NA
# x<-(as.data.frame(isric_clay_0_5[,3]))*l1
# x[,2]<- (isric_clay_5_15[,3])*l2
# x[,3]<- (isric_clay_15_30[,3])*l3
# x[,4]<- (isric_clay_30_60[,3])*l4
# x[,5]<- (isric_clay_60_100[,3])*l5
# x2 <- rowSums(x)
# isric_clay[,3] <- x2
#
# colnames(isric_clay) = c("longitude","latitude","clay")
# # Remove the raster layers that have been processed
# rm(is_clay_0_5, is_clay_5_15, is_clay_15_30, is_clay_30_60, is_clay_60_100)
# rm(r_is_clay_0_5, r_is_clay_5_15, r_is_clay_15_30, r_is_clay_30_60, r_is_clay_60_100)
# rm(isric_clay_0_5, isric_clay_5_15, isric_clay_15_30, isric_clay_30_60, isric_clay_60_100)
# rm(x, x2)
#
# # Optional: Run the garbage collector
# gc()
#
# ##### silt
#
# is_silt_0_5<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/SLTPPT_M_sl2_1km_ll.tif")
# is_silt_5_15<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/SLTPPT_M_sl3_1km_ll.tif")
# is_silt_15_30<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/SLTPPT_M_sl4_1km_ll.tif")
# is_silt_30_60<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/SLTPPT_M_sl5_1km_ll.tif")
# is_silt_60_100<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/SLTPPT_M_sl6_1km_ll.tif")
#
#
# r_is_silt_0_5<-crop(is_silt_0_5,exs)
# isric_silt_0_5<- raster::as.data.frame(r_is_silt_0_5,xy=TRUE)
#
# r_is_silt_5_15<-crop(is_silt_5_15,exs)
# isric_silt_5_15<- raster::as.data.frame(r_is_silt_5_15,xy=TRUE)
#
# r_is_silt_15_30<-crop(is_silt_15_30,exs)
# isric_silt_15_30<- raster::as.data.frame(r_is_silt_15_30,xy=TRUE)
#
# r_is_silt_30_60<-crop(is_silt_30_60,exs)
# isric_silt_30_60<- raster::as.data.frame(r_is_silt_30_60,xy=TRUE)
#
# r_is_silt_60_100<-crop(is_silt_60_100,exs)
# isric_silt_60_100<- raster::as.data.frame(r_is_silt_60_100,xy=TRUE)
#
# isric_silt <- isric_silt_15_30
# isric_silt[,1]<- isric_silt[,1]
# isric_silt[,2]<- isric_silt[,2]
# x<-NA
# x2<-NA
# x<-(as.data.frame(isric_silt_0_5[,3]))*l1
# x[,2]<- (isric_silt_5_15[,3])*l2
# x[,3]<- (isric_silt_15_30[,3])*l3
# x[,4]<- (isric_silt_30_60[,3])*l4
# x[,5]<- (isric_silt_60_100[,3])*l5
# x2 <- rowSums(x)
# isric_silt[,3] <- x2
#
# colnames(isric_silt) = c("longitude","latitude","silt")
#
# # Remove the raster layers that have been processed
# rm(is_silt_0_5, is_silt_5_15, is_silt_15_30, is_silt_30_60, is_silt_60_100)
# rm(r_is_silt_0_5, r_is_silt_5_15, r_is_silt_15_30, r_is_silt_30_60, r_is_silt_60_100)
# rm(isric_silt_0_5, isric_silt_5_15, isric_silt_15_30, isric_silt_30_60, isric_silt_60_100)
# rm(x, x2)
#
# # Optional: Run the garbage collector
# gc()
#
# ##### sand
#
# is_sand_0_5<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/SNDPPT_M_sl2_1km_ll.tif")
# is_sand_5_15<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/SNDPPT_M_sl3_1km_ll.tif")
is_sand_15_30<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/SNDPPT_M_sl4_1km_ll.tif")
# is_sand_30_60<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/SNDPPT_M_sl5_1km_ll.tif")
# is_sand_60_100<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/SNDPPT_M_sl6_1km_ll.tif")
#
#
# r_is_sand_0_5<-crop(is_sand_0_5,exs)
# isric_sand_0_5<- raster::as.data.frame(r_is_sand_0_5,xy=TRUE)
# gc()
#
# r_is_sand_5_15<-crop(is_sand_5_15,exs)
# isric_sand_5_15<- raster::as.data.frame(r_is_sand_5_15,xy=TRUE)
# gc()
#
# r_is_sand_15_30<-crop(is_sand_15_30,exs)
# isric_sand_15_30<- raster::as.data.frame(r_is_sand_15_30,xy=TRUE)
# gc()
#
# r_is_sand_30_60<-crop(is_sand_30_60,exs)
# isric_sand_30_60<- raster::as.data.frame(r_is_sand_30_60,xy=TRUE)
# gc()
#
# r_is_sand_60_100<-crop(is_sand_60_100,exs)
# isric_sand_60_100<- raster::as.data.frame(r_is_sand_60_100,xy=TRUE)
# gc()
#
# isric_sand <- isric_sand_15_30
# isric_sand[,1]<- isric_sand[,1]
# isric_sand[,2]<- isric_sand[,2]
# x<-NA
# x2<-NA
# x<-(as.data.frame(isric_sand_0_5[,3]))*l1
# x[,2]<- (isric_sand_5_15[,3])*l2
# x[,3]<- (isric_sand_15_30[,3])*l3
# x[,4]<- (isric_sand_30_60[,3])*l4
# x[,5]<- (isric_sand_60_100[,3])*l5
# x2 <- rowSums(x)
# isric_sand[,3] <- x2
#
# colnames(isric_sand) = c("longitude","latitude","sand")
# # Remove the raster layers that have been processed
# rm(is_sand_0_5, is_sand_5_15, is_sand_15_30, is_sand_30_60, is_sand_60_100)
# rm(r_is_sand_0_5, r_is_sand_5_15, r_is_sand_15_30, r_is_sand_30_60, r_is_sand_60_100)
# rm(isric_sand_0_5, isric_sand_5_15, isric_sand_15_30, isric_sand_30_60, isric_sand_60_100)
# rm(x, x2)
#
# # Optional: Run the garbage collector
# gc()
#
# ##### soc
# is_soc_0_5<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/OCSTHA_M_sd1_1km_ll.tif")
# is_soc_5_15<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/OCSTHA_M_sd2_1km_ll.tif")
# is_soc_15_30<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/OCSTHA_M_sd3_1km_ll.tif")
# is_soc_30_60<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/OCSTHA_M_sd4_1km_ll.tif")
# is_soc_60_100<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/OCSTHA_M_sd5_1km_ll.tif")
#
# gc()
# r_is_soc_0_5<-crop(is_soc_0_5,exs)
# isric_soc_0_5<- raster::as.data.frame(r_is_soc_0_5,xy=TRUE)
#
# gc()
# r_is_soc_5_15<-crop(is_soc_5_15,exs)
# isric_soc_5_15<- raster::as.data.frame(r_is_soc_5_15,xy=TRUE)
#
# gc()
# r_is_soc_15_30<-crop(is_soc_15_30,exs)
# isric_soc_15_30<- raster::as.data.frame(r_is_soc_15_30,xy=TRUE)
#
# gc()
# r_is_soc_30_60<-crop(is_soc_30_60,exs)
# isric_soc_30_60<- raster::as.data.frame(r_is_soc_30_60,xy=TRUE)
#
# gc()
# r_is_soc_60_100<-crop(is_soc_60_100,exs)
# isric_soc_60_100<- raster::as.data.frame(r_is_soc_60_100,xy=TRUE)
# gc()
#
# isric_soc <- isric_soc_15_30
# isric_soc[,1]<- isric_soc[,1]
# isric_soc[,2]<- isric_soc[,2]
# x<-NA
# x2<-NA
# x<-(as.data.frame(isric_soc_0_5[,3]))*l1
# x[,2]<-(isric_soc_5_15[,3])*l2
# x[,3]<- (isric_soc_15_30[,3])*l3
# x[,4]<- (isric_soc_30_60[,3])*l4
# x[,5]<- (isric_soc_60_100[,3])*l5
# x2 <- rowSums(x)
# isric_soc[,3] <- x2
#
# colnames(isric_soc) = c("longitude","latitude","soc")
#
# # Remove the raster layers that have been processed
# rm(is_soc_5_15, is_soc_15_30, is_soc_30_60, is_soc_60_100)
# rm( r_is_soc_5_15, r_is_soc_15_30, r_is_soc_30_60, r_is_soc_60_100)
# rm(isric_soc_0_5, isric_soc_5_15, isric_soc_15_30, isric_soc_30_60, isric_soc_60_100)
# rm(x, x2)
#
# # Optional: Run the garbage collector
# gc()
#
# ##### nitrogen
# is_nitrogen_0_5_x<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/nitrogen_0-5cm_mean_1000.tiff")
# is_nitrogen_5_15_x<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/nitrogen_5-15cm_mean_1000.tiff")
# is_nitrogen_15_30_x<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/nitrogen_15-30cm_mean_1000.tiff")
# is_nitrogen_30_60_x<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/nitrogen_30-60cm_mean_1000.tiff")
# is_nitrogen_60_100_x<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/nitrogen_60-100cm_mean_1000.tiff")
#
# is_nitrogen_0_5 <- terra::project(is_nitrogen_0_5_x, crs(is_soc_0_5))
# is_nitrogen_5_15 <- terra::project(is_nitrogen_5_15_x, crs(is_soc_0_5))
# is_nitrogen_15_30 <- terra::project(is_nitrogen_15_30_x, crs(is_soc_0_5))
# is_nitrogen_30_60 <- terra::project(is_nitrogen_30_60_x, crs(is_soc_0_5))
# is_nitrogen_60_100 <- terra::project(is_nitrogen_60_100_x, crs(is_soc_0_5))
#
#
# gc()
# r_is_nitrogen_0_5<-crop(is_nitrogen_0_5,exs)
# isric_nitrogen_0_5<- raster::as.data.frame(r_is_nitrogen_0_5,xy=TRUE)
#
# gc()
# r_is_nitrogen_5_15<-crop(is_nitrogen_5_15,exs)
# isric_nitrogen_5_15<- raster::as.data.frame(r_is_nitrogen_5_15,xy=TRUE)
#
# gc()
# r_is_nitrogen_15_30<-crop(is_nitrogen_15_30,exs)
# isric_nitrogen_15_30<- raster::as.data.frame(r_is_nitrogen_15_30,xy=TRUE)
#
# gc()
# r_is_nitrogen_30_60<-crop(is_nitrogen_30_60,exs)
# isric_nitrogen_30_60<- raster::as.data.frame(r_is_nitrogen_30_60,xy=TRUE)
#
# gc()
# r_is_nitrogen_60_100<-crop(is_nitrogen_60_100,exs)
# isric_nitrogen_60_100<- raster::as.data.frame(r_is_nitrogen_60_100,xy=TRUE)
# gc()
#
# isric_nitrogen <- isric_nitrogen_15_30
# isric_nitrogen[,1]<- isric_nitrogen[,1]
# isric_nitrogen[,2]<- isric_nitrogen[,2]
# x<-NA
# x2<-NA
# x<-(as.data.frame(isric_nitrogen_0_5[,3]/100))*l1
# x[,2]<-(isric_nitrogen_5_15[,3]/100)*l2
# x[,3]<- (isric_nitrogen_15_30[,3]/100)*l3
# x[,4]<- (isric_nitrogen_30_60[,3]/100)*l4
# x[,5]<- (isric_nitrogen_60_100[,3]/100)*l5
# x2 <- rowSums(x)
# isric_nitrogen[,3] <- x2
#
# colnames(isric_nitrogen) = c("longitude","latitude","nitrogen")
#
# # Remove the raster layers that have been processed
# rm(is_nitrogen_0_5, is_nitrogen_5_15, is_nitrogen_15_30, is_nitrogen_30_60, is_nitrogen_60_100)
# rm(r_is_nitrogen_0_5, r_is_nitrogen_5_15, r_is_nitrogen_15_30, r_is_nitrogen_30_60, r_is_nitrogen_60_100)
# rm(isric_nitrogen_0_5, isric_nitrogen_5_15, isric_nitrogen_15_30, isric_nitrogen_30_60, isric_nitrogen_60_100)
# rm(x, x2)
#
# # Optional: Run the garbage collector
# gc()
#
# ##### AWC
# gc()
# is_wv_h3_0_5<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/WWP_M_sl2_1km_ll.tif")
# r_is_wv_h3_wv_0_5<-crop(is_wv_h3_0_5,exs)
# isric_wv_h3_0_5<- raster::as.data.frame(r_is_wv_h3_wv_0_5,xy=TRUE)
#
# gc()
# is_wv_h3_5_15<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/WWP_M_sl3_1km_ll.tif")
# r_is_wv_h3_5_15<-crop(is_wv_h3_5_15,exs)
# isric_wv_h3_5_15<- raster::as.data.frame(r_is_wv_h3_5_15,xy=TRUE)
#
# gc()
# is_wv_h3_15_30<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/WWP_M_sl4_1km_ll.tif")
# r_is_wv_h3_15_30<-crop(is_wv_h3_15_30,exs)
# isric_wv_h3_15_30<- raster::as.data.frame(r_is_wv_h3_15_30,xy=TRUE)
#
# gc()
# is_wv_h3_30_60<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/WWP_M_sl5_1km_ll.tif")
# r_is_wv_h3_30_60<-crop(is_wv_h3_30_60,exs)
# isric_wv_h3_30_60<- raster::as.data.frame(r_is_wv_h3_30_60,xy=TRUE)
#
# gc()
# is_wv_h3_60_100<- rast("//hdrive/home$/u141/nakhavali/ISRIC_DATA/WWP_M_sl6_1km_ll.tif")
# r_is_wv_h3_60_100<-crop(is_wv_h3_60_100,exs)
# isric_wv_h3_60_100<- raster::as.data.frame(r_is_wv_h3_60_100,xy=TRUE)
# gc()
#
# isric_awc <- isric_wv_h3_0_5
# x<- NA
# x2<-NA
#
# d1<-50
# d2<- 100
# d3<- 150
# d4<- 300
# d5<- 400
# x_1 <- (as.data.frame(isric_wv_h3_0_5$WWP_M_sl2_1km_ll))*1e-2*d1
# x_1[,2] <- isric_wv_h3_5_15$WWP_M_sl3_1km_ll*1e-2*d2
# x_1[,3] <- isric_wv_h3_15_30$WWP_M_sl4_1km_ll*1e-2*d3
# x_1[,4] <- isric_wv_h3_30_60$WWP_M_sl5_1km_ll*1e-2*d4
# x_1[,5] <- isric_wv_h3_60_100$WWP_M_sl6_1km_ll*1e-2*d5
#
# x2<- rowSums(x_1)
#
# isric_awc[,3] <- x2
# colnames(isric_awc) = c("longitude","latitude","awc")
#
# # Remove the raster layers that have been processed
# rm(is_wv_h3_0_5, is_wv_h3_5_15, is_wv_h3_15_30, is_wv_h3_30_60, is_wv_h3_60_100)
# rm(r_is_wv_h3_wv_0_5, r_is_wv_h3_5_15, r_is_wv_h3_15_30, r_is_wv_h3_30_60, r_is_wv_h3_60_100)
# rm(isric_wv_h3_0_5, isric_wv_h3_5_15, isric_wv_h3_15_30, isric_wv_h3_30_60, isric_wv_h3_60_100)
# rm(x, x2, x_1)
#
# # Optional: Run the garbage collector to reclaim memory
# gc()
#
# clay_raster <- rasterFromXYZ(isric_clay)
# silt_raster <- rasterFromXYZ(isric_silt)
# sand_raster <- rasterFromXYZ(isric_sand)
# soc_raster <- rasterFromXYZ(isric_soc)
# nitrogen_raster<-rasterFromXYZ(isric_nitrogen)
# awc_raster <- rasterFromXYZ(isric_awc)
#
#
# xx<-NA
# clay_ex<-NA
# silt_ex<-NA
# sand_ex<-NA
# soc_ex<-NA
# nitrogen_ex<-NA
# awc_ex<-NA
#
# crs(clay_raster) <- crs(is_soc_0_5)
# crs(silt_raster) <- crs(is_soc_0_5)
# crs(sand_raster) <- crs(is_soc_0_5)
# crs(soc_raster) <- crs(is_soc_0_5)
# crs(nitrogen_raster) <- crs(is_soc_0_5)
# crs(awc_raster) <- crs(is_soc_0_5)
#
# ez <- readOGR(dsn = "//hdrive/home$/u141/nakhavali/Globio_grid/SimU_GLOBIOM.shp")
# crs(ez) <- crs(clay_raster)
#
#
#
# isric_soil_data<- as.data.frame(ez$YLAT)
#
# save(list = ls(all.names = TRUE), file = "//hdrive/home$/u141/nakhavali/Globio_grid/my_environment.rsd")
load("//hdrive/home$/u141/nakhavali/Globio_grid/my_environment.rsd")

nitrogen_resampled <- resample(nitrogen_raster, soc_raster, method = "bilinear")

writeRaster(nitrogen_raster, filename = "//hdrive/home$/u141/nakhavali/Globio_grid/nitrogen.tif", format = "GTiff")

for (i in station[[scenario]]) {
  for(i in 1:100){
  xx <- terra::extract(clay_raster, ez[i,], exact = TRUE, method = "weighted")
  clay_ex[i] <- mean(as.numeric(na.omit(unlist(xx))))
  xx <- terra::extract(silt_raster, ez[i,], exact = TRUE, method = "weighted")
  silt_ex[i] <- mean(as.numeric(na.omit(unlist(xx))))
  xx <- terra::extract(sand_raster, ez[i,], exact = TRUE, method = "weighted")
  sand_ex[i] <- mean(as.numeric(na.omit(unlist(xx))))
  xx <- terra::extract(soc_raster, ez[i,], exact = TRUE, method = "weighted")
  soc_ex[i] <- mean(as.numeric(na.omit(unlist(xx))))
  xx <- terra::extract(nitrogen_raster, ez[i,], exact = TRUE, method = "weighted")
  nitrogen_ex[i] <- mean(as.numeric(na.omit(unlist(xx))))
  xx <- terra::extract(awc_raster, ez[i,], exact = TRUE, method = "weighted")
  awc_ex[i] <- mean(as.numeric(na.omit(unlist(xx))))
  isric_soil_data[i,1]<- ez$SU_GLOB[i]
  isric_soil_data[i,2] <- clay_ex[i]
  isric_soil_data[i,3] <- silt_ex[i]
  isric_soil_data[i,4] <- sand_ex[i]
  isric_soil_data[i,5] <- soc_ex[i]
  isric_soil_data[i,6] <- nitrogen_ex[i]
  isric_soil_data[i,7] <- awc_ex[i]

}

colnames(isric_soil_data) <- c("longitude","latitude","clay","silt","sand","soC","N","AWC","pHiHXO","pHiKCl")

write.table(isric_soil_data,"output/nakhavali/soil_globiom/isric_soil_data.txt",sep=",",col.names = TRUE)






