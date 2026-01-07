environment(.libPaths)$.lib.loc=c("//hdrive/home/u141/nakhavali/R_libs/library/", environment(.libPaths)$.lib.loc)
library(knitr, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/" )
.lib.loc = "//hdrive/home$/u141/nakhavali/R_libs/library/"
.libPaths("//hdrive/home$/u141/nakhavali/R_libs/library/")
library(tidyverse, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/")
library(dplyr, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/" )
library(raster, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/" )
library(sf, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/" )
library(ncdf4, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/" )
library(rgdal, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/" )
library(exactextractr, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/" )
library(terra, lib.loc="//hdrive/home$/u141/nakhavali/R_libs/library/")

args <- commandArgs(trailingOnly=TRUE)

scenario <- as.integer(args)
station<- list()
x<-0
for (i in 1:100){
  if(i >1){x <- x+156000}
  station[[i]]<- (1+x):(156000+x)
}
station[[100]] <- 15444001 : 15586234

ex_s<- st_read("//hdrive/home$/u141/nakhavali/BOKU_extract/Raster_Points_with_LatLon.shp")


st_crs(ex_s) <- 4326


l1=0.442084
l2=0.243368
l3=0.135399
l4=0.076127
l5=0.043243
l6= 0.0298895
l7= 0.0298895
##### clay
is_clay_0_20<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D1_Clay_Percentage.tif")
is_clay_20_40<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D2_Clay_Percentage.tif")
is_clay_40_60<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D3_Clay_Percentage.tif")
is_clay_60_80<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D4_Clay_Percentage.tif")
is_clay_80_100<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D5_Clay_Percentage.tif")
is_clay_100_100<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D5_Clay_Percentage.tif")
is_clay_100_150<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D6_Clay_Percentage.tif")
is_clay_150_200<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D7_Clay_Percentage.tif")

exs <- st_transform(ex_s, st_crs(is_clay_0_20))

r_is_clay_0_20<- crop(is_clay_0_20,exs)
hwsd2_clay_0_20<- raster::as.data.frame(r_is_clay_0_20,xy=TRUE)

r_is_clay_20_40<-crop(is_clay_20_40,exs)
hwsd2_clay_20_40<- raster::as.data.frame(r_is_clay_20_40,xy=TRUE)

r_is_clay_40_60<-crop(is_clay_40_60,exs)
hwsd2_clay_40_60<- raster::as.data.frame(r_is_clay_40_60,xy=TRUE)

r_is_clay_60_80<-crop(is_clay_60_80,exs)
hwsd2_clay_60_80<- raster::as.data.frame(r_is_clay_60_80,xy=TRUE)

r_is_clay_80_100<-crop(is_clay_80_100,exs)
hwsd2_clay_80_100<- raster::as.data.frame(r_is_clay_80_100,xy=TRUE)

r_is_clay_100_150<-crop(is_clay_100_150,exs)
hwsd2_clay_100_150<- raster::as.data.frame(r_is_clay_100_150,xy=TRUE)

r_is_clay_150_200<-crop(is_clay_150_200,exs)
hwsd2_clay_150_200<- raster::as.data.frame(r_is_clay_150_200,xy=TRUE)


hwsd2_clay <- hwsd2_clay_0_20
hwsd2_clay[,1]<- hwsd2_clay[,1]
hwsd2_clay[,2]<- hwsd2_clay[,2]
x<-NA
x2<-NA
x<-(as.data.frame(hwsd2_clay_0_20[,3]))*l1
x[,2]<- (hwsd2_clay_20_40[,3])*l2
x[,3]<- (hwsd2_clay_40_60[,3])*l3
x[,4]<- (hwsd2_clay_60_80[,3])*l4
x[,5]<- (hwsd2_clay_80_100[,3])*l5
x[,6]<- (hwsd2_clay_100_150[,3])*l6
x[,7]<- (hwsd2_clay_150_200[,3])*l7
x2 <- rowSums(x)
x2[x2<0]<-NA
hwsd2_clay[,3] <- x2

is_clay_0_20<- NULL
is_clay_20_40<- NULL
is_clay_40_60<- NULL
is_clay_60_80<- NULL
is_clay_80_100<- NULL
is_clay_100_150<- NULL
is_clay_150_200<- NULL
gc()

##### silt
is_silt_0_20<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D1_silt_Percentage.tif")
is_silt_20_40<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D2_silt_Percentage.tif")
is_silt_40_60<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D3_silt_Percentage.tif")
is_silt_60_80<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D4_silt_Percentage.tif")
is_silt_80_100<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D5_silt_Percentage.tif")
is_silt_100_100<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D5_silt_Percentage.tif")
is_silt_100_150<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D6_silt_Percentage.tif")
is_silt_150_200<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D7_silt_Percentage.tif")


r_is_silt_0_20<- crop(is_silt_0_20,exs)
hwsd2_silt_0_20<- raster::as.data.frame(r_is_silt_0_20,xy=TRUE)

r_is_silt_20_40<-crop(is_silt_20_40,exs)
hwsd2_silt_20_40<- raster::as.data.frame(r_is_silt_20_40,xy=TRUE)

r_is_silt_40_60<-crop(is_silt_40_60,exs)
hwsd2_silt_40_60<- raster::as.data.frame(r_is_silt_40_60,xy=TRUE)

r_is_silt_60_80<-crop(is_silt_60_80,exs)
hwsd2_silt_60_80<- raster::as.data.frame(r_is_silt_60_80,xy=TRUE)

r_is_silt_80_100<-crop(is_silt_80_100,exs)
hwsd2_silt_80_100<- raster::as.data.frame(r_is_silt_80_100,xy=TRUE)

r_is_silt_100_150<-crop(is_silt_100_150,exs)
hwsd2_silt_100_150<- raster::as.data.frame(r_is_silt_100_150,xy=TRUE)

r_is_silt_150_200<-crop(is_silt_150_200,exs)
hwsd2_silt_150_200<- raster::as.data.frame(r_is_silt_150_200,xy=TRUE)


hwsd2_silt <- hwsd2_silt_0_20
hwsd2_silt[,1]<- hwsd2_silt[,1]
hwsd2_silt[,2]<- hwsd2_silt[,2]
x<-NA
x2<-NA
x<-(as.data.frame(hwsd2_silt_0_20[,3]))*l1
x[,2]<- (hwsd2_silt_20_40[,3])*l2
x[,3]<- (hwsd2_silt_40_60[,3])*l3
x[,4]<- (hwsd2_silt_60_80[,3])*l4
x[,5]<- (hwsd2_silt_80_100[,3])*l5
x[,6]<- (hwsd2_silt_100_150[,3])*l6
x[,7]<- (hwsd2_silt_150_200[,3])*l7
x2 <- rowSums(x)
x2[x2<0]<-NA
hwsd2_silt[,3] <- x2

is_silt_0_20<- NULL
is_silt_20_40<- NULL
is_silt_40_60<- NULL
is_silt_60_80<- NULL
is_silt_80_100<- NULL
is_silt_100_150<- NULL
is_silt_150_200<- NULL
gc()

##### sand
is_sand_0_20<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D1_sand_Percentage.tif")
is_sand_20_40<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D2_sand_Percentage.tif")
is_sand_40_60<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D3_sand_Percentage.tif")
is_sand_60_80<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D4_sand_Percentage.tif")
is_sand_80_100<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D5_sand_Percentage.tif")
is_sand_100_100<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D5_sand_Percentage.tif")
is_sand_100_150<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D6_sand_Percentage.tif")
is_sand_150_200<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D7_sand_Percentage.tif")


r_is_sand_0_20<- crop(is_sand_0_20,exs)
hwsd2_sand_0_20<- raster::as.data.frame(r_is_sand_0_20,xy=TRUE)

r_is_sand_20_40<-crop(is_sand_20_40,exs)
hwsd2_sand_20_40<- raster::as.data.frame(r_is_sand_20_40,xy=TRUE)

r_is_sand_40_60<-crop(is_sand_40_60,exs)
hwsd2_sand_40_60<- raster::as.data.frame(r_is_sand_40_60,xy=TRUE)

r_is_sand_60_80<-crop(is_sand_60_80,exs)
hwsd2_sand_60_80<- raster::as.data.frame(r_is_sand_60_80,xy=TRUE)

r_is_sand_80_100<-crop(is_sand_80_100,exs)
hwsd2_sand_80_100<- raster::as.data.frame(r_is_sand_80_100,xy=TRUE)

r_is_sand_100_150<-crop(is_sand_100_150,exs)
hwsd2_sand_100_150<- raster::as.data.frame(r_is_sand_100_150,xy=TRUE)

r_is_sand_150_200<-crop(is_sand_150_200,exs)
hwsd2_sand_150_200<- raster::as.data.frame(r_is_sand_150_200,xy=TRUE)


hwsd2_sand <- hwsd2_sand_0_20
hwsd2_sand[,1]<- hwsd2_sand[,1]
hwsd2_sand[,2]<- hwsd2_sand[,2]
x<-NA
x2<-NA
x<-(as.data.frame(hwsd2_sand_0_20[,3]))*l1
x[,2]<- (hwsd2_sand_20_40[,3])*l2
x[,3]<- (hwsd2_sand_40_60[,3])*l3
x[,4]<- (hwsd2_sand_60_80[,3])*l4
x[,5]<- (hwsd2_sand_80_100[,3])*l5
x[,6]<- (hwsd2_sand_100_150[,3])*l6
x[,7]<- (hwsd2_sand_150_200[,3])*l7
x2 <- rowSums(x)
x2[x2<0]<-NA
hwsd2_sand[,3] <- x2

is_sand_0_20<- NULL
is_sand_20_40<- NULL
is_sand_40_60<- NULL
is_sand_60_80<- NULL
is_sand_80_100<- NULL
is_sand_100_150<- NULL
is_sand_150_200<- NULL
gc()


##### SOC
is_s_0_20<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D1_org_Carbon_stock_kg_m2.tif")
is_s_20_40<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D2_org_Carbon_stock_kg_m2.tif")
is_s_40_60<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D3_org_Carbon_stock_kg_m2.tif")
is_s_60_80<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D4_org_Carbon_stock_kg_m2.tif")
is_s_80_100<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D5_org_Carbon_stock_kg_m2.tif")
is_s_100_150<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D6_org_Carbon_stock_kg_m2.tif")
is_s_150_200<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D7_org_Carbon_stock_kg_m2.tif")

r_is_s_0_20<- crop(is_s_0_20,exs)
hwsd2_s_0_20<- raster::as.data.frame(r_is_s_0_20,xy=TRUE)

r_is_s_20_40<-crop(is_s_20_40,exs)
hwsd2_s_20_40<- raster::as.data.frame(r_is_s_20_40,xy=TRUE)

r_is_s_40_60<-crop(is_s_40_60, exs)
hwsd2_s_40_60<- raster::as.data.frame(r_is_s_40_60,xy=TRUE)

r_is_s_60_80<-crop(is_s_60_80, exs)
hwsd2_s_60_80<- raster::as.data.frame(r_is_s_60_80,xy=TRUE)

r_is_s_80_100<-crop(is_s_80_100, exs)
hwsd2_s_80_100<- raster::as.data.frame(r_is_s_80_100,xy=TRUE)

r_is_s_100_150<-crop(is_s_100_150, exs)
hwsd2_s_100_150<- raster::as.data.frame(r_is_s_100_150,xy=TRUE)

r_is_s_150_200<-crop(is_s_150_200, exs)
hwsd2_s_150_200<- raster::as.data.frame(r_is_s_150_200,xy=TRUE)

hwsd2_s <- hwsd2_s_0_20
hwsd2_s[,1]<- hwsd2_s[,1]
hwsd2_s[,2]<- hwsd2_s[,2]
x<-NA
x2<-NA
x<-(as.data.frame(hwsd2_s_0_20[,3]))*10*l1
x[,2]<- (hwsd2_s_20_40[,3])*10*l2
x[,3]<- (hwsd2_s_40_60[,3])*10*l3
x[,4]<- (hwsd2_s_60_80[,3])*10*l4
x[,5]<- (hwsd2_s_80_100[,3])*10*l5
x[,6]<- (hwsd2_s_100_150[,3])*10*l6
x[,7]<- (hwsd2_s_150_200[,3])*10*l7

x2 <- rowSums(x)
x2[x2<0]<-NA
hwsd2_s[,3] <- x2

is_s_0_20<- NULL
is_s_20_40<- NULL
is_s_40_60<- NULL
is_s_60_80<- NULL
is_s_80_100<- NULL
is_s_100_150<- NULL
is_s_150_200<- NULL
gc()

##### N
is_N_0_20<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D1_Nitrogen_Stock_kg_m2.tif")
is_N_20_40<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D2_Nitrogen_Stock_kg_m2.tif")
is_N_40_60<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D3_Nitrogen_Stock_kg_m2.tif")
is_N_60_80<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D4_Nitrogen_Stock_kg_m2.tif")
is_N_80_100<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D5_Nitrogen_Stock_kg_m2.tif")
is_N_100_150<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D6_Nitrogen_Stock_kg_m2.tif")
is_N_150_200<- rast("//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D7_Nitrogen_Stock_kg_m2.tif")

r_is_N_0_20<- crop(is_N_0_20,exs)
hwsd2_N_0_20<- raster::as.data.frame(r_is_N_0_20,xy=TRUE)

r_is_N_20_40<-crop(is_N_20_40,exs)
hwsd2_N_20_40<- raster::as.data.frame(r_is_N_20_40,xy=TRUE)

r_is_N_40_60<-crop(is_N_40_60, exs)
hwsd2_N_40_60<- raster::as.data.frame(r_is_N_40_60,xy=TRUE)

r_is_N_60_80<-crop(is_N_60_80, exs)
hwsd2_N_60_80<- raster::as.data.frame(r_is_N_60_80,xy=TRUE)

r_is_N_80_100<-crop(is_N_80_100, exs)
hwsd2_N_80_100<- raster::as.data.frame(r_is_N_80_100,xy=TRUE)

r_is_N_100_150<-crop(is_N_100_150, exs)
hwsd2_N_100_150<- raster::as.data.frame(r_is_N_100_150,xy=TRUE)

r_is_N_150_200<-crop(is_N_150_200, exs)
hwsd2_N_150_200<- raster::as.data.frame(r_is_N_150_200,xy=TRUE)

hwsd2_N <- hwsd2_N_0_20
hwsd2_N[,1]<- hwsd2_N[,1]
hwsd2_N[,2]<- hwsd2_N[,2]
x<-NA
x2<-NA
x<-(as.data.frame(hwsd2_N_0_20[,3]))*10*l1
x[,2]<- (hwsd2_N_20_40[,3])*10*l2
x[,3]<- (hwsd2_N_40_60[,3])*10*l3
x[,4]<- (hwsd2_N_60_80[,3])*10*l4
x[,5]<- (hwsd2_N_80_100[,3])*10*l5
x[,6]<- (hwsd2_N_100_150[,3])*10*l6
x[,7]<- (hwsd2_N_150_200[,3])*10*l7

x2 <- rowSums(x)
x2[x2<0]<-NA
hwsd2_N[,3] <- x2

is_N_0_20<- NULL
is_N_20_40<- NULL
is_N_40_60<- NULL
is_N_60_80<- NULL
is_N_80_100<- NULL
is_N_100_150<- NULL
is_N_150_200<- NULL
gc()

#### AWC ####
hs_0_100<-rast('//hdrive/home$/u141/nakhavali/HWSD_RASTERS/output_extended/D1_AWC_mm.tif')
r_0_100<- crop(hs_0_100,exs)
hwsd2_awc<- raster::as.data.frame(r_0_100,xy=TRUE)

hs_0_100<- NULL
gc()


xx<-NA
clay_ex<-NA
silt_ex<-NA
sand_ex<-NA
soc_ex<-NA
N_ex<-NA
AWC_ex<-NA

clay_raster <- rasterFromXYZ(hwsd2_clay)
crs(clay_raster) <- crs(is_s_0_20)

silt_raster <- rasterFromXYZ(hwsd2_silt)
crs(silt_raster) <- crs(is_s_0_20)

sand_raster <- rasterFromXYZ(hwsd2_sand)
crs(sand_raster) <- crs(is_s_0_20)

soc_raster <- rasterFromXYZ(hwsd2_s)
crs(soc_raster) <- crs(is_s_0_20)

N_raster <- rasterFromXYZ(hwsd2_N)
crs(N_raster) <- crs(is_s_0_20)

AWC_raster<- rasterFromXYZ(hwsd2_awc)
crs(AWC_raster) <- crs(is_s_0_20)

# Add coordinates as new columns to the dataframe
#coords <- st_coordinates(exs)
#exs$XLON <- coords[, "X"]
#exs$YLAT <- coords[, "Y"]


#exs <- exs #readOGR(dsn = "E:/Andre/climate_data_GWL2_boku/Raster_Points_with_LatLon.shp")
#crs(exs) <- crs(AWC_raster)
gc()

hwsd2_soil_data<- as.data.frame(exs$YLAT)


for (i in station[[scenario]]) {

    xx <- terra::extract(clay_raster, exs[i,], exact = TRUE, method = "simple")
    clay_ex[i] <- mean(as.numeric(na.omit(unlist(xx))))
    xx <- terra::extract(silt_raster, exs[i,], exact = TRUE, method = "simple")
    silt_ex[i] <- mean(as.numeric(na.omit(unlist(xx))))
    xx <- terra::extract(sand_raster, exs[i,], exact = TRUE, method = "simple")
    sand_ex[i] <- mean(as.numeric(na.omit(unlist(xx))))
    xx <- terra::extract(soc_raster, exs[i,], exact = TRUE, method = "simple")
    soc_ex[i] <- mean(as.numeric(na.omit(unlist(xx))))
    xx <- terra::extract(N_raster, exs[i,], exact = TRUE, method = "simple")
    N_ex[i] <- mean(as.numeric(na.omit(unlist(xx))))
    xx <- terra::extract(AWC_raster, exs[i,], exact = TRUE, method = "simple")
    AWC_ex[i] <- mean(as.numeric(na.omit(unlist(xx))))
  hwsd2_soil_data[i,1]<- exs$YLAT[i]
  hwsd2_soil_data[i,2] <- exs$XLON[i]
  hwsd2_soil_data[i,3] <- clay_ex[i]
  hwsd2_soil_data[i,4] <- silt_ex[i]
  hwsd2_soil_data[i,5] <- sand_ex[i]
  hwsd2_soil_data[i,6] <- soc_ex[i]
  hwsd2_soil_data[i,7] <- N_ex[i]
  hwsd2_soil_data[i,8] <- AWC_ex[i]
  if (is.na(sand_ex[i])){
  hwsd2_soil_data[i,6] <- NaN
  hwsd2_soil_data[i,7] <- NaN
  hwsd2_soil_data[i,8] <- NaN
   }
# print(hwsd2_soil_data[i,1:8])

}

colnames(hwsd2_soil_data) <- c("longitude","latitude","clay","silt","sand","soc","N","AWC")

write.table(hwsd2_soil_data,"output/nakhavali/HWSD2_BOKU/hwsd2_soil_data_BOKU.txt",sep=",",col.names = TRUE)

