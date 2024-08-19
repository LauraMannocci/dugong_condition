rm(list=ls())
library(magrittr)




#***IMPORTANT*** because of the absence of geographic coordinates in the original data some of the below analyses will no longer be reproducible


#---------------------------------------------- load dugong observation data ----------------------------------------------------------------------

load("dat_final.RData")








#---------------------------------------------- load gravity file  ----------------------------------------------------------------------
directory <- here::here("data/envir_data/gravity")

dat_gravity <- raster::raster("data/envir_data/gravity/rastGravity.tif")

#New projection CRS (WGS84)
projection_crs <- sp::CRS("+proj=longlat +datum=WGS84")

#Project raster in new crs
dat_gravity_crs <- raster::projectRaster(dat_gravity, crs = projection_crs)

plot(dat_gravity_crs)
print(dat_gravity_crs)

#Remane raster object names to gravity
names(dat_gravity_crs) <- "gravity"





#---------------------------------------------- associate gravity data to dugong observation data  ----------------------------------------------------------------------

dat_list <- list()

#----- loop on individuals  
for (i in 1:nrow(dat_final)) {
  
  print("**********************************************************************")
  print(paste("indiv number:", i))
  ptm <- proc.time()
  print(ptm)
  
  #Select i row (individual)
  dat_indiv <- dat_final [i,]
  
  #Convert individual to sf object
  dat_indiv_sf <- sf::st_as_sf(dat_indiv, coords = c("approx_longitude", "approx_latitude"), crs = 4326)
 
  #Create buffer around individual point
  buffer <- sf::st_buffer(dat_indiv_sf, dist = 26000)  #26000 m or 26 km from Marsh et al. 2022
  
  #Extract gravity values in buffer
  print("extract gravity values")
  ptm <- proc.time()
  gravity_values <- raster::extract(dat_gravity_crs, buffer)
  print(ptm)
  
  #calculate gravity_in_buffer 
  print("calculating gravity")
  all_values <- unlist(gravity_values) 
  gravity_in_buffer_mean <- mean(all_values, na.rm = TRUE)
  gravity_in_buffer_sd <- sd(all_values, na.rm = TRUE)
  

  #add columns
  dat_indiv$mean_gravity <-gravity_in_buffer_mean
  dat_indiv$sd_gravity <-gravity_in_buffer_sd

  
  #store data set individual i in list
  dat_list[[i]] <- dat_indiv
  
}#end loop individuals



#------ Bind list elements by row
dat_final_gravity <- dplyr::bind_rows(dat_list) 

save(dat_final_gravity, file = "dat_final_gravity.RData")



  
  
  
  
