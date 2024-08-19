rm(list=ls())
library(magrittr)
library(doParallel)



#***IMPORTANT*** because of the absence of geographic coordinates in the original data some of the below analyses will no longer be reproducible



#---------------------------------------------- load and prepare dugong observation data ----------------------------------------------------------------------

load("dat_final.RData")


#----------------------------------------------------- load and prepare mpas_all data -----------------------------------------------------------

load(here::here("data/envir_data/mpas/mpas_all.RData"))


dat_list <- list()

#----- loop on individuals  

print(paste("number of individuals:", nrow(dat_final)))

for (i in 1:nrow(dat_final)) {
  
  print("**********************************************************************")
  print(paste("indiv number:", i))
  ptm <- proc.time()
  print(ptm)
  
  #select i row (individual)
  dat_indiv <- dat_final[i,]
  
  #Extract longitude latitude 
  lat_lon <- dat_indiv[, c("approx_longitude", "approx_latitude")] 
  
  #Convert to sf object
  point_sf <- sf::st_as_sf(lat_lon, coords = c("approx_longitude", "approx_latitude"), crs = 4326) #4326 corresponds to wgs84
  
  #the coordinate reference should be the same for both objects in the intersection below
  #Get buffer around points
  print("getting buffer")
  buffer <- sf::st_buffer(point_sf, dist = 26000)  #26000 m or 26 km from Marsh et al. 2022
  print("getting centroid")
  centroid <- sf::st_centroid(buffer) #for visualization 
  
  #Intersect buffer with mpas
  print("doing intersection")
  
  #set up parallel processing
  cl <- parallel::makeCluster(parallel::detectCores()-1)
  doParallel::registerDoParallel(cl)
  
  #intersect with parallel processing to speed up calculation
  
  pols = foreach::foreach(j = 1:nrow(mpas_all)) %dopar% {
    if (sf::st_is_valid(mpas_all[j,]) == TRUE) {
      intersect <- sf::st_intersection(buffer, mpas_all[j,])
    }
    intersect
  }
  
  print(proc.time() - ptm)
  parallel::stopCluster(cl)
  
  #Bind list of polygons by row to obtain sf object
  print("binding polygons")
  pols_bind <- dplyr::bind_rows(pols) 
  
  #calculate mpas variables
  print("calculating mpas variables")
  #calculate total mpas area in km2
  mpas_all_area_in_buffer <- sf::st_area(pols_bind)  #in m²
  mpas_all_area_in_buffer_sum <- sum(mpas_all_area_in_buffer)
  mpas_all_area_in_buffer_sum_km2 <- mpas_all_area_in_buffer_sum / 1e6 #convert to km2
  
  #calculate surface of buffer
  buffer_area <- sf::st_area(buffer)
  buffer_area_km2 <- buffer_area / 1e6 #convert to km2
  
  #calculate percent of mpas surface area in buffer
  percent_of_mpas_all_in_buffer <- (mpas_all_area_in_buffer_sum_km2 /  buffer_area_km2) * 100
  
  #add columns to dugong observation data
  dat_indiv$sum_mpas_all_area_km2 <- as.numeric(mpas_all_area_in_buffer_sum_km2)
  dat_indiv$buffer_area_km2 <- as.numeric(buffer_area_km2)
  dat_indiv$percent_mpas_all <- as.numeric(percent_of_mpas_all_in_buffer)
  
  #store data set for individual i in list
  dat_list[[i]] <- dat_indiv
  
}#end loop individuals


#------ Bind list elements by row
dat_final_mpas_all<- dplyr::bind_rows(dat_list) 

save(dat_final_mpas_all, file = "dat_final_mpas_all.RData")




#----------------------------------------------------- load and prepare mpas_full_no_take data -----------------------------------------------------------

load(here::here("data/envir_data/mpas/mpas_full_no_take.RData"))


dat_list <- list()

dat_detail_mpas_full_no_take <- data.frame()

#----- loop on individuals  

print(paste("number of individuals:", nrow(dat_final)))

for (i in 1:nrow(dat_final)) {
  
  print("**********************************************************************")
  print(paste("indiv number:", i))
  ptm <- proc.time()
  print(ptm)
  
  #select i row (individual)
  dat_indiv <- dat_final[i,]
  
  #Extract longitude latitude 
  lat_lon <- dat_indiv[, c("approx_longitude", "approx_latitude")] 
  
  #Convert to sf object
  point_sf <- sf::st_as_sf(lat_lon, coords = c("approx_longitude", "approx_latitude"), crs = 4326) #4326 corresponds to wgs84
  
  #the coordinate reference should be the same for both objects in the intersection below
  #Get buffer around points
  print("getting buffer")
  buffer <- sf::st_buffer(point_sf, dist = 26000)  #26000 m or 26 km from Marsh et al. 2022
  print("getting centroid")
  centroid <- sf::st_centroid(buffer) #for visualization 
  
  #Intersect buffer with mpas
  print("doing intersection")
  
  #set up parallel processing
  cl <- parallel::makeCluster(parallel::detectCores()-1)
  doParallel::registerDoParallel(cl)
  
  #intersect with parallel processing to speed up calculation
  
  pols = foreach::foreach(j = 1:nrow(mpas_full_no_take)) %dopar% {
    if (sf::st_is_valid(mpas_full_no_take[j,]) == TRUE) {
      intersect <- sf::st_intersection(buffer, mpas_full_no_take[j,])
    }
    intersect
  }
  
  print(proc.time() - ptm)
  parallel::stopCluster(cl)
  
  #Bind list of polygons by row to obtain sf object
  print("binding polygons")
  pols_bind <- dplyr::bind_rows(pols) 
  
  #calculate mpas variables
  print("calculating mpas variables")
  #calculate total mpas area in km2
  mpas_full_no_take_area_in_buffer <- sf::st_area(pols_bind)  #in m²
  mpas_full_no_take_area_in_buffer_sum <- sum(mpas_full_no_take_area_in_buffer)
  mpas_full_no_take_area_in_buffer_sum_km2 <- mpas_full_no_take_area_in_buffer_sum / 1e6 #convert to km2
  
  #calculate surface of buffer
  buffer_area <- sf::st_area(buffer)
  buffer_area_km2 <- buffer_area / 1e6 #convert to km2
  
  #calculate percent of mpas surface area in buffer
  percent_of_mpas_full_no_take_in_buffer <- (mpas_full_no_take_area_in_buffer_sum_km2 /  buffer_area_km2) * 100
  
  #add columns to dugong observation data
  dat_indiv$sum_mpas_full_no_take_area_km2 <- as.numeric(mpas_full_no_take_area_in_buffer_sum_km2)
  dat_indiv$buffer_area_km2 <- as.numeric(buffer_area_km2)
  dat_indiv$percent_mpas_full_no_take <- as.numeric(percent_of_mpas_full_no_take_in_buffer)
  
  #store data set for individual i in list
  dat_list[[i]] <- dat_indiv
  
  #store MPAs details (polygons) in data frame
  dat_detail_mpas_full_no_take <- rbind(dat_detail_mpas_full_no_take, pols_bind)
  
}#end loop individuals


#------ Bind list elements by row

dat_final_mpas_full_no_take <- dplyr::bind_rows(dat_list) 
save(dat_final_mpas_full_no_take, file = "dat_final_mpas_full_no_take.RData")

dat_detail_mpas_full_no_take = unique(dat_detail_mpas_full_no_take)
save(dat_detail_mpas_full_no_take, file = "dat_detail_mpas_full_no_take.RData")








#----------------------------------------------------- load and prepare mpas_partial_no_take data -----------------------------------------------------------

load(here::here("data/envir_data/mpas/mpas_partial_no_take.RData"))


dat_list <- list()

dat_detail_mpas_partial_no_take <- data.frame()

#----- loop on individuals  

print(paste("number of individuals:", nrow(dat_final)))

for (i in 1:nrow(dat_final)) {
  
  print("**********************************************************************")
  print(paste("indiv number:", i))
  ptm <- proc.time()
  print(ptm)
  
  #select i row (individual)
  dat_indiv <- dat_final[i,]
  
  #Extract longitude latitude 
  lat_lon <- dat_indiv[, c("approx_longitude", "approx_latitude")] 
  
  #Convert to sf object
  point_sf <- sf::st_as_sf(lat_lon, coords = c("approx_longitude", "approx_latitude"), crs = 4326) #4326 corresponds to wgs84
  
  #the coordinate reference should be the same for both objects in the intersection below
  #Get buffer around points
  print("getting buffer")
  buffer <- sf::st_buffer(point_sf, dist = 26000)  #26000 m or 26 km from Marsh et al. 2022
  print("getting centroid")
  centroid <- sf::st_centroid(buffer) #for visualization 
  
  #Intersect buffer with mpas
  print("doing intersection")
  
  #set up parallel processing
  cl <- parallel::makeCluster(parallel::detectCores()-1)
  doParallel::registerDoParallel(cl)
  
  #intersect with parallel processing to speed up calculation
  
  pols = foreach::foreach(j = 1:nrow(mpas_partial_no_take)) %dopar% {
    if (sf::st_is_valid(mpas_partial_no_take[j,]) == TRUE) {
      intersect <- sf::st_intersection(buffer, mpas_partial_no_take[j,])
    }
    intersect
  }
  
  print(proc.time() - ptm)
  parallel::stopCluster(cl)
  
  #Bind list of polygons by row to obtain sf object
  print("binding polygons")
  pols_bind <- dplyr::bind_rows(pols) 
  
  #calculate mpas variables
  print("calculating mpas variables")
  #calculate total mpas area in km2
  mpas_partial_no_take_area_in_buffer <- sf::st_area(pols_bind)  #in m²
  mpas_partial_no_take_area_in_buffer_sum <- sum(mpas_partial_no_take_area_in_buffer)
  mpas_partial_no_take_area_in_buffer_sum_km2 <- mpas_partial_no_take_area_in_buffer_sum / 1e6 #convert to km2
  
  #calculate surface of buffer
  buffer_area <- sf::st_area(buffer)
  buffer_area_km2 <- buffer_area / 1e6 #convert to km2
  
  #calculate percent of mpas surface area in buffer
  percent_of_mpas_partial_no_take_in_buffer <- (mpas_partial_no_take_area_in_buffer_sum_km2 /  buffer_area_km2) * 100
  
  #add columns to dugong observation data
  dat_indiv$sum_mpas_partial_no_take_area_km2 <- as.numeric(mpas_partial_no_take_area_in_buffer_sum_km2)
  dat_indiv$buffer_area_km2 <- as.numeric(buffer_area_km2)
  dat_indiv$percent_mpas_partial_no_take <- as.numeric(percent_of_mpas_partial_no_take_in_buffer)
  
  #store data set for individual i in list
  dat_list[[i]] <- dat_indiv
  
  #store MPAs details (polygons) in data frame
  dat_detail_mpas_partial_no_take <- rbind(dat_detail_mpas_partial_no_take, pols_bind)
  
}#end loop individuals


#------ Bind list elements by row

dat_final_mpas_partial_no_take <- dplyr::bind_rows(dat_list) 
save(dat_final_mpas_partial_no_take, file = "dat_final_mpas_partial_no_take.RData")

dat_detail_mpas_partial_no_take = unique(dat_detail_mpas_partial_no_take)
save(dat_detail_mpas_partial_no_take, file = "dat_detail_mpas_partial_no_take.RData")



