rm(list=ls())
library(magrittr)
library(doParallel)


#---------------------------------------------- load loop parameters ----------------------------------------------------------------------

#read osd file countaining region parameters
dat <- readODS::read_ods(here::here("data", "envir_data", "region_rdata.ods"))
all_region_obs = dat$region_obs

#---------------------------------------------- loop on regions ----------------------------------------------------------------------

for (r in 1:length(all_region_obs)){
  
  print("######################################################################")
  print(paste("region:", all_region_obs[r]))
  
  # select regions
  region_obs = dat$region_obs[r]
  region_rdata =  dat$region_rdata[r]
  region_rdata2 =  dat$region_rdata2[r] #for renaming data after loop
  
  #box coord
  lat1 = dat$lat1[r]
  lat2 = dat$lat2[r]
  lon1 = dat$lon1[r]
  lon2 = dat$lon2[r]
  
  #map box
  # world <- rnaturalearth::ne_countries(scale = 'medium',  type = "countries", returnclass = 'sf')
  
  # ggplot2::ggplot() + 
  #   ggplot2::geom_sf(data = world, fill = "grey50") + 
  #   ggplot2::coord_sf(xlim = c(lon1, lon2), ylim = c(lat1, lat2)) 

  
  #---------------------------------------------- load and prepare dugong observation data ----------------------------------------------------------------------
  
  
  load("dat_final.RData")
  
  #filter admin province
  if (! region_obs %in% c("south davao,palawan province","palawan province")){
    dat_final_region <- dat_final %>%
      dplyr::filter(province_admin  %in%  unlist(strsplit(region_obs, ",")))
  }
  
  #filter admin province and most precise location
  if (region_obs %in% c("south davao,palawan province","palawan province")){
    print("south davao,palawan province or palawan province")
    region_most_precise_location =  dat$most_precise_location[r]
    dat_final_region <- dat_final %>%
      dplyr::filter(province_admin  %in% unlist(strsplit(region_obs, ","))) %>% 
      dplyr::filter(most_precise_location %in% unlist(strsplit(region_most_precise_location, ",")))
  }

  
  
  
  #----------------------------------------------------- load and prepare seagrass data -----------------------------------------------------------
  
  
  #load sf object for region (derived from script 3-prepare_environmental_data)
  load(here::here(paste0("data/envir_data/allen_atlas_seagrass/", "seagrass_", region_rdata, ".RData")))
  seagrass <- get(paste0("seagrass_", region_rdata))
  
  #crop seagrass to box
  box = c(xmin = lon1, ymin = lat1, xmax = lon2, ymax = lat2)
  seagrass_crop <- sf::st_crop(seagrass, box)
  sf::st_crs(seagrass_crop) <- 4326 #4326 corresponds to wgs84 (trick required in Ubuntu - the warning can be ignored)
  
  
  #check map
  # ggplot2::ggplot(seagrass_crop) +
  #   ggplot2::geom_sf(fill = "grey50")
  
  
  
  #----------------------------------------------------- associate seagrass data to dugong observation data -----------------------------------------------------------
  
  dat_list <- list()
  
  #----- loop on individuals  
  
  print(paste("number of individuals:", nrow(dat_final_region)))
  
  for (i in 1:nrow(dat_final_region)) {
    
    print("**********************************************************************")
    print(paste("indiv number:", i))
    ptm <- proc.time()
    print(ptm)
    
    #select i row (individual)
    dat_indiv <- dat_final_region[i,]
    
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
    
    #Intersect buffer with seagrass
    print("doing intersection")
    
    #set up parallel processing
    cl <- parallel::makeCluster(parallel::detectCores()-1)
    doParallel::registerDoParallel(cl)
    
    #intersect with parallel processing to speed up calculation
  
    pols = foreach::foreach(j = 1:nrow(seagrass_crop)) %dopar% {
      intersect <- sf::st_intersection(buffer, seagrass_crop[j,])
      intersect
    }
    
    print(proc.time() - ptm)
    parallel::stopCluster(cl)
    
    #Bind list of polygons by row to obtain sf object
    print("binding polygons")
    pols_bind <- dplyr::bind_rows(pols) 

    #calculate seagrass variables
    print("calculating seagrass variables")
    #calculate total seagrass area in km2
    seagrass_area_in_buffer <- sf::st_area(pols_bind)  #in m²
    seagrass_area_in_buffer_sum <- sum(seagrass_area_in_buffer)
    seagrass_area_in_buffer_sum_km2 <- seagrass_area_in_buffer_sum / 1e6 #convert to km2
    
    #calculate mean seagrass patch area in km2
    seagrass_patch_area_in_buffer_mean <- mean(seagrass_area_in_buffer)
    seagrass_patch_area_in_buffer_mean_km2 <- seagrass_patch_area_in_buffer_mean / 1e6  #convert to km2
    
    #calculate number of seagrass patches
    seagrass_patch_number_in_buffer <- length(seagrass_area_in_buffer)
    
    #calculate surface of buffer
    buffer_area <- sf::st_area(buffer)
    buffer_area_km2 <- buffer_area / 1e6 #convert to km2
    
    #calculate percent of seagrass surface area in buffer
    percent_of_seagrass_in_buffer <- (seagrass_area_in_buffer_sum_km2 /  buffer_area_km2) * 100
    
    #add columns to dugong observation data
    dat_indiv$sum_seagrass_area_km2 <- as.numeric(seagrass_area_in_buffer_sum_km2)
    dat_indiv$mean_seagrass_patch_area_km2 <- as.numeric(seagrass_patch_area_in_buffer_mean_km2)
    dat_indiv$seagrass_patch_number <- as.numeric(seagrass_patch_number_in_buffer)
    dat_indiv$buffer_area_km2 <- as.numeric(buffer_area_km2)
    dat_indiv$percent_seagrass <- as.numeric(percent_of_seagrass_in_buffer)
    
    #store data set for individual i in list
    dat_list[[i]] <- dat_indiv
    
  }#end loop individuals
  
  
  #------ Bind list of individuals by row
  
  dat_final_region <- dplyr::bind_rows(dat_list) 
  assign(paste0("dat_final_", region_rdata2), dat_final_region)
  
  
  #check map
  # ggplot2::ggplot() + 
  #   ggplot2::geom_sf(data = seagrass_crop, col = "gray40") + 
  #   ggplot2::geom_sf(data = centroid, color = "red") +
  #   ggplot2::geom_sf(data = buffer, fill = "greenyellow", alpha = 0.2)

}#end loop regions



#----------------------------------------------------- bind all regional datasets -----------------------------------------------------------

dat_final_seagrass = rbind(dat_final_Mozambique_coast, dat_final_Comores_Mayotte, dat_final_Sri_Lanka_India, dat_final_Andaman_sea,
                           dat_final_Malacca_strait, dat_final_Vanuatu_New_Caledonia1, dat_final_Vanuatu_New_Caledonia2,
                           dat_final_North_East_Queensland1, dat_final_North_East_Queensland2, dat_final_Queensland, dat_final_Timor_sea, 
                           dat_final_West_Australia, dat_final_Palau, dat_final_bali, dat_final_Java_sea, dat_final_Palawan, dat_final_Philippines1,
                           dat_final_Philippines2, dat_final_Flores_sea, dat_final_Sulawesi, dat_final_Northern_Papouasie, dat_final_Aldabra, 
                           dat_final_Persian_Gulf, dat_final_Red_sea, dat_final_Riau_Archipelago)

save(dat_final_seagrass, file = "dat_final_seagrass.RData")

summary(dat_final_seagrass)
