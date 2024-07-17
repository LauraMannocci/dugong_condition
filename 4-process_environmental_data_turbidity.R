rm(list=ls())
library(magrittr)



#data we want to extract : mean turbidity in a 26km buffer


#---------------------------------------------- load dugong observation data ----------------------------------------------------------------------

load("dat_final.RData")


#Create new column year
dat_final <- dat_final %>%
  dplyr::mutate(year = dplyr::case_when(!is.na(collection_year) ~ collection_year,
                                        TRUE ~ publication_year))




#---------------------------------------------- load loop parameters ----------------------------------------------------------------------

#read ods file countaining region parameters
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
  
  #---------------------------------------------- prepare dugong observation data ----------------------------------------------------------------------
  

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
  
  
  
  
  #----------------------------------------------------- load and prepare turbidity data -----------------------------------------------------------
  
  
  #load sf object for region (derived from script 3-preprocess_environmental_data)
  load(here::here(paste0("data/envir_data/allen_atlas_turbidity/", "turbidity_", region_rdata, ".RData")))
  turbidity <- get(paste0("turbidity_", region_rdata))

  

  
  
  #----------------------------------------------------- associate turbidity data to dugong observation data -----------------------------------------------------------
  
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
    
    #Select year in dat_final 
    selected_year <- dat_indiv$year
    
    print(paste("year of observation:", selected_year))
    
    #Select raster corresponding to year, taking the mean of all years if no turbidity data is available for the given year
    if (selected_year %in% c("2016", "2017", "2018", "2024")){
      print("no available turbidity - taking average turbidity of all years")
      selected_raster <- turbidity[[paste0("turbidity_region_2019_2023")]]
    }else{
      selected_raster <- turbidity[[paste0("turbidity_", selected_year)]]
    }
    
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
    
    #Extract sst values in buffer
    print("extract turbidity values for given year")
    turbidity_values <- raster::extract(selected_raster, buffer)
    print(proc.time() - ptm)

    #calculate turbidity_in_buffer 
    print("calculating turbidity")
    all_values <- unlist(turbidity_values) 
    turbidity_in_buffer_mean <- mean(all_values, na.rm = TRUE)
    turbidity_in_buffer_sd <- sd(all_values, na.rm = TRUE)
    
    #add columns
    dat_indiv$mean_turbidity <- turbidity_in_buffer_mean
    dat_indiv$sd_turbidity <- turbidity_in_buffer_sd
    
    #store data set for individual i in list
    dat_list[[i]] <- dat_indiv
    
  }#end loop individuals
  
  
  
  #------ Bind list of individuals by row
  
  dat_final_region <- dplyr::bind_rows(dat_list) 
  assign(paste0("dat_final_", region_rdata2), dat_final_region)
  
  
  
}#end loop regions


#----------------------------------------------------- bind all regional datasets -----------------------------------------------------------


dat_final_turbidity = rbind(dat_final_Mozambique_coast, dat_final_Comores_Mayotte, dat_final_Sri_Lanka_India, dat_final_Andaman_sea,
                           dat_final_Malacca_strait, dat_final_Vanuatu_New_Caledonia1, dat_final_Vanuatu_New_Caledonia2,
                           dat_final_North_East_Queensland1, dat_final_North_East_Queensland2, dat_final_Queensland, dat_final_Timor_sea, 
                           dat_final_West_Australia, dat_final_Palau, dat_final_bali, dat_final_Java_sea, dat_final_Palawan, dat_final_Philippines1,
                           dat_final_Philippines2, dat_final_Flores_sea, dat_final_Sulawesi, dat_final_Northern_Papouasie, dat_final_Aldabra, dat_final_Persian_Gulf, 
                           dat_final_Red_Sea, dat_final_Riau_Archipelago)

save(dat_final_turbidity, file = "dat_final_turbidity.RData")

summary(dat_final_turbidity$mean_turbidity) #4 NAs







