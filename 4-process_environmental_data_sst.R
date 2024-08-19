rm(list=ls())
library(magrittr)




#***IMPORTANT*** because of the absence of geographic coordinates in the original data some of the below analyses will no longer be reproducible



#---------------------------------------------- load dugong observation data ----------------------------------------------------------------------

load("dat_final.RData")


#Create new columns month and year
dat_final <- dat_final %>%
  dplyr::mutate(month = dplyr::case_when(!is.na(collection_month) ~ collection_month,
                                         TRUE ~ publication_month)) %>% 
  dplyr::mutate(year = dplyr::case_when(!is.na(collection_year) ~ collection_year,
                                         TRUE ~ publication_year))




#---------------------------------------------- read all sst rasters ----------------------------------------------------------------------
#read raster object countaining sst 

directory <- here::here("data/envir_data/copernicus_sst")

file_list <- list.files(directory, pattern = "\\.nc$", full.names = TRUE)

for (file in file_list) {
  
  #Extract month and year
  filename <- basename(file)
  year_sst <- substr(filename, 1, 4)
  month_sst <- substr(filename, 5, 6)
  
  #Import file nc in raster object
  assign(paste0("sst_", month_sst, "_", year_sst), raster::raster(file))

}




#----------------------------------------------------- calculate sst mean by month across all years --------------------------------------------------------------

months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

for (m in months) {
  
  print(paste("--------------------doing mean of month", m))
  
  list_rasters_month <- ls(pattern = paste0("sst_", m))
  
  stack <- raster::stack(get(list_rasters_month[1]), get(list_rasters_month[2]), get(list_rasters_month[3]),
                         get(list_rasters_month[4]), get(list_rasters_month[5]), get(list_rasters_month[6]), get(list_rasters_month[7]))
  
  mean_stack <- raster::mean(stack)
  
  names(mean_stack) <- paste0("sst_", m)
  
  assign(paste0("sst_mean_", m), mean_stack)
  
}




#----------------------------------------------------- associate sst data to dugong observation data -----------------------------------------------------------

dat_list <- list()

#----- loop on individuals  
for (i in 1:nrow(dat_final)) {
  
  print("**********************************************************************")
  print(paste("indiv number:", i))
  ptm <- proc.time()
  print(ptm)
  
  #Select i row (individual)
  dat_indiv <- dat_final[i,]
  
  #Select month and year in dat_final 
  selected_month <- dat_indiv$month
  selected_year <- dat_indiv$year
  
  print(paste("month of observation:", selected_month))
  print(paste("year of observation:", selected_year))
  
  #Select raster corresponding to month and year, taking the mean of all years if no sst data is available for the given year 
  #*****here make code to get mean monthly raster across all years if obs year is 2023 or 2024
  if (selected_year %in% c("2023", "2024")){
    print("no available sst - taking average sst of all years")
    selected_raster <- get(paste0("sst_mean_", selected_month)) 
  }else{
    selected_raster <- get(paste0("sst_", selected_month, "_", selected_year)) 
  }
  
  #Convert individual to sf object
  dat_indiv_sf <- sf::st_as_sf(dat_indiv, coords = c("approx_longitude", "approx_latitude"), crs = 4326)
   
  # #Transform to raster projection
  # dat_indiv_sf <- sf::st_transform(dat_indiv_sf, crs = sf::st_crs(selected_raster))
  
  # Create buffer around individual point
  buffer <- sf::st_buffer(dat_indiv_sf, dist = 26000)  #26000 m or 26 km from Marsh et al. 2022
  
  #Extract sst values in buffer
  print("extract sst values for given month and year")
  ptm <- proc.time()
  sst_values <- raster::extract(selected_raster, buffer)
  print(ptm)
  
  #calculate sst_in_buffer in Celcius
  print("calculating sst")
  all_values_kelvin <- unlist(sst_values) 
  all_values_celcius <- unlist(sst_values) - 273.15
  sst_in_buffer_mean_celcius <- mean(all_values_celcius, na.rm = TRUE)
  sst_in_buffer_sd_celcius <- sd(all_values_celcius, na.rm = TRUE)
  
  
  #add columns
  dat_indiv$mean_sst_celsius <-sst_in_buffer_mean_celcius
  dat_indiv$sd_sst_celsius <- sst_in_buffer_sd_celcius
  
  #store data set individual i in list
  dat_list[[i]] <- dat_indiv
  
}#end loop individuals




#------ Bind list elements by row
dat_final_sst<- dplyr::bind_rows(dat_list) 

save(dat_final_sst, file = "dat_final_sst.RData")




