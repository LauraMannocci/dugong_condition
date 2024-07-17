library(magrittr)
rm(list=ls())


############## preprocess seagrass from Allen Coral Atlas -----------------------------------------------------------------------------

#define empty list
list <- list()

#list all directories (1 direction = 1 region) in "allen_atlas" directory (read from hard disk, as the data is very voluminous)
full_dirs <- list.dirs(path = here::here("G:/envir_data_for_dugong/Allen_atlas_seagrass"), full.names = TRUE, recursive = FALSE)


#******loop on directories****** 

for (i in 1:length(full_dirs)) {
  
  if(i == 1){region_name = "Aldabra"}
  if(i == 2){region_name = "Andaman_sea"}
  if(i == 3){region_name = "Arafura_sea"}
  if(i == 4){region_name = "bali"}
  if(i == 5){region_name = "Bengladesh_Birmanie_coast"}
  if(i == 6){region_name = "Bismarck_sea"}
  if(i == 7){region_name = "Comores_Mayotte"}
  if(i == 8){region_name = "Flores_sea"}
  if(i == 9){region_name = "Java_sea"}
  if(i == 10){region_name = "Madagascar"}
  if(i == 11){region_name = "Malacca_strait"}
  if(i == 12){region_name = "Mozambique_coast"}
  if(i == 13){region_name = "North_East_Queensland"}
  if(i == 14){region_name = "Northern_Papouasie"}
  if(i == 15){region_name = "Oman_Yemen_coast"}
  if(i == 16){region_name = "Pakistan_India_coast"}
  if(i == 17){region_name = "Palau"}
  if(i == 18){region_name = "Palawan"}
  if(i == 19){region_name = "Persian_Gulf"}
  if(i == 20){region_name = "Philippines"}
  if(i == 21){region_name = "Queensland"}
  if(i == 22){region_name = "Red_sea"}
  if(i == 23){region_name = "Riau_Archipelago"}
  if(i == 24){region_name = "Salomon_islands"}
  if(i == 25){region_name = "Seychelles"}
  if(i == 26){region_name = "Somalia_coast"}
  if(i == 27){region_name = "South_India"}
  if(i == 28){region_name = "Southern_China"}
  if(i == 29){region_name = "Sri_Lanka_India"}
  if(i == 30){region_name = "Sulawesi"}
  if(i == 31){region_name = "Sumatra"}
  if(i == 32){region_name = "Thailand_gulf"}
  if(i == 33){region_name = "Timor_sea"}
  if(i == 34){region_name = "Vanuatu_New_Caledonia"}
  if(i == 35){region_name = "Vietnam"}
  if(i == 36){region_name = "West_Australia"}
  
  print("**********************************************************************")
  print(full_dirs[i])
  print(i)
  
  ### open geojson file for benthic map
  benthic_region <- geojsonio::geojson_read(paste0(full_dirs[i], "/Benthic-Map", "/benthic.geojson"),  what = "sp")
  #sp::plot(benthic_region)
  
  ### extract seagrass class
  benthic_region_seagrass <- benthic_region[benthic_region@data$class == "Seagrass", ]
  #sp::plot(benthic_region_seagrass)
  
  ### add data to the predefined empty list
  list[[i]] <- benthic_region_seagrass 
  
  ### convert to sf
  seagrass <- sf::st_as_sf(list[[i]]) 
  
  ### rename with region name
  assign(paste0("seagrass", "_", region_name), seagrass)
  
  ### save sf object to allen atlas folder
  save(list = paste0("seagrass", "_", region_name), 
       file = here::here("data", "envir_data", "allen_atlas_seagrass", paste0("seagrass", "_", region_name, ".RData")))
  
}
  
 




############## preprocess MPAs from world data on protected areas -----------------------------------------------------------------------------


#read protected area shapefiles from "mpa" directory (read from hard disk, as the data is very voluminous)
shp0 <- sf::st_read(dsn = "E:/envir_data_for_dugong/mpas/WDPA_Feb2024_Public_shp/WDPA_Feb2024_Public_shp_0/WDPA_Feb2024_Public_shp-polygons.shp")
shp1 <- sf::st_read(dsn = "E:/envir_data_for_dugong/mpas/WDPA_Feb2024_Public_shp/WDPA_Feb2024_Public_shp_1/WDPA_Feb2024_Public_shp-polygons.shp")
shp2 <- sf::st_read(dsn = "E:/envir_data_for_dugong/mpas/WDPA_Feb2024_Public_shp/WDPA_Feb2024_Public_shp_2/WDPA_Feb2024_Public_shp-polygons.shp")

#merge
shp <- rbind(shp0, shp1, shp2)

#extract all mpas
mpas_all <- shp %>% 
  dplyr::filter(MARINE != 0)
  
#plot
ggplot2::ggplot(mpas_all) +
  ggplot2::geom_sf(fill = "grey")

#extract entirely no take mpas
mpas_full_no_take <- mpas_all %>% 
  dplyr::filter(NO_TAKE == "All")

#extract partially no take mpas
mpas_partial_no_take <- mpas_all %>% 
  dplyr::filter(NO_TAKE == "Part")

### save sf objects to mpa folder
save(mpas_all, file = here::here("data", "envir_data", "mpas", "mpas_all.RData"))
save(mpas_full_no_take, file = here::here("data", "envir_data", "mpas", "mpas_full_no_take.RData"))
save(mpas_partial_no_take, file = here::here("data", "envir_data", "mpas", "mpas_partial_no_take.RData"))






############## preprocess turbidity data from Allen Coral Atlas -----------------------------------------------------------------------------

#define empty list
list <- list()

#list all directories (1 direction = 1 region) in "allen_atlas" directory (read from hard disk, as the data is very voluminous)
full_dirs <- list.dirs(path = here::here("E:/envir_data_for_dugong/Allen_atlas_turbidity"), full.names = TRUE, recursive = FALSE)

#to avoid disk space issues with terra::app() need to create a temp directory 
#it is important to keep this temp directory, otherwise the script process_environmental_data_turbidity wont work
terra::terraOptions(tempdir = "E:/envir_data_for_dugong/temp")

#******loop on directories****** 

for (i in 1:length(full_dirs)) {
  
  if(i == 1){region_name = "Aldabra"}
  if(i == 2){region_name = "Andaman_sea"}
  if(i == 3){region_name = "bali"}
  if(i == 4){region_name = "Comores_Mayotte"}
  if(i == 5){region_name = "Flores_sea"}
  if(i == 6){region_name = "Java_sea"}
  if(i == 7){region_name = "Malacca_strait"}
  if(i == 8){region_name = "Mozambique_coast"}
  if(i == 9){region_name = "North_East_Queensland"}
  if(i == 10){region_name = "Northern_Papouasie"}
  if(i == 11){region_name = "Palau"}
  if(i == 12){region_name = "Palawan"}
  if(i == 13){region_name = "Persian_Gulf"}
  if(i == 14){region_name = "Philippines"}
  if(i == 15){region_name = "Queensland"}
  if(i == 16){region_name = "Red_Sea"}
  if(i == 17){region_name = "Riau_Archipelago"}
  if(i == 18){region_name = "Sri_Lanka_India"}
  if(i == 19){region_name = "Sulawesi"}
  if(i == 20){region_name = "Timor_sea"}
  if(i == 21){region_name = "Vanuatu_New_Caledonia"}
  if(i == 22){region_name = "West_Australia"}

  
  print("**********************************************************************")
  print(full_dirs[i])
  print(i)
  
  ### open raster files for turbidity per year
  turbidity_region_2019 <- raster::raster(paste0(full_dirs[i], "/Turbidity-", 2019, "/turbidity-annual_0.tif"),  what = "sp")
  turbidity_region_2020 <- raster::raster(paste0(full_dirs[i], "/Turbidity-", 2020, "/turbidity-annual_0.tif"),  what = "sp")
  turbidity_region_2021 <- raster::raster(paste0(full_dirs[i], "/Turbidity-", 2021, "/turbidity-annual_0.tif"),  what = "sp")
  turbidity_region_2022 <- raster::raster(paste0(full_dirs[i], "/Turbidity-", 2022, "/turbidity-annual_0.tif"),  what = "sp")
  turbidity_region_2023 <- raster::raster(paste0(full_dirs[i], "/Turbidity-", 2023, "/turbidity-annual_0.tif"),  what = "sp")
  
  ### make stack
  turbidity_region <- raster::stack(turbidity_region_2019, turbidity_region_2020, turbidity_region_2021, turbidity_region_2022, turbidity_region_2023)
  
  ### mean of turbidity across years
  turbidity_region_terra = terra::rast(turbidity_region) #need to transform to terra object first
  turbidity_region_mean_terra = terra::app(turbidity_region_terra, fun = mean) #much faster than raster::mean()

  ### make final stack
  turbidity_region_final <- raster::stack(turbidity_region_2019, turbidity_region_2020, turbidity_region_2021, turbidity_region_2022, turbidity_region_2023, raster::raster(turbidity_region_mean_terra))
  names(turbidity_region_final) <- c("turbidity_2019", "turbidity_2020", "turbidity_2021", "turbidity_2022", "turbidity_2023", "turbidity_region_2019_2023")
  
  ### add data to the predefined empty list
  list[[i]] <- turbidity_region_final 
  
  ### rename with region name
  assign(paste0("turbidity_", region_name), turbidity_region_final)
  
  ### save sf object to allen atlas folder
  save(list = paste0("turbidity_", region_name),  
       file = here::here("data", "envir_data", "allen_atlas_turbidity", paste0("turbidity_", region_name, ".RData")))
  
}






