rm(list = ls())


#---------------------------------------- MPAs full no take ------------------------------------


# Read RData of MPA details 
load(here::here("dat_detail_mpas_full_no_take.RData"))



# Make quick map
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot2::ggplot(data = world) +
  ggplot2::geom_sf() +
  ggplot2::geom_sf(data = dat_detail_mpas_full_no_take, fill = "red") +
  ggplot2::coord_sf(xlim=c(35,166), ylim = c(27, -40)) # limit to Indo-Pacific
#NB: MPAs too small to be seen on Indo-Pacific mpa



# Data cleaning
dat <- sf::st_drop_geometry(dat_detail_mpas_full_no_take) #drop geometry to make data lighter
dim(dat) 
dat <- unique(dat) #remove remaining duplicates
dim(dat) 


# Do quick exploration
table(dat$NO_TAKE) #check that all MPAs are no take
dat$NAME #all MPA names
table(dat$IUCN_CAT) #IUCN categories
table(dat$DESIG_ENG) #designations
table(dat$STATUS) #status 
table(dat$STATUS_YR) #status year
table(dat$OWN_TYPE) #governance type
table(dat$ISO3) # countries - ARE United Arab Emirates - AUS Asutralia - MYS Malaysia - NCL New Caledonia - PLW: Palao - SYC Seychelles - THA Thailand
mean(dat$GIS_AREA) #mean area of MPAs : 5882.175 km2
range(dat$GIS_AREA) #range area of MPAs : 3.159961e-04 1.953064e+05 km2
mean(dat$NO_TK_AREA) #mean area of MPAs : 5966.795 km2 (slightly different from GIS_AREA ?)
range(dat$NO_TK_AREA) #range area of MPAs :   0.0 201235.8 km2 (slightly different from GIS_AREA ?)





#---------------------------------------- MPAs partial no take ------------------------------------

# Read RData of MPA details 
load(here::here("dat_detail_mpas_partial_no_take.RData"))



# Make quick map
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot2::ggplot(data = world) +
  ggplot2::geom_sf() +
  ggplot2::geom_sf(data = dat_detail_mpas_partial_no_take, fill = "red") +
  ggplot2::coord_sf(xlim=c(35,166), ylim = c(27, -40)) # limit to Indo-Pacific
#NB: MPAs too small to be seen on Indo-Pacific mpa



# Data cleaning
dat <- sf::st_drop_geometry(dat_detail_mpas_partial_no_take) #drop geometry to make data lighter
dim(dat) 
dat <- unique(dat) #remove remaining duplicates
dim(dat) 


# Do quick exploration
table(dat$NO_TAKE) #check that all MPAs are partly no take
dat$NAME #all MPA names
table(dat$IUCN_CAT) #IUCN categories
table(dat$DESIG_ENG) #designations
table(dat$STATUS) #status 
table(dat$STATUS_YR) #status year
table(dat$OWN_TYPE) #governance type
table(dat$ISO3) # countries - ARE United Arab Emirates - AUS Asutralia - NCL New Caledonia - PLW: Palao - IDN Indonesia - TLS Timor Leste
mean(dat$GIS_AREA) #mean area of MPAs : 83181.82 km2
range(dat$GIS_AREA) #range area of MPAs : 4.487459e-01 1.291643e+06 km2
mean(dat$NO_TK_AREA) #mean area of NO TAKE MPAs : 284.4256 km2 
range(dat$NO_TK_AREA) #range area of NO TAKE MPAs :   0 3236 km2 


     