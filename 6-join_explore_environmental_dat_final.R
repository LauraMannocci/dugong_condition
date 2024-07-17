rm(list = ls())
load("dat_final_seagrass.RData")
load("dat_final_turbidity.RData")
load("dat_final_sst.RData")
load("dat_final_gdp_per_capita.RData")
load("dat_final_gravity.RData")
load("dat_final_mpas_partial_no_take.RData")
load("dat_final_mpas_full_no_take.RData")
library(magrittr)



#----------------------------- correct 3 individuals with data errors -------------------------------------

dat_final_seagrass <- dat_final_seagrass %>% 
  dplyr::filter(!(video_name_long == "GH034215_1.mp4" & indiv_number == 7 & type_individual == "female")) %>% 
  dplyr::filter(!(video_name_long == "FaisalAljarkas.mp4" & indiv_number == 3 & type_individual == "unidentified")) %>% 
  dplyr::filter(!(video_name_long == "FaisalAljarkas.mp4" & indiv_number == 2 & type_individual == "female"))

dat_final_turbidity <- dat_final_turbidity %>% 
  dplyr::filter(!(video_name_long == "GH034215_1.mp4" & indiv_number == 7 & type_individual == "female")) %>% 
  dplyr::filter(!(video_name_long == "FaisalAljarkas.mp4" & indiv_number == 3 & type_individual == "unidentified")) %>% 
  dplyr::filter(!(video_name_long == "FaisalAljarkas.mp4" & indiv_number == 2 & type_individual == "female"))

dat_final_sst <- dat_final_sst %>% 
  dplyr::filter(!(video_name_long == "GH034215_1.mp4" & indiv_number == 7 & type_individual == "female")) %>% 
  dplyr::filter(!(video_name_long == "FaisalAljarkas.mp4" & indiv_number == 3 & type_individual == "unidentified")) %>% 
  dplyr::filter(!(video_name_long == "FaisalAljarkas.mp4" & indiv_number == 2 & type_individual == "female"))

dat_final_gdp_per_capita <- dat_final_gdp_per_capita %>% 
  dplyr::filter(!(video_name_long == "GH034215_1.mp4" & indiv_number == 7 & type_individual == "female")) %>% 
  dplyr::filter(!(video_name_long == "FaisalAljarkas.mp4" & indiv_number == 3 & type_individual == "unidentified")) %>% 
  dplyr::filter(!(video_name_long == "FaisalAljarkas.mp4" & indiv_number == 2 & type_individual == "female"))

dat_final_gravity <- dat_final_gravity %>% 
  dplyr::filter(!(video_name_long == "GH034215_1.mp4" & indiv_number == 7 & type_individual == "female")) %>% 
  dplyr::filter(!(video_name_long == "FaisalAljarkas.mp4" & indiv_number == 3 & type_individual == "unidentified")) %>% 
  dplyr::filter(!(video_name_long == "FaisalAljarkas.mp4" & indiv_number == 2 & type_individual == "female"))

dat_final_mpas_partial_no_take <- dat_final_mpas_partial_no_take %>% 
  dplyr::filter(!(video_name_long == "GH034215_1.mp4" & indiv_number == 7 & type_individual == "female")) %>% 
  dplyr::filter(!(video_name_long == "FaisalAljarkas.mp4" & indiv_number == 3 & type_individual == "unidentified")) %>% 
  dplyr::filter(!(video_name_long == "FaisalAljarkas.mp4" & indiv_number == 2 & type_individual == "female"))

dat_final_mpas_full_no_take <- dat_final_mpas_full_no_take %>% 
  dplyr::filter(!(video_name_long == "GH034215_1.mp4" & indiv_number == 7 & type_individual == "female")) %>% 
  dplyr::filter(!(video_name_long == "FaisalAljarkas.mp4" & indiv_number == 3 & type_individual == "unidentified")) %>% 
  dplyr::filter(!(video_name_long == "FaisalAljarkas.mp4" & indiv_number == 2 & type_individual == "female"))


#------------------------------------------------- join all dat_final in same data --------------------------------------

all_columns <- c("type_individual", "social_media", "video_owner", "owner_type", "source", 
                 "publication_date", "collection_date", "overall_location", "country", "most_precise_location", 
                 "approx_latitude", "approx_longitude", "localisation_precision", "number_individual_measured", "video_width",
                 "video_height", "same_individual_measured", "collection_year", "publication_year", "collection_month", 
                 "publication_month", "iucn_status", "province_marsh", "province_admin", "lat_province_admin", 
                 "lon_province_admin", 
                 "young_identification", "video_name_long", "indiv_number", "nb_indiv_replicate", "mean_body_condition",
                 "sd_body_condition", "mean_resolution", "mean_contrast", "mean_distortion", "mean_partial", 
                 "mean_angle_quality", "mean_overall_quality", "sd_resolution", "sd_contrast", "sd_distortion", 
                 "sd_partial", "sd_angle_quality", "sd_overall_quality", "type_individual_refined", "type_stage", 
                 "most_precise_date")

#join all datasets
dat_final_all_envir <- dat_final_seagrass %>%
  dplyr::left_join(dat_final_sst, by = all_columns) %>%
  dplyr::left_join(dat_final_gdp_per_capita, by = all_columns) %>%
  dplyr::left_join(dat_final_gravity, by = all_columns) %>% 
  dplyr::left_join(dat_final_mpas_partial_no_take, by = all_columns) %>% 
  dplyr::left_join(dat_final_mpas_full_no_take, by = all_columns) %>% 
  dplyr::left_join(dat_final_turbidity, by = all_columns)
  





#------------------------------------------ clean and reorder columns in dat_final_all_envir_data ------------------------------------

dat_final_all_envir <- dat_final_all_envir %>%
  #cleaning of mean_seagrass_patch_area_km2
  dplyr::mutate(mean_seagrass_patch_area_km2_new = ifelse(is.nan(mean_seagrass_patch_area_km2), 0, mean_seagrass_patch_area_km2)) %>% 
  dplyr::select(-mean_seagrass_patch_area_km2) %>%  
  dplyr::rename(mean_seagrass_patch_area_km2 = "mean_seagrass_patch_area_km2_new") %>% 
  #cleaning of percent_mpas_partial_no_take
  dplyr::mutate(percent_mpas_partial_no_take_new = ifelse(percent_mpas_partial_no_take > 100, 100, percent_mpas_partial_no_take)) %>% 
  dplyr::select(-percent_mpas_partial_no_take) %>%  
  dplyr::rename(percent_mpas_partial_no_take = "percent_mpas_partial_no_take_new") %>% 
  dplyr::mutate(id = dplyr::row_number()) %>% 
  dplyr::select(id, social_media, video_name_long, video_owner, owner_type, source, 
                publication_date, collection_date, collection_year, publication_year, collection_month, 
                publication_month, most_precise_date, month, overall_location, country, most_precise_location, 
                localisation_precision, approx_latitude, approx_longitude, province_marsh, province_admin, 
                lon_province_admin, lat_province_admin, iucn_status, genetic_cluster, video_height, video_width,
                mean_body_condition, 
                sd_body_condition, mean_resolution, mean_contrast, mean_distortion, mean_partial, 
                mean_angle_quality, mean_overall_quality, sd_resolution, sd_contrast, sd_distortion, 
                sd_partial, sd_angle_quality, sd_overall_quality, type_individual, young_identification, 
                type_individual_refined, type_stage, number_individual_measured, 
                indiv_number, nb_indiv_replicate, same_individual_measured, sum_seagrass_area_km2,
                mean_seagrass_patch_area_km2, seagrass_patch_number, percent_seagrass, mean_sst_celsius,
                gdp_per_capita, mean_gravity, sum_mpas_partial_no_take_area_km2, 
                percent_mpas_partial_no_take, sum_mpas_full_no_take_area_km2, percent_mpas_full_no_take, 
                mean_turbidity, buffer_area_km2) %>% 
  dplyr::filter(is.na(mean_turbidity) == FALSE)

  
save(dat_final_all_envir, file = "dat_final_all_envir.RData")






#--------------------------------------- explore data ---------------------------------------
rm(list = ls())
load("dat_final_all_envir.RData")
library(magrittr)

summary(dat_final_all_envir)

#export dat final all envir in csv file
#write.csv(dat_final_all_envir, file = "dat_final_all_envir.csv", row.names = FALSE)






#-------------------------------------- seagrass --------------------------------------------
#########plot seagrass vs mean_body_condition
p1 <- ggplot2::ggplot(dat_final_all_envir, ggplot2::aes(x = percent_seagrass, y = mean_body_condition)) +
        ggplot2::geom_point() + 
        ggplot2::geom_smooth(method = "lm", color = "red")

p2 <- ggplot2::ggplot(dat_final_all_envir, ggplot2::aes(x = mean_seagrass_patch_area_km2, y = mean_body_condition)) +
        ggplot2::geom_point() + 
        ggplot2::geom_smooth(method = "lm", color = "red")

p3 <- ggplot2::ggplot(dat_final_all_envir, ggplot2::aes(x = seagrass_patch_number, y = mean_body_condition)) +
  ggplot2::geom_point() + 
  ggplot2::geom_smooth(method = "lm", color = "red")

p <- gridExtra::grid.arrange(p1, p2, p3, ncol = 1)

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_seagrass_BC.png"), p, width = 8, height = 8)



#########barplot seagrass per province_admin
b1 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder(province_admin, percent_seagrass), y = percent_seagrass)) +
        ggplot2::geom_boxplot() +
        ggplot2::coord_flip() +
        ggplot2::labs(x = "")

b2 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder (province_admin, mean_seagrass_patch_area_km2), y = mean_seagrass_patch_area_km2)) +
        ggplot2::geom_boxplot() +
        ggplot2::coord_flip() +
        ggplot2::labs(x = "")

b3 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder (province_admin, seagrass_patch_number), y = seagrass_patch_number)) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "")

b <- gridExtra::grid.arrange(b1, b2, b3, ncol = 1)

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_seagrass_province_admin.png"), b, width = 7, height = 9)



#########barplot seagrass per country
b1 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder(country, percent_seagrass), y = percent_seagrass)) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "")

b2 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder (country, mean_seagrass_patch_area_km2), y = mean_seagrass_patch_area_km2)) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "")

b3 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder (country, seagrass_patch_number), y = seagrass_patch_number)) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "")

b <- gridExtra::grid.arrange(b1, b2, b3, ncol = 1)

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_seagrass_country.png"), b, width = 7, height = 9)


#########barplot seagrass per iucn_status
b1 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder(iucn_status, percent_seagrass), y = percent_seagrass)) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(x = "")

b2 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder (iucn_status, mean_seagrass_patch_area_km2), y = mean_seagrass_patch_area_km2)) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(x = "")

b3 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder (iucn_status, seagrass_patch_number), y = seagrass_patch_number)) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(x = "")

b <- gridExtra::grid.arrange(b1, b2, b3, ncol = 1)

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_seagrass_iucn_status.png"), b, width = 7, height = 9)



#-------------------------------------- sst --------------------------------------------
#########plot sst vs mean_body_condition
p1 <- ggplot2::ggplot(dat_final_all_envir, ggplot2::aes(x = mean_sst_celsius, y = mean_body_condition)) +
        ggplot2::geom_point() + 
        ggplot2::geom_smooth(method = "lm", color = "red")

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_sst_BC.png"), p1, width = 9, height = 7)



#########barplot sst per province_admin
b1 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder(province_admin, mean_sst_celsius), y = mean_sst_celsius)) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "")

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_sst_province_admin.png"), b1, width = 7, height = 7)



#########barplot sst per country
b1 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder(country, mean_sst_celsius), y = mean_sst_celsius)) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "")

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_sst_country.png"), b1, width = 7, height = 7)



#########barplot sst per iucn_status
b1 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder(iucn_status, mean_sst_celsius), y = mean_sst_celsius)) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(x = "")

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_sst_iucn_status.png"), b1, width = 7, height = 7)



#-------------------------------------- gravity --------------------------------------------
#########plot gravity vs mean_body_condition
p1 <- ggplot2::ggplot(dat_final_all_envir, ggplot2::aes(x = mean_gravity, y = mean_body_condition)) +
  ggplot2::geom_point() + 
  ggplot2::geom_smooth(method = "lm", color = "red")

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_gravity_BC.png"), p1, width = 9, height = 7)



#########barplot gravity per province_admin
b1 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder(province_admin, mean_gravity), y = mean_gravity)) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "")

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_gravity_province_admin.png"), b1, width = 7, height = 7)




#########barplot gravity per country
b1 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder(country, mean_gravity), y = mean_gravity)) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "")

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_gravity_country.png"), b1, width = 7, height = 7)




#########barplot gravity per iucn_status
b1 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder(iucn_status, mean_gravity), y = mean_gravity)) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(x = "")

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_gravity_iucn_status.png"), b1, width = 7, height = 7)



#-------------------------------------- gdp per capita --------------------------------------------
#########plot gdp_per_capita vs mean_body_condition
p1 <- ggplot2::ggplot(dat_final_all_envir, ggplot2::aes(x = gdp_per_capita, y = mean_body_condition)) +
  ggplot2::geom_point() + 
  ggplot2::geom_smooth(method = "lm", color = "red")

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_gdp_per_capita_BC.png"), p1, width = 9, height = 7)



#########barplot gdp_per_capita per province_admin
b1 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder(province_admin, gdp_per_capita), y = gdp_per_capita)) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "")

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_gdp_per_capita_province_admin.png"), b1, width = 7, height = 7)


#########barplot gdp_per_capita per country
b1 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder(country, gdp_per_capita), y = gdp_per_capita)) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "")

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_gdp_per_capita_country.png"), b1, width = 7, height = 7)


#########barplot gdp_per_capita per iucn_status
b1 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder(iucn_status, gdp_per_capita), y = gdp_per_capita)) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(x = "")

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_gdp_per_capita_iucn_status.png"), b1, width = 7, height = 7)



#-------------------------------------- MPA --------------------------------------------
#########plot mpa vs mean_body_condition
p1 <- ggplot2::ggplot(dat_final_all_envir, ggplot2::aes(x = percent_mpas_partial_no_take, y = mean_body_condition)) +
  ggplot2::geom_point() + 
  ggplot2::geom_smooth(method = "lm", color = "red")

p2 <- ggplot2::ggplot(dat_final_all_envir, ggplot2::aes(x = percent_mpas_full_no_take, y = mean_body_condition)) +
  ggplot2::geom_point() + 
  ggplot2::geom_smooth(method = "lm", color = "red")

p <- gridExtra::grid.arrange(p1, p2, ncol = 1)

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_mpa_BC.png"), p, width = 6, height = 9)




#########barplot mpa per province_admin
b1 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder (province_admin, percent_mpas_partial_no_take), y = percent_mpas_partial_no_take)) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "")

b2 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder(province_admin, percent_mpas_full_no_take), y = percent_mpas_full_no_take)) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "")

b <- gridExtra::grid.arrange(b1, b2, ncol = 1)

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_mpa_province_admin.png"), b, width = 7, height = 11)



#########barplot mpa per country
b1 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder (country, percent_mpas_partial_no_take), y = percent_mpas_partial_no_take)) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "")

b2 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder(country, percent_mpas_full_no_take), y = percent_mpas_full_no_take)) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "")

b <- gridExtra::grid.arrange(b1, b2, ncol = 1)

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_mpa_country.png"), b, width = 7, height = 11)



#########barplot mpa per iucn_status
b1 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder (iucn_status, percent_mpas_partial_no_take), y = percent_mpas_partial_no_take)) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(x = "")

b2 <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder(iucn_status, percent_mpas_full_no_take), y = percent_mpas_full_no_take)) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(x = "")

b <- gridExtra::grid.arrange(b1, b2, ncol = 1)

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_mpa_iucn_status.png"), b, width = 6, height = 11)



#-------------------------------------- turbidity --------------------------------------------
#########plot turbidity vs mean_body_condition
p <- ggplot2::ggplot(dat_final_all_envir, ggplot2::aes(x = mean_turbidity, y = mean_body_condition)) +
  ggplot2::geom_point() + 
  ggplot2::geom_smooth(method = "lm", color = "red")


ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_turbidity_BC.png"), p, width = 6, height = 9)



#########plot turbidity vs percent_seagrass
p <- ggplot2::ggplot(dat_final_all_envir, ggplot2::aes(x = mean_turbidity, y = percent_seagrass)) +
  ggplot2::geom_point() + 
  ggplot2::geom_smooth(method = "lm", color = "red")

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_turbidity_percent_seagrass.png"), p, width = 6, height = 9)




#########barplot turbidity per province_admin
b <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder(province_admin, mean_turbidity), y = mean_turbidity)) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "")

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_turbidity_province_admin.png"), b, width = 7, height = 11)



#########barplot turbidity per country
b <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder(country, mean_turbidity), y = mean_turbidity)) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "")

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_turbidity_country.png"), b, width = 7, height = 11)



#########barplot turbidity per iucn_status
b <- ggplot2::ggplot(data = dat_final_all_envir, ggplot2::aes(x = reorder(iucn_status, mean_turbidity), y = mean_turbidity)) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(x = "")

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_turbidity_iucn_status.png"), b, width = 6, height = 11)





#---------------------------------- explore correlation between environmental variables -------------------
#----------------------------------------------- prepare all pca ---------------------------
############ Correlation matrix
dat_envir <- dat_final_all_envir %>% 
  dplyr::select(c(mean_seagrass_patch_area_km2, seagrass_patch_number, percent_seagrass, mean_sst_celsius, gdp_per_capita, mean_gravity, 
                  percent_mpas_partial_no_take, percent_mpas_full_no_take, mean_turbidity))

#Computing correlation matrix
matrix_cor_envir <- stats::cor(dat_envir)

#invert color palette : blue to red
palette_couleurs <- RColorBrewer::brewer.pal(n = 8, name = "RdYlBu")
palette_couleurs_inverse <- rev(palette_couleurs)

#Visualize the correlation matrix
png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_cor_envir.png"), width = 900, height = 900)
corrplot::corrplot(matrix_cor_envir, type = "upper",
                               col = palette_couleurs_inverse)
dev.off() #save plot without ggplot2

png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_cor_envir_coeff.png"), width = 5000, height = 5000)
corrplot::corrplot(matrix_cor_envir, type = "upper",
                   col = palette_couleurs_inverse, 
                   method = "number", 
                   tl.col = "black",
                   tl.cex = 2,
                   number.cex = 2, 
                   number.font = 2, 
                   cl.cex = 2)
dev.off() 


#--------------------------------------- Crammer's V test ------------------------------- (corr?lation entre variables qualitative)
#Cramer test between 2 categorical variables
#Chi-test : https://medium.com/@manindersingh120996/understanding-categorical-correlations-with-chi-square-test-and-cramers-v-a54fe153b1d6
# https://stats.stackexchange.com/questions/155523/r-prop-test-chi-squared-approximation-may-be-incorrect

#create data with all categorial variables 
dat_cramer_test <- dat_final_all_envir %>% 
  dplyr::select(c(type_stage, type_individual_refined, country, iucn_status))

#Visualization : Multiple correspondence analysis of 4 categorical variables 
png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_mca_cramer_test.png"), width = 900, height = 900)
mca_full <- FactoMineR::MCA(dat_cramer_test)
dev.off()

#Contingency table of 2 variables 
table_c_stage_country <- table(dat_cramer_test$type_stage, dat_cramer_test$country)
table_c_iucn_ind <- table(dat_cramer_test$iucn_status, dat_cramer_test$type_individual_refined)
table_c_country_ind <- table(dat_cramer_test$country, dat_cramer_test$type_individual_refined)
table_c_country_iucn <- table(dat_cramer_test$country, dat_cramer_test$iucn_status)
table_c_stage_ind <- table(dat_cramer_test$type_stage, dat_cramer_test$type_individual_refined)


#Chi test
chi_square_test <- stats::chisq.test(table_c_stage_country)# p-value= 0.8614 no significant relationship
print(chi_square_test)
chi_square_test <- stats::chisq.test(table_c_iucn_ind)# p-value= 0.7243 no significant relationship
print(chi_square_test)
chi_square_test <- stats::chisq.test(table_c_country_ind)# p-value= 0.9782 no significant relationship
print(chi_square_test)
chi_square_test <- stats::chisq.test(table_c_country_iucn)# p-value < 2.2e-16 high significant relationship
print(chi_square_test)
chi_square_test <- stats::chisq.test(table_c_stage_ind)# p-value < 2.2e-16 high significant relationship
print(chi_square_test)


#V-Cramer test 
confintr::cramersv(table_c_stage_country)#0.1944188 = low association
confintr::cramersv(table_c_iucn_ind) # 0.1088717 = low association
confintr::cramersv(table_c_country_ind) # 0.2015953 = low association
confintr::cramersv(table_c_country_iucn) # 1 = high association
confintr::cramersv(table_c_stage_ind) # 1 = high association




#-------------------------------- clustering of body condition --------------------------------------
#scale used to centers and/or scales the columns of a numeric matrix
dat_envir_response_rescale <- scale(dat_final_all_envir$mean_body_condition) #only numeric data
head(dat_envir_response_rescale, n = 3)

#Method n?1 - graph determine optimal number of clusters
png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_optimal_number_of_clusters_BC.png"), width = 900, height = 900)
factoextra::fviz_nbclust(dat_envir_response_rescale, kmeans, method = "wss") #Elbow method (look at the knee in graph), wss = within - cluster sum of square
dev.off()
nb_clusters = 3

#Method n?2 - Nbclust
nb_clust <- NbClust::NbClust(dat_envir_response_rescale, diss = NULL, distance = "euclidean", method = "complete")

#kmeans clustering
clusters <- stats::kmeans(dat_envir_response_rescale, centers = nb_clusters, iter.max = 10, nstart = 1)

#add number of cluster at dat_final_all_envir
dat_final_all_envir_cluster <- cbind(dat_final_all_envir, cluster = clusters$cluster)  

#check mean body condition by cluster
dat_final_all_envir_cluster %>% 
  dplyr::group_by(cluster) %>% 
  dplyr::summarise(mean(mean_body_condition))



#visualization percent of number of individual per cluster per country
percent_individual_per_cluster <- dat_final_all_envir_cluster %>% 
  dplyr::group_by(country, cluster) %>% 
  dplyr::summarise(count = dplyr::n()) %>% 
  dplyr::mutate(percentage = count/sum(count) * 100) %>% 
  #assign number 1 to 4 to average mean body condition in descending order 
  dplyr::mutate(cluster_range = dplyr::case_when(cluster == 1 ~ "2", 
                                                 cluster == 2 ~ "1",
                                                 cluster == 3 ~ "3")) %>% 
  dplyr::mutate(cluster_category = dplyr::case_when(cluster_range == 1 ~ "high", 
                                                    cluster_range == 2 ~ "medium",
                                                    cluster_range == 3 ~ "low")) %>% 
  #put in the correct order for visualization
  dplyr::mutate(cluster_category = factor(cluster_category, levels = c("high", "medium", "low"))) 



ggplot2::ggplot(percent_individual_per_cluster, ggplot2::aes(fill = cluster_category, x = country, y = percentage)) +
  ggplot2::geom_bar(stat = "identity", position = "fill") +
  ggplot2::labs(x = "", y = "Pourcentage of individuals per cluster", fill = "Cluster body condition") +
  ggplot2::scale_fill_brewer(palette = "Oranges") +
  ggplot2::geom_text(ggplot2::aes(label = paste0(round(percentage), "%")), 
                     position = ggplot2::position_fill(vjust = 0.5), 
                     color = "black", size = 3, fontface = "bold") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))


ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_percent_individual_cluster_per_country.png"), width = 9, height = 6)


#visualization percent of number of individual per cluster per iucn_status
percent_individual_per_cluster <- dat_final_all_envir_cluster %>% 
  dplyr::group_by(iucn_status, cluster) %>% 
  dplyr::summarise(count = dplyr::n()) %>% 
  dplyr::mutate(percentage = count/sum(count) * 100) %>% 
  #assign number 1 to 4 to average mean body condition in descending order 
  dplyr::mutate(cluster_range = dplyr::case_when(cluster == 1 ~ "2", 
                                                 cluster == 2 ~ "1",
                                                 cluster == 3 ~ "3")) %>% 
  dplyr::mutate(cluster_category = dplyr::case_when(cluster_range == 1 ~ "high", 
                                                    cluster_range == 2 ~ "medium",
                                                    cluster_range == 3 ~ "low")) %>% 
  #put in the correct order for visualization
  dplyr::mutate(cluster_category = factor(cluster_category, levels = c("high", "medium", "low"))) 



ggplot2::ggplot(percent_individual_per_cluster, ggplot2::aes(fill = cluster_category, x = iucn_status, y = percentage)) +
  ggplot2::geom_bar(stat = "identity", position = "fill") +
  ggplot2::labs(x = "", y = "Pourcentage of individuals per cluster", fill = "Cluster body condition") +
  ggplot2::scale_fill_brewer(palette = "Oranges") +
  ggplot2::geom_text(ggplot2::aes(label = paste0(round(percentage), "%")), 
                     position = ggplot2::position_fill(vjust = 0.5), 
                     color = "black", size = 4, fontface = "bold")

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_percent_individual_cluster_per_iucn_status.png"), width = 9, height = 6)


#visualization percent of number of individual per cluster per type_stage
percent_individual_per_cluster <- dat_final_all_envir_cluster %>% 
  dplyr::group_by(type_stage, cluster) %>% 
  dplyr::summarise(count = dplyr::n()) %>% 
  dplyr::mutate(percentage = count/sum(count) * 100) %>% 
  #assign number 1 to 4 to average mean body condition in descending order 
  dplyr::mutate(cluster_range = dplyr::case_when(cluster == 1 ~ "2", 
                                                 cluster == 2 ~ "1",
                                                 cluster == 3 ~ "3")) %>% 
  dplyr::mutate(cluster_category = dplyr::case_when(cluster_range == 1 ~ "high", 
                                                    cluster_range == 2 ~ "medium",
                                                    cluster_range == 3 ~ "low")) %>% 
  #put in the correct order for visualization
  dplyr::mutate(cluster_category = factor(cluster_category, levels = c("high", "medium", "low")))  



ggplot2::ggplot(percent_individual_per_cluster, ggplot2::aes(fill = cluster_category, x = type_stage, y = percentage)) +
  ggplot2::geom_bar(stat = "identity", position = "fill") +
  ggplot2::labs(x = "", y = "Pourcentage of individuals per cluster", fill = "Cluster body condition") +
  ggplot2::scale_fill_brewer(palette = "Oranges") +
  ggplot2::geom_text(ggplot2::aes(label = paste0(round(percentage), "%")), 
                     position = ggplot2::position_fill(vjust = 0.5), 
                     color = "black", size = 4, fontface = "bold") 

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_percent_individual_cluster_per_type_stage.png"), width = 9, height = 6)




#visualization percent of number of individual per cluster per type_individual
percent_individual_per_cluster <- dat_final_all_envir_cluster %>% 
  dplyr::group_by(type_individual, cluster) %>% 
  dplyr::summarise(count = dplyr::n()) %>% 
  dplyr::mutate(percentage = count/sum(count) * 100) %>% 
  #assign number 1 to 4 to average mean body condition in descending order 
  dplyr::mutate(cluster_range = dplyr::case_when(cluster == 1 ~ "2", 
                                                 cluster == 2 ~ "1",
                                                 cluster == 3 ~ "3")) %>% 
  dplyr::mutate(cluster_category = dplyr::case_when(cluster_range == 1 ~ "high", 
                                                    cluster_range == 2 ~ "medium",
                                                    cluster_range == 3 ~ "low")) %>% 
  #put in the correct order for visualization
  dplyr::mutate(cluster_category = factor(cluster_category, levels = c("high", "medium", "low"))) 



ggplot2::ggplot(percent_individual_per_cluster, ggplot2::aes(fill = cluster_category, x = type_individual, y = percentage)) +
  ggplot2::geom_bar(stat = "identity", position = "fill") +
  ggplot2::labs(x = "", y = "Pourcentage of individuals per cluster", fill = "Cluster body condition") +
  ggplot2::scale_fill_brewer(palette = "Oranges") +
  ggplot2::geom_text(ggplot2::aes(label = paste0(round(percentage), "%")), 
                     position = ggplot2::position_fill(vjust = 0.5), 
                     color = "black", size = 4, fontface = "bold") 

ggplot2::ggsave(here::here("outputs", "6-join_explore_environmental_dat_final", "barplot_percent_individual_cluster_per_type_individual.png"), width = 9, height = 6)




#---------------------------------------------- PCA of environmental variables ------------------------------------
#calculate eigenvalue
eigenvalue <- eigen(matrix_cor_envir)


#Kaiser criterion
graphics::barplot(eigenvalue$values, ylab = "eigenvalue", xlab = "principal component")
graphics::abline(h = mean(eigenvalue$values)) #visualization the limit = 1 --> 3 components > 1


#Screeplot : visualize eigenvalues
PCA = ade4::dudi.pca(dat_envir, scannf = FALSE, nf = 4)

png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_screeplot_eigenvalue.png"), width = 900, height = 900)
factoextra::fviz_eig(PCA, addlabels = TRUE,)
dev.off()
summary(PCA) #total inertia = 12 = number of variables


#Visualize correlation circle of variables (default plot)
png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_pca_default.png"), width = 900, height = 900)
factoextra::fviz_pca_var(PCA, repel = TRUE, col.var = "black") #component 1 and 2
dev.off()



#Control variable colors using their contributions
png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_pca_contrib.png"), width = 900, height = 900)
factoextra::fviz_pca_var(PCA, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE #avoid text overlapping
)
dev.off()

#Contributions of variables to PC1
png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_variables_contrib_axe1.png"), width = 900, height = 900)
factoextra::fviz_contrib(PCA, choice = "var", axes = 1, top = 10)
dev.off()

#Quality of representation of variables 
factoextra::fviz_cos2(PCA, choice = "var", axes = 1:2)

png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_pca.png"), width = 900, height = 900)
factoextra::fviz_pca_var(PCA, col.var = "cos2",
                         gradient.cols = c("blue", "yellow", "red"),
                         repel = TRUE)
dev.off()



#PCA for individuals
# Coordinates of individuals
ind <- factoextra::get_pca_ind(PCA)
head(ind$coord)
png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_pca_coord_individual.png"), width = 900, height = 900)
factoextra::fviz_pca_ind(PCA, col.ind = "cos2",  # cos2 = the quality of the individuals on the factor map
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             legend.title = "cos2",
             repel = TRUE #avoid text overlapping (slow if many points)
)
dev.off()


#Biplot of individuals and variables
factoextra::fviz_pca_biplot(PCA, repel = TRUE)


dat_final_all_envir_cluster <- dat_final_all_envir_cluster %>% 
  dplyr::mutate(cluster_range = dplyr::case_when(cluster == 1 ~ "2",  
                                                 cluster == 2 ~ "1",
                                                 cluster == 3 ~ "3")) %>% 
  dplyr::mutate(cluster_category = dplyr::case_when(cluster_range == 1 ~ "high",
                                                    cluster_range == 2 ~ "medium",
                                                    cluster_range == 3 ~ "low")) %>% 
#put in the correct order for visualization
  dplyr::mutate(cluster_category = factor(cluster_category, levels = c("high", "medium", "low")))                                                    

#PCA all individual with clustering with their mean_body_condition 
png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_pca_coord_individual_clustering.png"), width = 900, height = 900)
factoextra::fviz_pca_ind(PCA,
             label = "none", #hide individual labels
             habillage = factor(dat_final_all_envir_cluster$cluster_category, levels = c("high", "medium-high", "medium", "low")), #color by mean_body_condition groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07", "green"),
             legend.title = "Cluster body condition",
             addEllipses = TRUE #Concentration ellipses
)
dev.off()





#------------------------------ MFA of environmental and other variables  -----------------------

# tutorial: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/116-mfa-multiple-factor-analysis-in-r-essentials/
# Multiple factor analysis (MFA) (J. Pagès 2002) is a multivariate data analysis method for summarizing and visualizing a complex data table in which individuals 
# are described by several sets of variables (quantitative and /or qualitative) structured into groups. It takes into account the contribution of all active groups 
# of variables to define the distance between individuals. The number of variables in each group may differ and the nature of the variables (qualitative or quantitative) 
# can vary from one group to the other but the variables should be of the same nature in a given group (Abdi and Williams 2010).



#----- Data formating and MFA

dat_final_all_envir_cluster$cluster <- as.factor(dat_final_all_envir_cluster$cluster)

dat_mfa <- dat_final_all_envir_cluster[, c("country", "iucn_status", "type_individual_refined", "type_stage", "cluster",
                                           "mean_seagrass_patch_area_km2", "percent_seagrass", "mean_sst_celsius", "gdp_per_capita", "mean_gravity",                                         
                                           "percent_mpas_partial_no_take", "percent_mpas_full_no_take", "mean_turbidity")]  

#MFA with 2 groups: categorical and environmental variables
mfa <- FactoMineR::MFA(dat_mfa, group = c(5, 8), type = c("n", "s"), ind.sup = NULL, # c: quantitative, n: categorical, s: quantitative scaled
                       name.group = c("other", "environmental"), graph = TRUE) ### may change group and type here
mfa



#---- Get eigen values and make scree plot

eigen <- factoextra::get_eigenvalue(mfa)
eigen

factoextra::fviz_screeplot(mfa) 




#---- Graph of variables

group <- factoextra::get_mfa_var(mfa, "group")
group

# Coordinates of groups
head(group$coord)
# Cos2: quality of representation on the factore map
head(group$cos2)
# Contributions to the  dimensions
head(group$contrib)

#plot group of variables
png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_mfa_group.png"))
factoextra::fviz_mfa_var(mfa, "group")
dev.off()
# The plot above illustrates the correlation between groups and dimensions. The coordinates of the two active groups on the first dimension are almost identical. 
# This means that they contribute similarly to the first dimension. Concerning the second dimension, environmental group has the highest contribution

# Contribution to the first dimension
png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_mfa_contributions_group_dim1.png"))
factoextra::fviz_contrib(mfa, "group", axes = 1)
dev.off()

# Contribution to the second dimension
png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_mfa_contributions_group_dim2.png"))
factoextra::fviz_contrib(mfa, "group", axes = 2)
dev.off()

# Extract results for quantitative variables
quanti.var <- factoextra::get_mfa_var(mfa, "quanti.var")
quanti.var 

# Coordinates
head(quanti.var$coord)
# Cos2: quality on the factore map
head(quanti.var$cos2)
# Contributions to the dimensions
head(quanti.var$contrib)




#---- Plot quantitative variables colored by groups

png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_mfa_quantitative_var.png"))
#options(ggrepel.max.overlaps = 100000000, ggrepel.box.padding = 0.5) #Inf #https://ggrepel.slowkow.com/articles/examples.html#always-show-all-labels-even-when-they-have-too-many-overlaps
factoextra::fviz_mfa_var(mfa, "quanti.var", palette = "jco", 
                         col.var.sup = "violet", repel = TRUE) #with arrows
#this graph shows the relationship between variables, the quality of the representation of variables, as well as, the correlation between variables 
#and the dimensions
dev.off()

factoextra::fviz_mfa_var(mfa, "quanti.var", palette = "jco", 
                         col.var.sup = "violet", repel = TRUE,
                         geom = c("point", "text"), legend = "bottom") #without arrows




#---- Contribution of quantitative variables 

# Contributions to dimension 1

factoextra::fviz_contrib(mfa, choice = "quanti.var", axes = 1, top = 20,
                         palette = "jco")

# Contributions to dimension 2

factoextra::fviz_contrib(mfa, choice = "quanti.var", axes = 2, top = 20,
                         palette = "jco")

# Most contributing variables
factoextra::fviz_mfa_var(mfa, "quanti.var", col.var = "contrib", 
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                         col.var.sup = "violet", repel = TRUE,
                         geom = c("point", "text"))

# Color by cos2 values: quality on the factor map
factoextra::fviz_mfa_var(mfa, col.var = "cos2",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                         col.var.sup = "violet", repel = TRUE)

# bar plot of variables cos2
factoextra::fviz_cos2(mfa, choice = "quanti.var", axes = 1)





#---- Plot qualitative variables colored by groups

png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_mfa_qualitative_var.png"))
factoextra::fviz_mfa_var(mfa, "quali.var", palette = "jco", 
                         col.var.sup = "violet", repel = TRUE) #with arrows
dev.off()



#---- Contribution of qualitative variables 

# Contributions to dimension 1

factoextra::fviz_contrib(mfa, choice = "quali.var", axes = 1, top = 20,
                         palette = "jco")

# Contributions to dimension 2

factoextra::fviz_contrib(mfa, choice = "quali.var", axes = 2, top = 20,
                         palette = "jco")

# Most contributing variables
factoextra::fviz_mfa_var(mfa, "quali.var", col.var = "contrib", 
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                         col.var.sup = "violet", repel = TRUE,
                         geom = c("point", "text"))

# bar plot of variables cos2
factoextra::fviz_cos2(mfa, choice = "quali.var", axes = 1)


#---- Graph of individuals

ind <- factoextra::get_mfa_ind(mfa)
ind

factoextra::fviz_mfa_ind(mfa, col.ind = "cos2", 
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE)# Avoid text overlapping

factoextra::fviz_mfa_ind(mfa, col.ind = "cos2", 
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         addEllipses = TRUE, #add ellipse
                         repel = TRUE) 

#graph of individuals per categorical factor

png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_mfa_individuals_per_country_convex_hull_ellipses.png"))
factoextra::fviz_ellipses(mfa, c("country"), ellipse.type = "convex", repel = TRUE)
dev.off()

png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_mfa_individuals_per_iucn_status_convex_hull_ellipses.png"))
factoextra::fviz_ellipses(mfa, c("iucn_status"), ellipse.type = "convex", repel = TRUE)
dev.off()

png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_mfa_individuals_per_type_individual_refined_convex_hull_ellipses.png"))
factoextra::fviz_ellipses(mfa, c("type_individual_refined"), ellipse.type = "convex", repel = TRUE)
dev.off()

png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_mfa_individuals_per_type_stage_convex_hull_ellipses.png"))
factoextra::fviz_ellipses(mfa, c("type_stage"), ellipse.type = "convex", repel = TRUE)
dev.off()

png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_mfa_individuals_per_cluster_convex_hull_ellipses.png"))
factoextra::fviz_ellipses(mfa, c("cluster"), ellipse.type = "convex", repel = TRUE) # convex hull ellipses
dev.off()

png(here::here("outputs", "6-join_explore_environmental_dat_final", "plot_mfa_individuals_per_cluster_confidence_ellipses.png"))
factoextra::fviz_ellipses(mfa, c("cluster"), ellipse.type = "confidence", repel = TRUE) # confidence ellipses arround group mean points
dev.off()













