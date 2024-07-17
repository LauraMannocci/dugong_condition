rm(list = ls())
library(magrittr)


#------------------------------------------ load RData -----------------------------------------

load("dat_final_all_envir.RData")
load("mes_dat_final_img.RData")


#select columns in dat_final_all_envir
dat_final_all_envir <- dat_final_all_envir %>% 
  dplyr::select(video_name_long, sum_seagrass_area_km2, mean_seagrass_patch_area_km2, seagrass_patch_number, percent_seagrass, mean_sst_celsius,
                gdp_per_capita, mean_gravity, sum_mpas_partial_no_take_area_km2, percent_mpas_partial_no_take, 
                sum_mpas_full_no_take_area_km2, percent_mpas_full_no_take, 
                mean_turbidity, buffer_area_km2, country, iucn_status, genetic_cluster,
                most_precise_date, approx_latitude, approx_longitude)


#check number of unique video_name_long 
#difference in video numbers between mes_dat_final_img and dat_final_all_envir is due to the selection of the first video 
#(when we do the group_by in lines 281 à 289 script 2-explore_measurement_tables)
#the true number of videos is 131
length(unique(mes_dat_final_img$video_name_long)) #131
sort(unique(mes_dat_final_img$video_name_long))

length(unique(dat_final_all_envir$video_name_long)) #121
sort(unique(dat_final_all_envir$video_name_long))

#keep only unique rows in dat_final_all_envir
dat_final_all_envir_unique <- unique(dat_final_all_envir)
nrow(dat_final_all_envir_unique) #121


#right join on dat_final_all_envir_unique and mes_dat_final_img by video_name_long 
dat_final_all_envir_img <- dplyr::right_join(mes_dat_final_img, dat_final_all_envir_unique, by = "video_name_long")
dim(mes_dat_final_img) #1200
dim(dat_final_all_envir_unique) #121
dim(dat_final_all_envir_img) #1133
#NB loss of 1200 - 1133 measurements because their video_name_long do not exist in dat_final_all_envir_unique 


#add column type_stage_refined 
dat_final_all_envir_img <- dat_final_all_envir_img %>% 
  dplyr::mutate(type_stage_refined = dplyr::case_when(
    type_individual_refined == "adult_female" ~ "adult_female",
    type_individual_refined %in% c("unidentified", "adult_male") ~ "unidentified",
    type_individual_refined %in% c("juvenile", "calf") ~ "juvenile")) %>% 
  dplyr::mutate(type_stage_refined = as.factor(type_stage_refined)) %>% 
  dplyr::mutate(indiv_id = paste0(video_name_long, "_", indiv_number)) 


table(dat_final_all_envir_img$type_stage_refined, useNA = "always")
dim(dat_final_all_envir_img) #1133


#save dat_final_all_envir_img
save(dat_final_all_envir_img, file = "dat_final_all_envir_img.RData")






#--------------------------------------- GLM with indiv_replicate as mixed effect ------------------------------------------
#distribution and density of response variable --> bimodal
ggplot2::ggplot(dat_final_all_envir_img, ggplot2::aes(x = body_condition)) +
  ggplot2::geom_histogram() +
  ggplot2::geom_density(alpha = 0.2, color = "red", fill = "red")
ggplot2::ggsave(here::here("outputs", "7-join_explore_environmental_mes_dat_final_img_with_interaction", "barplot_distribution_and_density.png"), width = 9, height = 6)

#--------------------------- SHAPIRO - normality test 
shapiro.test(log(dat_final_all_envir_img$body_condition))  #normal
shapiro.test(dat_final_all_envir_img$body_condition) #not normal


#------------------------------------------------------ explore model ----------------------------------------------
rm(list = ls())
load("dat_final_all_envir_img.RData")


#source function
source(file = here::here("functions", "functions_with_interaction_double_re.R"))

#mean_sst and gdp_per_capita highly correlated (>0.7)
#seagrass_patch_number and percent_seagrass highly correlated (>0.7)

#filter countries with low sample size to avoid convergence error 
dat_final_all_envir_img_country_filtered <- dat_final_all_envir_img %>% 
  dplyr::filter(!country %in% c("Mayotte", "Vanuatu", "Papua New Guinea", "Sri Lanka", "Timor-Leste", "Seychelles", "India", "Malaysia"))


#------- model1inter -------------------
model1inter <- glmmTMB::glmmTMB(log(body_condition) ~ mean_sst_celsius + mean_turbidity + percent_mpas_full_no_take +
                             mean_seagrass_patch_area_km2 + percent_mpas_partial_no_take + mean_gravity:country + seagrass_patch_number +
                               (1|type_stage_refined) + (1|type_stage_refined:indiv_id), data = dat_final_all_envir_img_country_filtered, family = gaussian)


#parameters 
model <- model1inter
model_name <- "model1inter"
ylim <- c(0.5, 1.6)
list_quantitative <- c( "mean_turbidity", "percent_mpas_full_no_take","mean_seagrass_patch_area_km2", 
                        "percent_mpas_partial_no_take", "mean_sst_celsius", "seagrass_patch_number")



#------- model1polyinter -------------------
model1polyinter <- glmmTMB::glmmTMB(log(body_condition) ~ poly(mean_sst_celsius, 2) + poly(mean_turbidity, 2) + poly(percent_mpas_full_no_take, 2) +
                              poly(mean_seagrass_patch_area_km2, 2) + poly(percent_mpas_partial_no_take, 2) + poly(mean_gravity, 2):country + poly(seagrass_patch_number, 2) + 
                                (1|type_stage_refined) + (1|type_stage_refined:indiv_id), data = dat_final_all_envir_img_country_filtered, family = gaussian)

#parameters 
model <- model1polyinter
model_name <- "model1polyinter"
ylim <- c(-0.2, 3)
list_quantitative <- c( "mean_turbidity", "percent_mpas_full_no_take","mean_seagrass_patch_area_km2", 
                        "percent_mpas_partial_no_take", "mean_sst_celsius", "seagrass_patch_number")


#------- model2inter -------------------
model2inter <- glmmTMB::glmmTMB(log(body_condition) ~ seagrass_patch_number + mean_turbidity + percent_mpas_full_no_take +
                             mean_seagrass_patch_area_km2 + percent_mpas_partial_no_take + mean_gravity:country + gdp_per_capita + 
                               (1|type_stage_refined) + (1|type_stage_refined:indiv_id), data = dat_final_all_envir_img_country_filtered, family = gaussian) 

#parameters 
model <- model2inter
model_name <- "model2inter"
ylim <- c(0.4, 2)
list_quantitative <- c( "mean_turbidity", "percent_mpas_full_no_take","mean_seagrass_patch_area_km2", 
                        "percent_mpas_partial_no_take", "seagrass_patch_number", "gdp_per_capita")



#------- model2polyinter -------------------
model2polyinter <- glmmTMB::glmmTMB(log(body_condition) ~ poly(seagrass_patch_number, 2) + poly(mean_turbidity, 2) + poly(percent_mpas_full_no_take, 2) +
                                  poly(mean_seagrass_patch_area_km2, 2) + poly(percent_mpas_partial_no_take, 2) + poly(mean_gravity, 2):country + poly(gdp_per_capita, 2) + 
                                    (1|type_stage_refined) + (1|type_stage_refined:indiv_id), data = dat_final_all_envir_img_country_filtered, family = gaussian)

#parameters 
model <- model2polyinter
model_name <- "model2polyinter"
ylim <- c(-0.1, 2.5)
list_quantitative <- c( "mean_turbidity", "percent_mpas_full_no_take","mean_seagrass_patch_area_km2", 
                        "percent_mpas_partial_no_take", "gdp_per_capita", "seagrass_patch_number")


#------- model3inter -------------------
model3inter <- glmmTMB::glmmTMB(log(body_condition) ~ mean_sst_celsius + percent_seagrass + mean_turbidity + 
                             percent_mpas_full_no_take + mean_seagrass_patch_area_km2 + percent_mpas_partial_no_take + mean_gravity:country + 
                             (1|type_stage_refined) + (1|type_stage_refined:indiv_id),  data = dat_final_all_envir_img_country_filtered, family = gaussian)
#parameters 
model <- model3inter
model_name <- "model3inter"
ylim <- c(0.6, 1.1)
list_quantitative <- c( "mean_sst_celsius", "percent_seagrass","mean_turbidity", "percent_mpas_full_no_take", 
                        "mean_seagrass_patch_area_km2", "percent_mpas_partial_no_take")



#------- model3polyinter -------------------
model3polyinter <- glmmTMB::glmmTMB(log(body_condition) ~ poly(mean_sst_celsius, 2) + poly(percent_seagrass, 2) + poly(mean_turbidity, 2) + 
                                  poly(percent_mpas_full_no_take, 2) + poly(mean_seagrass_patch_area_km2, 2) + poly(percent_mpas_partial_no_take, 2) + poly(mean_gravity, 2):country + 
                                 (1|type_stage_refined) + (1|type_stage_refined:indiv_id), data = dat_final_all_envir_img_country_filtered, family = gaussian)

#parameters 
model <- model3polyinter
model_name <- "model3polyinter"
ylim <- c(-0.3, 2.4)
list_quantitative <- c( "mean_sst_celsius", "percent_seagrass","mean_turbidity", "percent_mpas_full_no_take", 
                        "mean_seagrass_patch_area_km2", "percent_mpas_partial_no_take")



#------- model4inter -------------------
model4inter <- glmmTMB::glmmTMB(log(body_condition) ~ gdp_per_capita + percent_seagrass + mean_turbidity + 
                             percent_mpas_full_no_take + mean_seagrass_patch_area_km2 + percent_mpas_partial_no_take + mean_gravity:country + 
                             (1|type_stage_refined) + (1|type_stage_refined:indiv_id), data = dat_final_all_envir_img_country_filtered, family = gaussian)
#parameters 
model <- model4inter
model_name <- "model4inter"
ylim <- c(0.6, 1.1)
list_quantitative <- c( "gdp_per_capita", "percent_seagrass","mean_turbidity", "percent_mpas_full_no_take", 
                        "mean_seagrass_patch_area_km2", "percent_mpas_partial_no_take")



#------- model4polyinter -------------------
model4polyinter <- glmmTMB::glmmTMB(log(body_condition) ~ poly(gdp_per_capita, 2) + poly(percent_seagrass, 2) + poly(mean_turbidity, 2) + poly(mean_gravity, 2):country +
                                  poly(percent_mpas_full_no_take, 2) + poly(mean_seagrass_patch_area_km2, 2) + poly(percent_mpas_partial_no_take, 2) + 
                                 (1|type_stage_refined) + (1|type_stage_refined:indiv_id), data = dat_final_all_envir_img_country_filtered, family = gaussian)


#parameters 
model <- model4polyinter
model_name <- "model4polyinter"
ylim <- c(-0.5, 2.9)
list_quantitative <- c("gdp_per_capita", "percent_seagrass","mean_turbidity", "percent_mpas_full_no_take", 
                        "mean_seagrass_patch_area_km2", "percent_mpas_partial_no_take")




#----------------- call function to produce model outputs -----------------------------------
produce_model_outputs(model, model_name, list_quantitative, ylim)


#nice way to display summary for each model
dir_name <- here::here("outputs", "7-join_explore_environmental_mes_dat_final_img_with_interaction_double_re", model_name)
sjPlot::tab_model(model, show.reflvl = TRUE, p.style = "stars", digits.re = 4, file = here::here(dir_name, paste0("summary_", model_name, ".html")))
#The marginal R-squared considers only the variance of the fixed effects, while the conditional R-squared takes both the fixed and random effects into account.
#To get the contribution of random effects: conditional R-squared - marginal R-squared
#ICC = intra-class correlation coefficient: represents the consitency within an individual accross multiple measurements
#Sigma squared = variance of random effect
#https://strengejacke.github.io/sjPlot/articles/tab_mixed.html

#take screenshot of summary 
html_file <- here::here(dir_name, paste0("summary_", model_name, ".html"))
webshot2::webshot(html_file, file = "summary_capture.png", delay = 5, vwidth = 500)

#convert html file in png and load in model directory 
magick::image_write(magick::image_read("summary_capture.png"), path = here::here(dir_name, paste0("summary_", model_name, ".png")))




#--------------------------
#compare statistics of different model
sink(here::here("outputs", "7-join_explore_environmental_mes_dat_final_img_with_interaction_double_re", paste0("test_anova_models.txt")))
stats::anova(model1inter, model1polyinter, model2inter, model2polyinter, model3inter, model3polyinter, model4inter, model4polyinter, test = "Chisq")
sink(NULL)

sink(here::here("outputs", "7-join_explore_environmental_mes_dat_final_img_with_interaction_double_re", paste0("AIC_models.txt")))
bbmle::AICtab(model1inter, model1polyinter, model2inter, model2polyinter, model3inter, model3polyinter, model4inter, model4polyinter)
sink(NULL)

#nice way to display summary for all model
dir_name <- here::here("outputs", "7-join_explore_environmental_mes_dat_final_img_with_interaction_double_re")
sjPlot::tab_model(model1inter, model1polyinter, model2inter, model2polyinter, model3inter, model3polyinter, model4inter, model4polyinter, show.reflvl = TRUE, p.style = "stars", digits.re = 4, file = here::here(dir_name, paste0("tab_models_1inter_1polyinter_2inter_2polyinter_3inter_3polyinter_4inter_4polyinter.html")))

#take screenshot of summary 
html_file <- here::here(dir_name, paste0("tab_models_1inter_1polyinter_2inter_2polyinter_3inter_3polyinter_4inter_4polyinter.html"))
webshot2::webshot(html_file, file = "tab_model_capture.png", delay = 4, vwidth = 2000)

#convert html file in png and load in model directory 
magick::image_write(magick::image_read("tab_model_capture.png"), path = here::here(dir_name, paste0("tab_models_1inter_1polyinter_2inter_2polyinter_3inter_3polyinter_4inter_4polyinter.png")))






