rm(list = ls())
library(magrittr)


#------------------------------------------ load RData -----------------------------------------

load("dat_final_all_envir.RData")
load("mes_dat_final_img.RData")


#select columns in dat_final_all_envir
dat_final_all_envir <- dat_final_all_envir %>% 
  dplyr::select(video_name_long, mean_sst_celsius,
                gdp_per_capita_province, mean_gravity, sum_mpas_partial_no_take_area_km2, percent_mpas_partial_no_take, 
                sum_mpas_all_area_km2, percent_mpas_all, 
                sum_mpas_full_no_take_area_km2, percent_mpas_full_no_take, 
                mean_turbidity, country, iucn_status, genetic_cluster,
                most_precise_date, approx_latitude, approx_longitude, month)


#check number of unique video_name_long 
#difference in video numbers between mes_dat_final_img and dat_final_all_envir is due to the selection of the first video 
#(when we do the group_by in lines 281 à 289 script 2-explore_measurement_tables)
#the true number of videos is 131
length(unique(mes_dat_final_img$video_name_long)) #131
sort(unique(mes_dat_final_img$video_name_long))

length(unique(dat_final_all_envir$video_name_long)) #124
sort(unique(dat_final_all_envir$video_name_long))

#keep only unique rows in dat_final_all_envir
dat_final_all_envir_unique <- unique(dat_final_all_envir)
nrow(dat_final_all_envir_unique) #124


#right join on dat_final_all_envir_unique and mes_dat_final_img by video_name_long 
dat_final_all_envir_img <- dplyr::right_join(mes_dat_final_img, dat_final_all_envir_unique, by = "video_name_long")
dim(mes_dat_final_img) #1200
dim(dat_final_all_envir_unique) #124
dim(dat_final_all_envir_img) #1156
#NB loss of 1200 - 1156 measurements because their video_name_long do not exist in dat_final_all_envir_unique 


#add column type_stage_refined 
dat_final_all_envir_img <- dat_final_all_envir_img %>% 
  dplyr::mutate(type_stage_refined = dplyr::case_when(
    type_individual_refined == "adult_female" ~ "adult_female",
    type_individual_refined %in% c("unidentified", "adult_male") ~ "unidentified",
    type_individual_refined %in% c("juvenile", "calf") ~ "juvenile")) %>% 
  dplyr::mutate(type_stage_refined = as.factor(type_stage_refined)) %>% 
  dplyr::mutate(indiv_id = paste0(video_name_long, "_", indiv_number)) 


table(dat_final_all_envir_img$type_stage_refined, useNA = "always")
dim(dat_final_all_envir_img) #1156


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
source(file = here::here("functions", "functions_with_interaction.R"))


#filter countries with low sample size <=5
dat_final_all_envir_img_country_filtered <- dat_final_all_envir_img %>% 
  dplyr::filter(!country %in% c("Mayotte", "Vanuatu", "Papua New Guinea", "Sri Lanka", "Timor-Leste", "Seychelles", "India")) 


#set argument of produce_model_outputs()
dat_model <- dat_final_all_envir_img_country_filtered

#factor corrections
dat_model$country <- as.factor(dat_model$country)
dat_model$type_stage_refined <- as.factor(dat_model$type_stage_refined)
dat_model$month <- as.factor(dat_model$month)

#mean_sst and gdp_per_capita_province highly correlated (>0.7)

#------- model1inter -------------------
model1inter <- glmmTMB::glmmTMB(log(body_condition) ~ mean_sst_celsius + percent_mpas_all + mean_gravity * country + 
                             type_stage_refined + month + (1|indiv_id), data = dat_model, family = gaussian)
#intercept varying among type_stage_refined and among indiv_id within type_stage_refined

#parameters 
model <- model1inter
model_name <- "model1inter"
ylim <- c(0.6, 0.9)
list_quantitative <- c("mean_sst_celsius",  "percent_mpas_all", "mean_gravity", "type_stage_refined", "month")


#------- model1polyinter -------------------
model1polyinter <- glmmTMB::glmmTMB(log(body_condition) ~ poly(mean_sst_celsius, 2) + poly(percent_mpas_all, 2) + poly(mean_gravity, 2) * country + 
                                 type_stage_refined + month + (1|indiv_id), data = dat_model, family = gaussian)
#intercept varying among type_stage_refined and among indiv_id within type_stage_refined

#parameters 
model <- model1polyinter
model_name <- "model1polyinter"
ylim <- c(0.5, 1)
list_quantitative <- c("mean_sst_celsius",  "percent_mpas_all", "mean_gravity", "type_stage_refined", "month")


#------- model2inter -------------------
model2inter <- glmmTMB::glmmTMB(log(body_condition) ~ gdp_per_capita_province + percent_mpas_all + mean_gravity * country + 
                             type_stage_refined + month + (1|indiv_id), data = dat_model, family = gaussian)
#intercept varying among type_stage_refined and among indiv_id within type_stage_refined

#parameters 
model <- model2inter
model_name <- "model2inter"
ylim <- c(0.6, 0.9)
list_quantitative <- c("gdp_per_capita_province",  "percent_mpas_all", "mean_gravity", "type_stage_refined", "month")


#------- model2polyinter -------------------
model2polyinter <- glmmTMB::glmmTMB(log(body_condition) ~ poly(gdp_per_capita_province, 2) + poly(percent_mpas_all, 2) + poly(mean_gravity, 2) + poly(mean_gravity, 2)*country + 
                                 type_stage_refined + month + (1|indiv_id), data = dat_model, family = gaussian)
#intercept varying among type_stage_refined and among indiv_id within type_stage_refined

#parameters 
model <- model2polyinter
model_name <- "model2polyinter"
ylim <- c(0.5, 1)
list_quantitative <- c("gdp_per_capita_province",  "percent_mpas_all", "mean_gravity", "type_stage_refined", "month")



#----------------- call function to produce outputs for each model -----------------------------------
produce_model_outputs(model, model_name, list_quantitative, ylim, dat_model)


#nice way to display summary for each model
dir_name <- here::here("outputs", "7-join_explore_environmental_mes_dat_final_img_with_interaction", model_name)
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
sink(here::here("outputs", "7-join_explore_environmental_mes_dat_final_img_with_interaction", paste0("test_anova_models.txt")))
stats::anova(model1inter, model1polyinter, model2inter, model2polyinter, test = "Chisq")
sink(NULL)

sink(here::here("outputs", "7-join_explore_environmental_mes_dat_final_img_with_interaction", paste0("AIC_models.txt")))
bbmle::AICtab(model1inter, model1polyinter, model2inter, model2polyinter)
sink(NULL)

#nice way to display summary for all model
dir_name <- here::here("outputs", "7-join_explore_environmental_mes_dat_final_img_with_interaction")
sjPlot::tab_model(model1inter, model1polyinter, model2inter, model2polyinter, show.reflvl = TRUE, p.style = "stars", digits.re = 4, file = here::here(dir_name, paste0("tab_models_1_1poly_2_2poly.html")))

#take screenshot of summary 
html_file <- here::here(dir_name, paste0("tab_models_1_1poly_2_2poly.html"))
webshot2::webshot(html_file, file = "tab_model_capture.png", delay = 4, width = 2000)

#convert html file in png and load in model directory 
magick::image_write(magick::image_read("tab_model_capture.png"), path = here::here(dir_name, paste0("tab_models_1_1poly_2_2poly.png")))





############ explore predicted_body_condition variable from dat_final_all_envir_img
#add column predicted_body_condition to dat_final_all_envir_img
best_model <- model4poly
predicted_values <- predict(best_model, newdata = NULL)  #setting all random effects to zero
dat_final_all_envir_img <- cbind(dat_final_all_envir_img, predicted_body_condition = predicted_values)

dat_final_all_envir_img <- dat_final_all_envir_img %>% 
  dplyr::mutate(mean_predicted_body_condition = mean(predicted_body_condition)) %>% 
  dplyr::group_by(indiv_id) %>% 
  dplyr::mutate(mean_body_condition = mean(body_condition))


###plot predicted_body_condition versus iucn_status_check 
dat_final_all_envir_img <- dat_final_all_envir_img %>% 
  dplyr::mutate(iucn_status_check = dplyr::case_when(country == "Mozambique" ~ "CR",
                                                     country == "New Caledonia" ~ "EN",
                                                     TRUE ~ "DD"))

colors <- c("NT" = "yellowgreen", "EN" = "orange", "CR" = "red", "DD" = "grey60")
iucn_levels <- c("CR", "EN", "NT", "DD")
dat_final_all_envir_img$iucn_status_check <- factor(dat_final_all_envir_img$iucn_status_check, levels = iucn_levels)
dat_final_all_envir_img$iucn_status <- factor(dat_final_all_envir_img$iucn_status, levels = iucn_levels)


ggplot2::ggplot(dat_final_all_envir_img, ggplot2::aes(x = country, y = predicted_body_condition, fill = iucn_status_check)) +
  ggplot2::geom_point(ggplot2::aes(color = iucn_status_check)) +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::scale_color_manual(values = colors) +
  ggplot2::labs(x = "", y = "Body condition predicted") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9)) +
  ggplot2::geom_smooth(ggplot2::aes(color = iucn_status_check), method = lm, se = FALSE, fullrange = TRUE) 

ggplot2::ggsave(here::here("outputs", "7-join_explore_environmental_mes_dat_final_img", "barplot_predicted_BC_per_country_per_iucn_status_check.png"), width = 10, height = 6)


###plot body_condition versus iucn_status_check 
ggplot2::ggplot(dat_final_all_envir_img, ggplot2::aes(x = country, y = mean_body_condition, fill = iucn_status_check)) +
  ggplot2::geom_point(ggplot2::aes(color = iucn_status_check)) +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::scale_color_manual(values = colors) +
  ggplot2::labs(x = "", y = "Mean body condition measured") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9)) +
  ggplot2::geom_smooth(ggplot2::aes(color = iucn_status_check), method = lm, se = FALSE, fullrange = TRUE) 

ggplot2::ggsave(here::here("outputs", "7-join_explore_environmental_mes_dat_final_img", "barplot_BC_per_country_per_iucn_status_check.png"), width = 10, height = 6)


###plot predicted_body_condition versus iucn_status

ggplot2::ggplot(dat_final_all_envir_img, ggplot2::aes(x = country, y = predicted_body_condition, fill = iucn_status)) +
  ggplot2::geom_point(ggplot2::aes(color = iucn_status)) +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::scale_color_manual(values = colors) +
  ggplot2::labs(x = "", y = "Body condition predicted") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9)) +
  ggplot2::geom_smooth(ggplot2::aes(color = iucn_status), method = lm, se = FALSE, fullrange = TRUE) 

ggplot2::ggsave(here::here("outputs", "7-join_explore_environmental_mes_dat_final_img", "barplot_predicted_BC_per_country_per_iucn_status.png"), width = 10, height = 6)


###plot body_condition versus iucn_status 
ggplot2::ggplot(dat_final_all_envir_img, ggplot2::aes(x = country, y = mean_body_condition, fill = iucn_status)) +
  ggplot2::geom_point(ggplot2::aes(color = iucn_status)) +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::scale_color_manual(values = colors) +
  ggplot2::labs(x = "", y = "Mean body condition measured") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9)) +
  ggplot2::geom_smooth(ggplot2::aes(color = iucn_status), method = lm, se = FALSE, fullrange = TRUE) 

ggplot2::ggsave(here::here("outputs", "7-join_explore_environmental_mes_dat_final_img", "barplot_BC_per_country_per_iucn_status.png"), width = 10, height = 6)


###plot predicted_body_condition versus country 
ggplot2::ggplot(dat_final_all_envir_img, ggplot2::aes(x = mean_body_condition, y = mean_predicted_body_condition, fill = country)) +
  ggplot2::geom_point(ggplot2::aes(color = country)) +
  ggplot2::labs(x = "Mean body condition measured", y = "Mean body condition predicted") +
  ggplot2::geom_smooth(ggplot2::aes(color = country), method = lm, se = FALSE, fullrange = TRUE) 

ggplot2::ggsave(here::here("outputs", "7-join_explore_environmental_mes_dat_final_img", "barplot_predicted_BC_VS_BC_per_country.png"), width = 10, height = 6)


###plot predicted_body_condition versus body_condition 
ggplot2::ggplot(dat_final_all_envir_img, ggplot2::aes(x = mean_body_condition, y = mean_predicted_body_condition)) +
  ggplot2::geom_point()+
  ggplot2::labs(x = "Mean body condition measured", y = "Mean body condition predicted") +
  ggplot2::geom_smooth()

ggplot2::ggsave(here::here("outputs", "7-join_explore_environmental_mes_dat_final_img", "barplot_predicted_BC_VS_BC.png"), width = 6, height = 6)


#check mean and sd predicted_mean_body_condition per country 
dat_final_country_bc_predicted <- dat_final_all_envir_img %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(mean_condition = mean(mean_predicted_body_condition, na.rm = TRUE),
                   sd_condition = sd(mean_predicted_body_condition, na.rm = TRUE)) 


