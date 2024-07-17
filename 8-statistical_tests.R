rm(list = ls())
library(magrittr)



#------------check significance of BCI between countries------------------------
load("dat_final.RData")


#ANOVA - parametric
anova_BCI_country <- aov(mean_body_condition ~ country, data = dat_final)
summary(anova_BCI_country) #significant ***

#SHAPIRO - normality test 
residuals <- residuals(anova_BCI_country)
shapiro.test(residuals) #p-value > 0.05; W = 0.98773, p-value = 0.03203 
levene_test <- car::leveneTest(mean_body_condition ~ country, data = dat_final)
print(levene_test) #p-value = 0.07
#non normal distribution


#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ country, data = dat_final)
print(kruskal_test) #Kruskal-Wallis chi-squared = 94.373, df = 17, p-value = 9.7e-13 --> significant

#DUNN - post-hoc test
dunn_result <- dunn.test::dunn.test(dat_final$mean_body_condition, dat_final$country, method = "bonferroni")
print(dunn_result)


#------------check significance of BCI between type_individual------------------------
load("dat_final.RData")

#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ type_stage_refined, data = dat_final)
print(kruskal_test) #Kruskal-Wallis chi-squared = 14.657, df = 2, p-value = 0.0006565 --> significant

#DUNN - post-hoc test
dunn_result <- dunn.test::dunn.test(dat_final$mean_body_condition, dat_final$type_stage_refined, method = "bonferroni")
print(dunn_result)
#get significant couples
val <- dunn_result$P.adjusted[dunn_result$P.adjusted < 0.01]
ind <- which(dunn_result$P.adjusted %in% val)
dunn_result$comparisons[ind]


#------------check significance of BCI between UICN status ------------------------
load("dat_final.RData")


#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ iucn_status, data = dat_final)
print(kruskal_test) #Kruskal-Wallis chi-squared = 35.17, df = 3, p-value = 1.122e-07 --> significant

#DUNN - post-hoc test
dunn_result <- dunn.test::dunn.test(dat_final$mean_body_condition, dat_final$iucn_status, method = "bonferroni")
print(dunn_result)



#------------check significance of BCI between genetic_cluster ------------------------
load("dat_final.RData")

#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ genetic_cluster, data = dat_final)
print(kruskal_test) #Kruskal-Wallis chi-squared = 41.485, df = 4, p-value = 2.133e-08 --> significant

#DUNN - post-hoc test
dunn_result <- dunn.test::dunn.test(dat_final$mean_body_condition, dat_final$genetic_cluster, method = "bonferroni")
print(dunn_result)



#------------check significance of BCI predicted between genetic_cluster ------------------------
load("dat_final_all_envir_img.RData")

best_model <- model4poly # from script 10-join_explore_environmental_mes_dal_final_img_without_interaction
predicted_values <- predict(best_model, newdata = NULL)  #setting all random effects to zero
dat_final_all_envir_img <- cbind(dat_final_all_envir_img, predicted_body_condition = predicted_values)

dat_final_all_envir_img <- dat_final_all_envir_img %>% 
  dplyr::mutate(mean_predicted_body_condition = mean(predicted_body_condition)) %>% 
  dplyr::group_by(indiv_id) %>% 
  dplyr::mutate(mean_body_condition = mean(body_condition))


#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_predicted_body_condition ~ type_stage_refined, data = dat_final_all_envir_img)
print(kruskal_test) #Kruskal-Wallis chi-squared = 56.922, df = 2, p-value = 4.36e-13 --> significant 

#DUNN - post-hoc test
dunn_result <- dunn.test::dunn.test(dat_final_all_envir_img$mean_predicted_body_condition, dat_final_all_envir_img$type_stage_refined, method = "bonferroni")
print(dunn_result)



#------------check significance of BCI between province_admin ------------------------
load("dat_final.RData")

#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ province_admin, data = dat_final)
print(kruskal_test) #Kruskal-Wallis chi-squared = 122.78, df = 30, p-value = 3.474e-13 --> significant 

#DUNN - post-hoc test
dunn_result <- dunn.test::dunn.test(dat_final$mean_body_condition, dat_final$province_admin, method = "bonferroni")
print(dunn_result)



#------------check significance of BCI most_precise_location ------------------------
load("dat_final.RData")

#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ most_precise_location, data = dat_final)
print(kruskal_test) #Kruskal-Wallis chi-squared = 159.26, df = 68, p-value = 2.759e-09 --> significant 

#DUNN - post-hoc test
dunn_result <- dunn.test::dunn.test(dat_final$mean_body_condition, dat_final$most_precise_location, method = "bonferroni")
print(dunn_result)




#------------check significance of BCI most_precise_location and country ------------------------
load("dat_final.RData")

#these countries have a single location: Malaysia, Mayotte, PNG, Seychelles, Sri Lanka, Vanuatu

#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ most_precise_location, data = dat_final[dat_final$country == "Australia",])
print(kruskal_test) #Kruskal-Wallis chi-squared = 19.798, df = 17, p-value = 0.2847 --> NS 

#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ most_precise_location, data = dat_final[dat_final$country == "Indonesia",])
print(kruskal_test) #Kruskal-Wallis chi-squared = 30.13, df = 15, p-value = 0.01146 --> NS

#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ most_precise_location, data = dat_final[dat_final$country == "United Arab Emirates",])
print(kruskal_test) #Kruskal-Wallis chi-squared = 2.9233, df = 3, p-value = 0.4036 --> NS

#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ most_precise_location, data = dat_final[dat_final$country == "Saudi Arabia",])
print(kruskal_test) #Kruskal-Wallis chi-squared = 0.020979, df = 1, p-value = 0.8848 --> NS

#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ most_precise_location, data = dat_final[dat_final$country == "Mozambique" ,])
print(kruskal_test) #Kruskal-Wallis chi-squared = 2.3333, df = 1, p-value = 0.1266 --> NS

#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ most_precise_location, data = dat_final[dat_final$country == "Qatar",])
print(kruskal_test) #Kruskal-Wallis chi-squared = 5.3333, df = 1, p-value = 0.02092 --> NS

#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ most_precise_location, data = dat_final[dat_final$country == "Timor-Leste" ,])
print(kruskal_test) #Kruskal-Wallis chi-squared = 2, df = 2, p-value = 0.3679 --> NS

#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ most_precise_location, data = dat_final[dat_final$country == "New Caledonia",])
print(kruskal_test) #Kruskal-Wallis chi-squared = 3.5272, df = 3, p-value = 0.3173 --> NS

#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ most_precise_location, data = dat_final[dat_final$country == "Philippines",])
print(kruskal_test) #Kruskal-Wallis chi-squared = 2.2898, df = 4, p-value = 0.6826 --> NS

#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ most_precise_location, data = dat_final[dat_final$country == "Thailand", ])
print(kruskal_test) #Kruskal-Wallis chi-squared = 1.3393, df = 1, p-value = 0.2472--> NS

#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ most_precise_location, data = dat_final[dat_final$country == "India", ])
print(kruskal_test) #Kruskal-Wallis chi-squared = 0.53333, df = 2, p-value = 0.7659 --> NS

#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ most_precise_location, data = dat_final[dat_final$country == "Palau", ])
print(kruskal_test) #Kruskal-Wallis chi-squared = 7.9773, df = 1, p-value = 0.004737 --> signif

#DUNN - post-hoc test
dunn_result <- dunn.test::dunn.test(dat_final$mean_body_condition[dat_final$country == "Palau"], dat_final$most_precise_location[dat_final$country == "Palau"], method = "bonferroni")
print(dunn_result)




#------------check significance of images resolution between videos from social media or survey ------------------------
load("dat_final.RData")

dat_final <- dat_final %>% 
  dplyr::mutate(resolution_image = video_width * video_height)

#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(resolution_image ~ social_media, data = dat_final)
print(kruskal_test) # Kruskal-Wallis chi-squared = 19.886, df = 1, p-value = 8.221e-06--> significant  



#------------check significance of mean_overall_quality between videos from social media or survey ------------------------
#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_overall_quality ~ social_media, data = dat_final)
print(kruskal_test) # Kruskal-Wallis chi-squared = 2.0402, df = 1, p-value = 0.1532 --> non significant  



#------------check significance of BCI between videos from social media or survey  ------------------------
#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ social_media, data = dat_final)
print(kruskal_test) # Kruskal-Wallis chi-squared = 0.013075, df = 1, p-value = 0.909 --> non significant  






#----------------- effect of image resolution on BCI : statistical tests


#load data per individual 
load("dat_final.RData")

############ Correlation matrix for ACP
dat_final <- dat_final %>%
  dplyr::ungroup() %>%
  dplyr::select(c(mean_body_condition, mean_overall_quality, mean_resolution, mean_contrast, mean_distortion,                 
                  mean_partial, mean_angle_quality, mean_w_nb_pixels, mean_l_nb_pixels)) 


#---mean_overall_quality

#ANOVA - parametric
anova <- aov(mean_body_condition ~ mean_overall_quality, data = dat_final)
summary(anova) #non signif

#SHAPIRO - normality test 
residuals <- residuals(anova)
shapiro.test(residuals) #p-value > 0.05


#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ mean_overall_quality, data = dat_final)
print(kruskal_test) #Kruskal-Wallis chi-squared = 42.093, df = 38, p-value = 0.2982 non signif



#---mean_resolution

#ANOVA - parametric
anova <- aov(mean_body_condition ~ mean_resolution, data = dat_final)
summary(anova) #non signif

#SHAPIRO - normality test 
residuals <- residuals(anova)
shapiro.test(residuals) #p-value > 0.05


#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ mean_resolution, data = dat_final)
print(kruskal_test) #Kruskal-Wallis chi-squared = 5.4093, df = 12, p-value = 0.9429 non signif




#---mean_contrast

#ANOVA - parametric
anova <- aov(mean_body_condition ~ mean_contrast, data = dat_final)
summary(anova) #non signif

#SHAPIRO - normality test 
residuals <- residuals(anova)
shapiro.test(residuals) #p-value > 0.05


#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ mean_contrast, data = dat_final)
print(kruskal_test) #Kruskal-Wallis chi-squared = 11.923, df = 6, p-value = 0.06372 non signif




#---mean_distortion

#ANOVA - parametric
anova <- aov(mean_body_condition ~ mean_distortion, data = dat_final)
summary(anova) #non signif

#SHAPIRO - normality test 
residuals <- residuals(anova)
shapiro.test(residuals) #p-value > 0.05


#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ mean_distortion, data = dat_final)
print(kruskal_test) #Kruskal-Wallis chi-squared = 3.9189, df = 8, p-value = 0.8644 non signif



#---mean_partial

#ANOVA - parametric
anova <- aov(mean_body_condition ~ mean_partial, data = dat_final)
summary(anova) # significative***

#SHAPIRO - normality test 
residuals <- residuals(anova)
shapiro.test(residuals) #p-value > 0.05


#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ mean_partial, data = dat_final)
print(kruskal_test) #Kruskal-Wallis chi-squared = 21.422, df = 11, p-value = 0.02926 non signif




#---mean_angle_quality

#ANOVA - parametric
anova <- aov(mean_body_condition ~ mean_angle_quality, data = dat_final)
summary(anova) #non signif

#SHAPIRO - normality test 
residuals <- residuals(anova)
shapiro.test(residuals) #p-value > 0.05


#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ mean_angle_quality, data = dat_final)
print(kruskal_test) #Kruskal-Wallis chi-squared = 9.2202, df = 7, p-value = 0.2372 non signif


