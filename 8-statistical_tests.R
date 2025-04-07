rm(list = ls())
library(magrittr)



#------------check significance of BCI between countries------------------------
load("dat_final.RData")

#select countries with at least 10 observations

dat_final %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::filter(n >= 10) %>% 
  dplyr::select(country) -> countries

dat_final %>% 
  dplyr::filter(country %in% countries$country) -> dat_final2

#ANOVA - parametric
anova_BCI_country <- aov(mean_body_condition ~ country, data = dat_final2)
summary(anova_BCI_country) #significant ***

#SHAPIRO - normality test 
residuals <- residuals(anova_BCI_country)
shapiro.test(residuals) #p-value > 0.05; W = 0.98579, p-value = 0.01651
levene_test <- car::leveneTest(mean_body_condition ~ country, data = dat_final2)
print(levene_test) #p-value = 0.07
#non normal distribution


#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ country, data = dat_final2)
print(kruskal_test) #Kruskal-Wallis chi-squared = 76.398, df = 9, p-value = 8.366e-13 --> significant

#DUNN - post-hoc test
dunn_result <- dunn.test::dunn.test(dat_final2$mean_body_condition, dat_final2$country, method = "bonferroni")
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


#select clusters with at least 10 observations

dat_final %>% 
  dplyr::group_by(genetic_cluster) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::filter(n >= 10) %>% 
  dplyr::select(genetic_cluster) -> clusters

dat_final %>% 
  dplyr::filter(genetic_cluster %in% clusters$genetic_cluster) -> dat_final2

#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ genetic_cluster, data = dat_final2)
print(kruskal_test) #Kruskal-Wallis chi-squared = 38.398, df = 3, p-value = 2.327e-08 --> significant

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

#select locations with at least 10 observations

dat_final %>% 
  dplyr::group_by(most_precise_location) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::filter(n >= 10) %>% 
  dplyr::select(most_precise_location) -> locations

dat_final %>% 
  dplyr::filter(most_precise_location %in% locations$most_precise_location) -> dat_final2

#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ most_precise_location, data = dat_final2)
print(kruskal_test) #Kruskal-Wallis chi-squared = 38.096, df = 8, p-value = 7.228e-06 --> significant 

#DUNN - post-hoc test
dunn_result <- dunn.test::dunn.test(dat_final2$mean_body_condition, dat_final2$most_precise_location, method = "bonferroni")
print(dunn_result)




#------------check significance of BCI most_precise_location and country ------------------------
load("dat_final.RData")

#only countries with at least 10 observations

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
kruskal_test <- kruskal.test(mean_body_condition ~ most_precise_location, data = dat_final[dat_final$country == "Qatar",])
print(kruskal_test) #Kruskal-Wallis chi-squared = 5.3333, df = 1, p-value = 0.02092 --> NS

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
print(kruskal_test) # Kruskal-Wallis chi-squared = 0.0075612, df = 1, p-value = 0.9307 --> non significant  






#----------------- effect of image resolution on BCI : statistical tests


#load data per individual 
load("dat_final.RData")



#---mean_overall_quality

dat_final %>% 
  dplyr::select(mean_body_condition, mean_overall_quality) %>% 
  dplyr::mutate(mean_overall_quality = round(mean_overall_quality)) -> dat_final2

#ANOVA - parametric
anova <- aov(mean_body_condition ~ mean_overall_quality, data = dat_final2)
summary(anova) #non signif

#SHAPIRO - normality test 
residuals <- residuals(anova)
shapiro.test(residuals) #p-value > 0.05


#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ mean_overall_quality, data = dat_final2)
print(kruskal_test) #Kruskal-Wallis chi-squared = 16.754, df = 10, p-value = 0.08 non signif



#---mean_resolution


dat_final %>% 
  dplyr::select(mean_body_condition, mean_resolution) %>% 
  dplyr::mutate(mean_resolution = round(mean_resolution)) -> dat_final2

#ANOVA - parametric
anova <- aov(mean_body_condition ~ mean_resolution, data = dat_final2)
summary(anova) #non signif

#SHAPIRO - normality test 
residuals <- residuals(anova)
shapiro.test(residuals) #p-value > 0.05


#Kruskal-Wallis - non parametric
kruskal_test <- kruskal.test(mean_body_condition ~ mean_resolution, data = dat_final2)
print(kruskal_test) #Kruskal-Wallis chi-squared = 4.0976, df = 7, p-value = 0.7685




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


