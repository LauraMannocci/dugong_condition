rm(list=ls())
#load pipe operator
library(magrittr)


#----------------- sensitivity of BCI to number of pixels

#load data per image
load("mes_dat_final_img.RData")

#caculate sensitivity to addition of one pixel in length, width and both length and width on body condition index
mes_dat_final_img <- mes_dat_final_img %>% 
  dplyr::mutate(body_condition_test_sensibility_l = (w_nb_pixels * pi) / (l_nb_pixels + 1)) %>% 
  dplyr::mutate(body_condition_test_sensibility_w = ((w_nb_pixels + 1) * pi) / (l_nb_pixels)) %>% 
  dplyr::mutate(body_condition_test_sensibility = ((w_nb_pixels + 1) * pi) / (l_nb_pixels + 1)) 

#means
mean(mes_dat_final_img$body_condition) # 0.7536023
mean(mes_dat_final_img$body_condition_test_sensibility_l) # 0.7487125
mean(mes_dat_final_img$body_condition_test_sensibility_w) # 0.7740983
mean(mes_dat_final_img$body_condition_test_sensibility) #  0.7689745

#impact of addition of one pixel on BCI
100*(mean(mes_dat_final_img$body_condition_test_sensibility_l) - mean(mes_dat_final_img$body_condition)) / mean(mes_dat_final_img$body_condition) #-0.67 % -> BCI decreases by 0.67 %
100*(mean(mes_dat_final_img$body_condition_test_sensibility_w) - mean(mes_dat_final_img$body_condition)) / mean(mes_dat_final_img$body_condition) #2.81 % -> BCI increases by 2.81 %
100*(mean(mes_dat_final_img$body_condition_test_sensibility) - mean(mes_dat_final_img$body_condition)) / mean(mes_dat_final_img$body_condition) #2.10 % -> BCI increases by 2.10 %
#ccl: BCI is 4 times more sensitive to width than to lentgh - in other words BCI is more robust to changes in length

# normality test 
shapiro.test(mes_dat_final_img$body_condition) # p-value = 8.132e-06 = normality 

# normality test for two variables 
ks.test(mes_dat_final_img$body_condition, mes_dat_final_img$body_condition_test_sensibility_l) # p-value = 0.7676 --> same distribution = normality

#t test for paired variables 
t.test(mes_dat_final_img$body_condition, mes_dat_final_img$body_condition_test_sensibility_l, paired = TRUE) # significant 
t.test(mes_dat_final_img$body_condition, mes_dat_final_img$body_condition_test_sensibility_w, paired = TRUE) # significant 
t.test(mes_dat_final_img$body_condition, mes_dat_final_img$body_condition_test_sensibility, paired = TRUE) # significant 


#load data per individual 
load("mes_dat_final_vid.RData")

#caculate sensitivity to addition of one pixel in length, width and both length and width on body condition index
mes_dat_final_vid <- mes_dat_final_vid %>% 
  dplyr::mutate(body_condition_test_sensibility_l = (mean_w_nb_pixels * pi) / (mean_l_nb_pixels + 1)) %>% 
  dplyr::mutate(body_condition_test_sensibility_w = ((mean_w_nb_pixels + 1) * pi) / (mean_l_nb_pixels)) %>% 
  dplyr::mutate(body_condition_test_sensibility = ((mean_w_nb_pixels + 1) * pi) / (mean_l_nb_pixels + 1))
  
#means
mean(mes_dat_final_vid$body_condition) # 0.7537955
mean(mes_dat_final_vid$body_condition_test_sensibility_l) #  0.7482747
mean(mes_dat_final_vid$body_condition_test_sensibility_w) #  0.7748824
mean(mes_dat_final_vid$body_condition_test_sensibility) #  0.7695056

#impact of addition of one pixel on BCI
100*mean((mes_dat_final_vid$body_condition_test_sensibility_l - mes_dat_final_vid$mean_body_condition) / mes_dat_final_vid$mean_body_condition) #-0.75 % -> BCI decreases by 0.75 %
100*mean((mes_dat_final_vid$body_condition_test_sensibility_w - mes_dat_final_vid$mean_body_condition) / mes_dat_final_vid$mean_body_condition)  #2.92 % -> BCI increases by 2.92 %
100*mean((mes_dat_final_vid$body_condition_test_sensibility - mes_dat_final_vid$mean_body_condition) / mes_dat_final_vid$mean_body_condition) #2.18 % -> BCI increases by 2.18 %
#ccl: BCI is 4 times more sensitive to width than to lentgh - in other words BCI is more robust to changes in length

# normality test 
shapiro.test(mes_dat_final_vid$mean_body_condition) # p-value = 0.1012 = non normality 

# normality test for two variables 
ks.test(mes_dat_final_vid$mean_body_condition, mes_dat_final_vid$mean_body_condition_test_sensibility_l) # p-value = 0.8625 --> same distribution 

#t test for paired variables 
t.test(mes_dat_final_vid$mean_body_condition, mes_dat_final_vid$mean_body_condition_test_sensibility_l, paired = TRUE) # significant 
t.test(mes_dat_final_vid$mean_body_condition, mes_dat_final_vid$mean_body_condition_test_sensibility_w, paired = TRUE) # significant 
t.test(mes_dat_final_vid$mean_body_condition, mes_dat_final_vid$mean_body_condition_test_sensibility, paired = TRUE) # significant 



#----------------- effect of image resolution on BCI : correlation and PCA

#load data per individual 
load("mes_dat_final_vid.RData")

############ Correlation matrix for ACP
mes_dat_final_vid <- mes_dat_final_vid %>%
  dplyr::ungroup() %>%
  dplyr::select(c(mean_body_condition, mean_overall_quality, mean_w_nb_pixels, mean_l_nb_pixels)) 
  
#Computing correlation matrix
matrix_cor_envir <- stats::cor(mes_dat_final_vid)

#invert color palette : blue to red
palette_couleurs <- RColorBrewer::brewer.pal(n = 8, name = "RdYlBu")
palette_couleurs_inverse <- rev(palette_couleurs)

#Visualize the correlation matrix
png(here::here("outputs", "9-sensibility_test_nb_pixels", "plot_cor_variables.png"), width = 900, height = 900)
corrplot::corrplot(matrix_cor_envir, type = "upper",
                   col = palette_couleurs_inverse)
dev.off() #save plot without ggplot2

#calculate eigenvalue
eigenvalue <- eigen(matrix_cor_envir)

#Kaiser criterion
graphics::barplot(eigenvalue$values, ylab = "eigenvalue", xlab = "principal component")

#Screeplot : visualize eigenvalues
PCA = ade4::dudi.pca(mes_dat_final_vid, scannf = FALSE, nf = 4)

png(here::here("outputs", "9-sensibility_test_nb_pixels", "plot_screeplot_eigenvalue.png"), width = 900, height = 900)
factoextra::fviz_eig(PCA, addlabels = TRUE)
dev.off()
summary(PCA) #total inertia = 12 = number of variables

#Visualize correlation circle of variables (default plot)
png(here::here("outputs", "9-sensibility_test_nb_pixels", "plot_pca_default.png"), width = 900, height = 900)
factoextra::fviz_pca_var(PCA, repel = TRUE, col.var = "black") #component 1 and 2
dev.off()

#Control variable colors using their contributions
png(here::here("outputs", "9-sensibility_test_nb_pixels", "plot_pca_contrib.png"), width = 900, height = 900)
factoextra::fviz_pca_var(PCA, col.var = "contrib",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE #avoid text overlapping
)
dev.off()

#Contributions of variables to PC1
png(here::here("outputs", "9-sensibility_test_nb_pixels", "plot_variables_contrib_axe1.png"), width = 900, height = 900)
factoextra::fviz_contrib(PCA, choice = "var", axes = 1, top = 10)
dev.off()

#Quality of representation of variables 
factoextra::fviz_cos2(PCA, choice = "var", axes = 1:2)

png(here::here("outputs", "9-sensibility_test_nb_pixels", "plot_pca.png"), width = 900, height = 900)
factoextra::fviz_pca_var(PCA, col.var = "cos2",
                         gradient.cols = c("blue", "yellow", "red"),
                         repel = TRUE)
dev.off()




