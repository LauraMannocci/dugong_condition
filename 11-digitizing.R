library(magrittr)


#-1 digitize png files

digitize::digitize(here::here("data", "digitizing", "burgess.png")) #Dugongs: https://academic.oup.com/conphys/article/1/1/cot014/362858 figure 1

digitize::digitize(here::here("data", "digitizing", "harshaw.png")) #Manatees: https://www.aquaticmammalsjournal.org/article/vol-42-iss-4-harshaw/ figure 4

#we save the digitized tabular data in csv files



#2- read digitized files

burgess <- read.csv(here::here("data", "digitizing", "burgess.csv"))
harshaw <- read.csv(here::here("data", "digitizing", "harshaw.csv"))
harshaw <- harshaw*100 #convert from m to cm
dim(burgess)
dim(harshaw)
summary(burgess)
summary(harshaw)

#extract quantiles to compare the bulk of the data
burgess %>% 
  dplyr::filter(x >= quantile(x, 0.05) & x <= quantile(x, 0.95)) %>% 
  dplyr::filter(y >= quantile(y, 0.05) & y <= quantile(y, 0.95)) -> burgess

harshaw %>% 
  dplyr::filter(x >= quantile(x, 0.05) & x <= quantile(x, 0.95)) %>% 
  dplyr::filter(y >= quantile(y, 0.05) & y <= quantile(y, 0.95)) -> harshaw



#3- fit linear regressions and plot

mod_burgess = lm(x ~ y, data = burgess)
summary(mod_burgess) 

mod_harshaw = lm(x ~ y, data = harshaw)
summary(mod_harshaw) 

#CIs
datnew <- data.frame(x = seq(150, 300, by = 10), y = seq(150, 300, by = 10)) #
ci_burgess <- predict(mod_burgess, newdata = datnew, interval = "confidence", level = 0.95)
ci_harshaw <- predict(mod_harshaw, newdata = datnew, interval = "confidence", level = 0.95)



png(here::here("outputs", "11-digitizing", "digitized_burgess_harshaw_lines.png"), width = 700, height = 700)
plot(burgess$x ~ burgess$y, col = "blue", pch = 16,  
     ylab = "Straight body length (cm)", xlab = "Umbillical girth (cm)", cex.lab = 1.5, cex.axis = 1.2)
points(harshaw$x ~ harshaw$y, col = "orange", pch = 16)
text(200, 210, "Dugongs", col = "blue", cex = 1.4)
text(200, 200, "Florida manatees", col = "orange", cex = 1.4)
abline(mod_burgess, col = "blue", lwd = 2)
lines(datnew$y, sort(ci_burgess[,2]), col = "blue", lty=2)
lines(datnew$y, ci_burgess[,3], col = "blue", lty=2)
abline(mod_harshaw, col = "orange", lwd = 2)
lines(datnew$y, ci_harshaw[,2], col = "orange", lty=2)
lines(datnew$y, ci_harshaw[,3], col = "orange", lty=2)
dev.off()

png(here::here("outputs", "11-digitizing", "digitized_burgess_harshaw.png"), width = 700, height = 700)
plot(burgess$x ~ burgess$y, col = "blue", pch = 16,  
     ylab = "Straight body length (cm)", xlab = "Umbillical girth (cm)", cex.lab = 1.5, cex.axis = 1.2)
points(harshaw$x ~ harshaw$y, col = "orange", pch = 16)
text(300, 155, "Dugongs", col = "blue", cex = 1.4)
text(300, 145, "Florida manatees", col = "orange", cex = 1.4)
dev.off()


#4- test differences between slopes
#Examine the ANOVA p-value from the interaction of y by Species, then compare the slopes

burgess$species <- "dugong" 
harshaw$species <- "manatee"
dat <- rbind(burgess, harshaw)

mod <- lm(x ~ y*factor(species), data = dat)
anova(mod) #pval of y:species is 0.197 

# Obtain slopes
mod$coefficients
lst <- emmeans::lstrends(mod, "species", var = "y")
lst

# Compare slopes using t test
#https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
pairs(lst, type = "response") #pvalue 0.1970 -> not signif different


