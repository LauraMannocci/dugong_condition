rm(list=ls())
#load pipe operator
library(magrittr)



#read and append all measurement tables -----------------------------------------------------------------------------

#define empty list
list <- list()

#list all csv files in all subdirectories of "social_media" directory
filenames <- list.files(here::here("data/social_media"), pattern = "csv", recursive = TRUE)

#******loop on files******
for (i in 1:length(filenames)) {
  
  print("**********************************************************************")
  print(filenames[i])
  print(i)
  #read data from csv each file from that directory
  dat <- readr::read_delim(here::here("data/social_media", filenames[i]))
  
  
  #add column relation female/calf
  
  if ("female_calf_couple" %in% colnames(dat)) {
    dat <- dat %>% 
      #modify column (remove last characters)
      dplyr::mutate(female_calf_couple = substr(female_calf_couple, 1, nchar(female_calf_couple)-2)) %>% 
      #add column female_calf_couple_image_name
      dplyr::mutate(female_calf_couple_image_name = sub(".jpeg.*", "", female_calf_couple))  
  
   } else { 
     dat <- dat %>%
       #add column female_calf_couple
       dplyr::mutate(female_calf_couple = NA) %>% 
       #add column female_calf_couple_image_name
       dplyr::mutate(female_calf_couple_image_name = NA) 
       
   }
  
  #clean data
  dat %>% 
    #add columns
    dplyr::mutate(image_name = sub(".jpeg.*", "", Label))  %>%   # extract characters before pattern ".jpeg"
    dplyr::mutate(video_name = dplyr::case_when(grepl("mov", Label) == TRUE ~ sub("mov.*", "", Label),  # extract characters before pattern "mov" or "mp4" 
                                                TRUE ~ sub("mp4.*", "", Label))) %>%
    dplyr::mutate(video_name_long = dplyr::case_when(grepl("mov", Label) == TRUE ~ paste0(video_name, ".mov"),  # column added to later join with video table
                                                     TRUE ~ paste0(video_name, ".mp4"))) %>%    
    dplyr::mutate(indiv_code = sub(".*:", "", Label)) %>%  #extract character after ":"
    dplyr::mutate(indiv_number = sub("_.*", "", indiv_code)) %>%  #extract character before "_"
    dplyr::mutate(indiv_replicate = gsub(".*_(.+)_.*", "\\1", indiv_code)) %>%  #extract character in between the two "_"
    dplyr::mutate(type_measure = stringr::str_sub(indiv_code, - 1, - 1)) %>%   # extract last character
    #remove unwanted columns
    dplyr::select( -1, -Area, -Angle, -X, -Y) %>% 
    #rename columns
    dplyr::rename_with(tolower) %>% #convert all column names to lower case
    dplyr::rename(tidyselect::any_of(c(resolution = "resolution ", partial = "partial "))) %>%  #eliminate space in two columns only if they exist
    dplyr::rename(nb_pixels = length) %>% 
    #correct type_individual
    dplyr::mutate(type_individual = dplyr::case_when(type_individual == "cafl" ~ "calf",
                                                     type_individual == "claf" ~ "calf",
                                                     type_individual == "calf " ~ "calf",
                                                     type_individual == "female " ~ "female",
                                                     type_individual == "unidentified " ~ "unidentified",
                                                     TRUE ~ type_individual)) %>% 
    
    #reorder colums
    dplyr::select(video_name_long, image_name, label, indiv_number, indiv_replicate, type_measure, nb_pixels, resolution, contrast, distortion, partial, angle_quality, type_individual, female_calf_couple, female_calf_couple_image_name) -> dat_new
  
  list[[i]] <- dat_new # add data to the predefined empty list
  
  }


#append all lists to obtain a dataframe and make final processings
mes_dat_final <- dplyr::bind_rows(list) %>% 
  #solve problem with Aldabra (we used an image series rather than video)
  dplyr::mutate(video_name_long = dplyr::case_when(grepl("mid_east_lagoon_Flight", video_name_long) == TRUE ~ "mid_east_lagoon_Flight_04_00083.JPG, mid_east_lagoon_Flight_04_00084.JPG, mid_east_lagoon_Flight_04_00085.JPG",
                                                   TRUE ~ video_name_long)) %>% 
  dplyr::mutate(image_name = dplyr::case_when(grepl("mid_east_lagoon_Flight_04_00083", image_name) == TRUE ~ "mid_east_lagoon_Flight_04_00083.JPG",
                                              grepl("mid_east_lagoon_Flight_04_00084", image_name) == TRUE ~ "mid_east_lagoon_Flight_04_00084.JPG",
                                              grepl("mid_east_lagoon_Flight_04_00085", image_name) == TRUE ~ "mid_east_lagoon_Flight_04_00085.JPG",
                                              TRUE ~ image_name))



                                                          

#prepare measurements table per image (1 row -> 1 replicate)-----------------------------------------------------------------------------

mes_dat_final_img <- mes_dat_final %>% 
  #pivot wide (https://tidyr.tidyverse.org/articles/pivot.html) to get l_nb_pixels and w_nb_pixels columns  
  tidyr::pivot_wider(names_from = type_measure, 
                     values_from = nb_pixels) %>% 
  #rename_columns
  dplyr::rename(l_nb_pixels = l,
                w_nb_pixels = w)  %>% 
  #get one length and one width measurement per image and individual 
  dplyr::group_by(video_name_long, image_name, indiv_number, indiv_replicate, resolution, contrast, distortion, partial, angle_quality, type_individual, female_calf_couple, female_calf_couple_image_name) %>%
  #summarize (after this step the number of rows of the dataframe is divided by 2)
  dplyr::summarise(l_nb_pixels = mean(l_nb_pixels, na.rm = TRUE), # here we use means as convenience functions to drop NAs, no real mean is calculated
                   w_nb_pixels = mean(w_nb_pixels, na.rm = TRUE)) %>% 
  #calculate body condition following Ramos et al 2022 
  dplyr::mutate(body_condition = (w_nb_pixels * pi) / l_nb_pixels) %>% 
  #calculate quality index following Landeo et al 2020 (the lowest the better)
  #this is body condition per individual and per image/replicate
  #remove individuals with body condition > 1 (bad angle of measurements)
  dplyr::filter(!(video_name_long == "BRUT_2.mp4" & indiv_number == "3")) %>% #remove individual 3 of BRUT_2.mp4
  dplyr::filter(!(video_name_long == "ClipDugong1_GH034196.mp4" & indiv_number == "2")) %>% #remove individual 2 of ClipDugong1_GH034196.mp4
  #calculate dugong area (the width measurement does not encompass the whole tail so we increase the width measurement by 20 % to get a more realistic measure of dugong area)
  dplyr::mutate(dugong_area_nb_pixels = round((1.20 * w_nb_pixels) * l_nb_pixels)) %>% 
  #calculate overall quality
  dplyr::mutate(overall_quality = resolution + contrast + distortion + partial + angle_quality) 






#calculate proportions calf / female base on column female_calf_couple-----------------------------------------------------------------------------


mes_dat_final_img <- mes_dat_final_img %>%
  #add back label column
  dplyr::mutate(label = dplyr::case_when(image_name %in% c("mid_east_lagoon_Flight_04_00083.JPG", "mid_east_lagoon_Flight_04_00084.JPG", "mid_east_lagoon_Flight_04_00085.JPG") ~ paste0(image_name, ":", indiv_number, "_", indiv_replicate),
                                         TRUE ~ paste0(image_name, ".jpeg:", indiv_number, "_", indiv_replicate)))


#add column l_nb_pixels_corresponding to the other member of the couple (cannot be done in dplyr)
mes_dat_final_img$l_nb_pixels_female_calf_couple <- sapply(seq_len(nrow(mes_dat_final_img)), function(x) {
  unique(mes_dat_final_img$l_nb_pixels[mes_dat_final_img$label == mes_dat_final_img$female_calf_couple[x]])
})
# attention: some female_calf_couple do not correspond to existing labels because the female has been deleted before (bad angle for measurement)


mes_dat_final_img <- mes_dat_final_img %>%
  #calculate length proportion  
  dplyr::mutate(young_female_length_percent = dplyr::case_when(is.na(l_nb_pixels_female_calf_couple) == FALSE ~ (min(l_nb_pixels, as.numeric(l_nb_pixels_female_calf_couple)) / max(l_nb_pixels, as.numeric(l_nb_pixels_female_calf_couple))) * 100,
                                                               is.na(l_nb_pixels_female_calf_couple) == TRUE ~ NA_real_)) %>% #  smallest measure on numerator / largest measure on denominator
  #From Marsh et al. 1984 determine calf or juvenile (> 2.5 m mature female ; between 1 - 1.3 m calf ; between 1.3 - 2.2 m juvenile ; between 2.2 - 2.5 uncertain)
  dplyr::mutate(young_identification = dplyr::case_when(young_female_length_percent < 100 * (1.3 / 2.5)  ~ "calf", 
                                                        young_female_length_percent >= 100 * (1.3 / 2.5) & young_female_length_percent < 100 * (2.2 / 2.5) ~ "juvenile",
                                                        young_female_length_percent >= 100 * (2.2 / 2.5) & young_female_length_percent <= 100 ~ "uncertain",
                                                        TRUE ~ NA_character_)) %>% 
  dplyr::mutate(young_identification = dplyr::case_when(type_individual != "calf" ~ NA_character_, #treat all none calf cases 
                                                        TRUE ~ young_identification)) %>% 
  dplyr::mutate(young_identification = dplyr::case_when(type_individual == "calf" &  is.na(young_identification) ~ "juvenile", #treat all calf with NA in young_identification
                                                        TRUE ~ young_identification)) %>% 
  #type_individual_refined
  dplyr::mutate(type_individual_refined = dplyr::case_when(type_individual == "female" ~ "adult_female", 
                                                           type_individual == "male" ~ "adult_male", 
                                                           type_individual == "unidentified" ~ "unidentified",
                                                           young_identification == "juvenile" ~ "juvenile",
                                                           young_identification == "calf" ~ "calf",
                                                           young_identification == "uncertain" ~ "unidentified",
                                                           TRUE ~ NA_character_)) %>% 
  #type_stage (adult VS young)
  dplyr::mutate(type_stage = dplyr::case_when(type_individual_refined %in% c("female", "male", "unidentified") ~ "adult",
                                              type_individual_refined %in% c("calf", "juvenile") ~ "juvenile", 
                                              TRUE ~ NA_character_)) 
  







#explore measurements table per image TODO-----------------------------------------------------------------------------

#calculate total number of measurements before cleaning 
nrow(mes_dat_final_img) #1237

#explore overall_quality -> we will delete 17 18 20 22 
table(mes_dat_final_img$overall_quality)
barplot(table(mes_dat_final_img$overall_quality))
(  11  + 6 +  4 +  16 ) / nrow(mes_dat_final_img) 

#number of unique image before cleaning 
length(unique(mes_dat_final_img$image_name)) #949






#clean measurements table per image (1 row -> 1 replicate)-----------------------------------------------------------------------------

mes_dat_final_img <- mes_dat_final_img %>% 
  #filter on overall_quality
  dplyr::filter(overall_quality < 17)

#number of measurements after cleaning 
nrow(mes_dat_final_img) #1200

#number of unique image after cleaning
length(unique(mes_dat_final_img$image_name)) #926 -> 23 unique images of low quality removed, corresponding to 0.02423604 %

#quality of images after cleaning
mean(mes_dat_final_img$overall_quality) # 9.865
sd(mes_dat_final_img$overall_quality) #2.779067

#statistics per measured individual across replicates 
mes_dat_final_img %>% 
  dplyr::group_by(video_name_long, indiv_number) %>% 
  dplyr::summarise(mean_bc = mean(body_condition),
                   sd_bc = sd(body_condition),
                   cv_bc = sd_bc / mean_bc,
                   nb_indiv_replicate = dplyr::n()) -> stats

mean(stats$cv_bc) #0.0460723

#total number of videos
length(unique(mes_dat_final_img$video_name_long)) #131

save(mes_dat_final_img, file = "mes_dat_final_img.RData")
#load("mes_dat_final_img.RData")



#prepare measurements table per video (1 row -> 1 individual with mean/sd measuremnets)-----------------------------------------------------------------------------

mes_dat_final_vid <- mes_dat_final_img %>% 
  dplyr::group_by(video_name_long, indiv_number, type_individual) %>%
  #summarize (after this step the number of rows of the dataframe is decreased)
  dplyr::summarise(nb_indiv_replicate = dplyr::n(),
                   mean_young_female_length_percent = mean(young_female_length_percent, na.rm = TRUE), 
                   mean_body_condition = mean(body_condition),
                   sd_body_condition = sd(body_condition),
                   mean_dugong_area_nb_pixels = mean(dugong_area_nb_pixels),
                   sd_dugong_area_nb_pixels = sd(dugong_area_nb_pixels),
                   mean_resolution = mean(resolution),
                   mean_contrast = mean(contrast),
                   mean_distortion = mean(distortion),
                   mean_partial = mean(partial),
                   mean_angle_quality = mean(angle_quality),
                   mean_overall_quality = mean(overall_quality),
                   sd_resolution = sd(resolution),
                   sd_contrast = sd(contrast),
                   sd_distortion = sd(distortion),
                   sd_partial = sd(partial),
                   sd_angle_quality = sd(angle_quality),
                   sd_overall_quality = sd(overall_quality),
                   mean_l_nb_pixels = mean(l_nb_pixels),
                   mean_w_nb_pixels = mean(w_nb_pixels)) %>% 
  #From Marsh et al. 1984 determine calf or juvenile (> 2.5 m mature female ; between 1 - 1.3 m calf ; between 1.3 - 2.2 m juvenile ; between 2.2 - 2.5 uncertain)
  dplyr::mutate(young_identification = dplyr::case_when(mean_young_female_length_percent < 100 * (1.3 / 2.5)  ~ "calf", 
                                                        mean_young_female_length_percent >= 100 * (1.3 / 2.5) & mean_young_female_length_percent < 100 * (2.2 / 2.5) ~ "juvenile",
                                                        mean_young_female_length_percent >= 100 * (2.2 / 2.5) & mean_young_female_length_percent <= 100 ~ "uncertain",
                                                        TRUE ~ NA_character_)) %>% 
  dplyr::mutate(young_identification = dplyr::case_when(type_individual != "calf" ~ NA_character_,
                                                        TRUE ~ young_identification)) 
#this is mean/sd body condition per individual across all images/replicates (ie per video)
#4 calves with NA in young_indentification due to absence of measured female


nrow(mes_dat_final_vid) #284 indiv
summary(mes_dat_final_vid$nb_indiv_replicate) #range 2-10
mean(mes_dat_final_vid$nb_indiv_replicate) # 4.225352
sd(mes_dat_final_vid$nb_indiv_replicate) # 0.9939347


#save for sensibility test
save(mes_dat_final_vid, file = "mes_dat_final_vid.RData")

table(mes_dat_final_vid$young_identification, useNA = "always")
table(mes_dat_final_vid$type_individual, useNA = "always")




#update measurements table per video considering cases with same_individual_measured-----------------------------------------------------------------------------
#ie when the same individual is measured in other videos

load("vid_dat_final.RData")


#select columns of vid_dat_final for convenience
vid_dat_final <- vid_dat_final %>% 
  dplyr::select("social_media", "video_owner", "owner_type", "source", "publication_date", "collection_date" ,
                "overall_location", "country", "most_precise_location", "approx_latitude" , "approx_longitude", 
                "localisation_precision", "number_individual_measured", "video_width", "video_height", 
                "same_individual_measured", "same_individual_measured_file", "video_name_long", "collection_year", 
                "publication_year", "collection_month", "publication_month", "iucn_status", "province_marsh", 
                "province_admin", "lat_province_admin", "lon_province_admin") 


#join mes_dat_final_vid with vid_dat_final based on column video_name_long
mes_dat_final_vid_join <- dplyr::left_join(mes_dat_final_vid, vid_dat_final, by = "video_name_long") # IMPORTANT use left_join to keep all observations in x (mes_dat_final_vid)

#check that numbers of rows are the same
nrow(mes_dat_final_vid_join) == nrow(mes_dat_final_vid)

#change same_individual_measured from "yes" to "no" in a few cases where the individuals measured were in fact not the same
mes_dat_final_vid_join <- mes_dat_final_vid_join %>% 
  dplyr::mutate(same_individual_measured = dplyr::case_when(video_name_long %in% c("rhyssmclean0.mp4", "rhyssmclean1.mp4", "BastianSaputra1.mp4", "dronesharkapp3.mp4", "geoffaquino2_1.mp4", "rebpentti1_1.mp4") ~ "no",
                                                            TRUE ~ same_individual_measured)) %>% 
  #percent_dugong_area (proportion of dugong in image)
  dplyr::mutate(image_area_nb_pixels = video_width * video_height) %>% 
  dplyr::mutate(percent_dugong_area = round(100 *(mean_dugong_area_nb_pixels / image_area_nb_pixels), 3))   #round to 3 decimals

range(mes_dat_final_vid_join$percent_dugong_area) #0.002 TO 35.145 %  


#process cases when same_individual_measured is yes (ie the same individaul is measured in different videos)
mes_dat_final_vid_join_yes <- mes_dat_final_vid_join %>% 
  dplyr::filter(same_individual_measured == "yes") %>% 
  #create short video name (extract first 10 characters) to differenciate the series of videos
  dplyr::mutate(video_name_short = substr(video_name_long, 1, 10)) %>%   
  #group by individual type (this assumes no more than one individual of same type is measured in each series of videos)
  dplyr::group_by(type_individual, video_name_short, #group by these 2 columns and all other columns of mes_dat_final_vid_join
                  social_media, video_owner, owner_type, source, publication_date, collection_date ,
                  overall_location, country, most_precise_location, approx_latitude ,            
                  approx_longitude, localisation_precision, number_individual_measured, 
                  video_width, video_height, same_individual_measured, 
                  collection_year, publication_year, collection_month, publication_month, iucn_status,
                  province_marsh, province_admin, lat_province_admin, lon_province_admin, 
                  young_identification) %>% 
  dplyr::summarize(video_name_long = dplyr::first(video_name_long), #attention: only the first video name is kept by default
                   indiv_number = dplyr::first(indiv_number),  #attention: only the first video name is kept by default
                   nb_indiv_replicate = sum(nb_indiv_replicate), #sum the number of replicates
                   mean_body_condition = mean(mean_body_condition), #make the mean for all other columns
                   sd_body_condition = mean(sd_body_condition),
                   mean_resolution = mean(mean_resolution),
                   mean_contrast = mean(mean_contrast),
                   mean_distortion = mean(mean_distortion),
                   mean_partial = mean(mean_partial),
                   mean_angle_quality = mean(mean_angle_quality),
                   mean_overall_quality = mean(mean_overall_quality),
                   sd_resolution = mean(sd_resolution),
                   sd_contrast = mean(sd_contrast),
                   sd_distortion = mean(sd_distortion),
                   sd_partial = mean(sd_partial),
                   sd_angle_quality = mean(sd_angle_quality),
                   sd_overall_quality = mean(sd_overall_quality)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-video_name_short)

#process cases when same_individual_measured is no or NA
mes_dat_final_vid_join_no <- mes_dat_final_vid_join %>% 
  dplyr::filter(same_individual_measured == "no" | is.na(same_individual_measured) == TRUE) %>% 
  #select same columns as mes_dat_final_vid_join_yes
  dplyr::select( type_individual, social_media, video_owner,                  
                 owner_type, source, publication_date, collection_date,              
                 overall_location, country, most_precise_location, approx_latitude,              
                 approx_longitude, localisation_precision, number_individual_measured, video_width,                  
                 video_height, same_individual_measured, collection_year,publication_year, collection_month,          
                 publication_month, iucn_status, province_marsh, province_admin, lat_province_admin,        
                 lon_province_admin,          
                 young_identification, video_name_long, indiv_number, nb_indiv_replicate, mean_body_condition,       
                 sd_body_condition, mean_resolution, mean_contrast, mean_distortion, mean_partial,              
                 mean_angle_quality, mean_overall_quality, sd_resolution, sd_contrast, sd_distortion,             
                 sd_partial, sd_angle_quality, sd_overall_quality)  

#append the two datasets
dat_final <- rbind(mes_dat_final_vid_join_yes, mes_dat_final_vid_join_no) 


#create new columns 
dat_final <- dat_final %>%
  #treat all calf with NA in young_identification
  dplyr::mutate(young_identification = dplyr::case_when(type_individual == "calf" &  is.na(young_identification) ~ "juvenile", 
                                                        TRUE ~ young_identification)) %>% 
  #type_individual_refined
  dplyr::mutate(type_individual_refined = dplyr::case_when(type_individual == "female" ~ "adult_female", 
                                                           type_individual == "male" ~ "adult_male", 
                                                           type_individual == "unidentified" ~ "unidentified",
                                                           young_identification == "juvenile" ~ "juvenile",
                                                           young_identification == "calf" ~ "calf",
                                                           young_identification == "uncertain" ~ "unidentified",
                                                           TRUE ~ NA_character_)) %>% 
  #type_stage (adult VS young)
  dplyr::mutate(type_stage = dplyr::case_when(type_individual_refined %in% c("adult_female", "adult_male", "unidentified") ~ "adult",
                                              type_individual_refined %in% c("calf", "juvenile") ~ "juvenile", 
                                              TRUE ~ NA_character_)) %>% 
  #most_precise_date 
  dplyr::mutate(most_precise_date = dplyr::case_when(!is.na(collection_date) ~ collection_date, 
                                                     TRUE ~ publication_date)) %>% 
  #genetic_cluster from Srinivas et al. 
  dplyr::mutate(genetic_cluster = dplyr::case_when(
    country %in% c("Mayotte", "Mozambique", "Seychelles") ~ "swio",
    country %in% c("United Arab Emirates","Saudi Arabia", "Qatar") ~ "nwio", 
    country %in% c("India", "Sri Lanka") ~ "sa",
    country %in% c("Thailand", "Philippines", "Indonesia", "Timor-Leste", "Malaysia") ~ "sea",
    country %in% c("Australia", "New Caledonia", "Papua New Guinea", "Palau", "Vanuatu") ~ "pac", 
    TRUE ~ NA )) %>% 
  dplyr::mutate(genetic_cluster = as.factor(genetic_cluster)) %>% 
  dplyr::mutate(number_individual = 1) %>% 
  #add column type_stage_refined 
  dplyr::mutate(type_stage_refined = dplyr::case_when(
    type_individual_refined == "adult_female" ~ "adult_female",
    type_individual_refined %in% c("unidentified", "adult_male") ~ "unidentified",
    type_individual_refined %in% c("juvenile", "calf") ~ "juvenile")) 


table(dat_final$genetic_cluster, useNA = "always")
table(dat_final$type_individual_refined, useNA = "always")
table(dat_final$type_stage, useNA = "always")



save(dat_final, file = "dat_final.RData")
load("dat_final.RData")

#export dat_final to csv file 
#readr::write_csv(dat_final, file = here::here("dat_final.csv"))



summary(dat_final)
#there are 3 NAs in columns beginning by sd due to the removal of replicates with overall_quality >=17 

#calculate total number of individuals measured
nrow(dat_final) #272

#calculate mean number of replicates by individual (+standard error)
mean(dat_final$nb_indiv_replicate) #4.411765
sd(dat_final$nb_indiv_replicate) #  1.47536
range(dat_final$nb_indiv_replicate) #2 to 15 replicates
mean(dat_final$mean_body_condition) # 0.7537605
summary(dat_final$mean_body_condition) # range 0.537 - 0.974


#check mean and sd BC per country 
dat_final_country <- dat_final %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(mean_condition = mean(mean_body_condition, na.rm = TRUE),
            sd_condition = sd(mean_body_condition, na.rm = TRUE)) 

#check mean and sd BC per iucn_status 
dat_final_iucn_status <- dat_final %>% 
  dplyr::group_by(iucn_status) %>% 
  dplyr::summarise(mean_condition = mean(mean_body_condition, na.rm = TRUE),
                   sd_condition = sd(mean_body_condition, na.rm = TRUE)) 

#check mean and sd BC per type_stage_refined 
dat_final_type_stage <- dat_final %>% 
  dplyr::group_by(type_stage_refined) %>% 
  dplyr::summarise(mean_condition = mean(mean_body_condition, na.rm = TRUE),
                   sd_condition = sd(mean_body_condition, na.rm = TRUE)) 
  
#check number of individual per owner_type 
dat_final_owner <- dat_final %>% 
  dplyr::group_by(owner_type) %>% 
  dplyr::count() %>% 
  dplyr::mutate(percent = n / sum(n) * 100)


#check number of individual per source
dat_final_source <- dat_final %>% 
  dplyr::group_by(source) %>% 
  dplyr::count()

#check number of individual per social_media
dat_final_social_media <- dat_final %>% 
  dplyr::group_by(social_media) %>% 
  dplyr::count()

#check number of individual per country
dat_final_country <- dat_final %>% 
  dplyr::group_by(country) %>% 
  dplyr::count()



#explore measurements table per indiv -----------------------------------------------------------------------------

#mean/sd overall body condition
summary(dat_final$mean_body_condition) # range  0.5373  - 0.9734 
mean(dat_final$mean_body_condition) #  0.7537605
sd(dat_final$mean_body_condition) # 0.07988885
sd(dat_final$mean_body_condition) / mean(dat_final$mean_body_condition) # 0.105987

#mean overall quality
mean(dat_final$mean_overall_quality) # 9.970895
sd(dat_final$mean_overall_quality) # 2.682581


#number of individuals from social media and scientist per country
count_data <- dat_final %>%
  dplyr::group_by(country, social_media) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::ungroup()

ggplot2::ggplot(count_data, ggplot2::aes(x = reorder(country, -count), y = count, fill = social_media)) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::labs(x = "", y = "Number of individuals") +
  ggplot2::theme_light() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_number_individuals_from_social_media_VS_surveys_per_country.png"), width = 7, height = 5)


#visualization percent of individuals per country
percent_individual_per_social_media <- dat_final %>% 
  dplyr::group_by(country, social_media) %>% 
  dplyr::summarise(count = dplyr::n()) %>% 
  dplyr::mutate(percentage = count/sum(count) * 100)

ggplot2::ggplot(percent_individual_per_social_media, ggplot2::aes(fill = social_media, x = country, y = percentage)) +
  ggplot2::geom_bar(stat = "identity", position = "fill") +
  ggplot2::labs(x = "", y = "Percentage of individuals", fill = "Individuals from\nsocial media") +
  ggplot2::scale_fill_brewer(palette = "Oranges") +
  ggplot2::geom_text(ggplot2::aes(label = paste0(round(percentage), "%")), 
                     position = ggplot2::position_fill(vjust = 0.5), 
                     color = "black", size = 3, fontface = "bold") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_percent_individuals_from_social_media_VS_surveys_per_country.png"), width = 9, height = 7)


#visualization of mean_overall_quality of images from social media and survey
ggplot2::ggplot(dat_final, ggplot2::aes(x = social_media, y = mean_overall_quality)) +
  ggplot2::geom_boxplot(fill = "grey80") +
  ggplot2::labs(x = "Videos from social media", y = "Mean overall quality image") +
  ggplot2::theme_light() 

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "boxplot_mean_overall_quality_images_from_social_media_VS_surveys_per_country.png"), width = 9, height = 7)


#visualization of resolution of video from social media and survey
dat_final <- dat_final %>% 
  dplyr::mutate(video_resolution = video_width * video_height)

ggplot2::ggplot(dat_final, ggplot2::aes(x = social_media, y = video_resolution)) +
  ggplot2::geom_boxplot(fill = "grey80") +
  ggplot2::labs(x = "Videos from social media", y = "Resolution of videos") +
  ggplot2::theme_light() 

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "boxplot_resolution_of_videos_from_social_media_VS_surveys_per_country.png"), width = 9, height = 7)



#########mean_overall_body_condition 
ggplot2::ggplot(dat_final, ggplot2::aes(x = " ", y = mean_body_condition)) + 
  ggplot2::labs(x= "") +
  ggplot2::labs(y= "Mean overall body condition") +
  ggplot2::scale_y_continuous(breaks = seq(0.5, 1, by = 0.05)) +
  ggplot2::geom_boxplot()

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_overall_body_condition.png"), width = 7, height = 5)


#########mean_body_condition_type_individual
#count sample size per type individual
count <- as.data.frame(table(dat_final$type_individual_refined))
names(count) <- c("type_individual", "Freq")

ggplot2::ggplot() +
  ggplot2::geom_boxplot(data = dat_final, ggplot2::aes(x = type_individual_refined, y = mean_body_condition)) +
  #add sample size
  ggplot2::geom_text(data = count, ggplot2::aes(label = paste("n =", Freq), x = type_individual, y = 0.99), size = 6, vjust = 0) +
  ggplot2::labs(x= "Type individual") +
  ggplot2::labs(y= "Mean body condition")

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_per_type_individual_refined.png"), width = 7, height = 5)


#########mean_body_condition_type_stage_refined
#count sample size per type individual
count <- as.data.frame(table(dat_final$type_stage_refined))
names(count) <- c("type_individual", "Freq")

ggplot2::ggplot() +
  ggplot2::geom_boxplot(data = dat_final, ggplot2::aes(x = type_stage_refined, y = mean_body_condition)) +
  #add sample size
  ggplot2::geom_text(data = count, ggplot2::aes(label = paste("n =", Freq), x = type_individual, y = 0.99), size = 6, vjust = 0) +
  ggplot2::labs(x= "Type individual") +
  ggplot2::labs(y= "Mean body condition")

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_per_type_stage_refined.png"), width = 7, height = 5)


########mean_body_condition per genetic_cluster 
#count sample size per genetic_cluster
count <- as.data.frame(table(dat_final$genetic_cluster))
names(count) <- c("genetic_cluster", "Freq")

median_data <- dat_final %>%
  dplyr::group_by(genetic_cluster) %>%
  dplyr::summarise(median_bc = median(mean_body_condition))

ggplot2::ggplot() +
  ggplot2::geom_boxplot(data = dat_final, ggplot2::aes(x = reorder(genetic_cluster, - mean_body_condition), y = mean_body_condition), fill = "grey80") +
  #add sample size
  ggplot2::geom_text(data = count, ggplot2::aes(label = paste("n =", Freq), x = genetic_cluster, y = 0.99), size = 4, vjust = 0) +
  ggplot2::theme_light() +
  ggplot2::labs(x= "Genetic cluster") +
  ggplot2::labs(y= "Mean body condition") +
  # our data as points
  ggplot2::stat_summary(data = median_data, fun = function(x) round(median(x), 4), geom = "text", 
                        ggplot2::aes(label = round(median_bc, 2), x = genetic_cluster, y = median_bc), vjust = -1, size = 4) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10)) +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 15)) 

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_per_genetic_cluster.png"), width = 6, height = 5)


#########number of individual per genetic_cluster
#count sample size per genetic_cluster
count <- as.data.frame(table(dat_final$genetic_cluster))
names(count) <- c("genetic_cluster", "Freq")

ggplot2::ggplot(dat_final, ggplot2::aes(x = forcats::fct_infreq(genetic_cluster))) + 
  ggplot2::labs(x = "Genetic cluster") +
  ggplot2::labs(y = "Count") +
  ggplot2::geom_bar() +  
  ggplot2::geom_text(data = count, ggplot2::aes(label = paste("n =", Freq), x = genetic_cluster, y = 0.99), size = 5, vjust = 0) 

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_number_individual_per_genetic_cluster.png"), width = 7, height = 5)




########mean_body_condition per most_precise_location 
#count sample size per most_precise_location
count <- as.data.frame(table(dat_final$most_precise_location))
names(count) <- c("most_precise_location", "Freq")

median_data <- dat_final %>%
  dplyr::group_by(most_precise_location) %>%
  dplyr::summarise(median_bc = median(mean_body_condition))

nb_indiv <- dat_final %>%
  dplyr::group_by(most_precise_location) %>%
  dplyr::summarise(nb_ind = n(),
                   mean_body_condition = mean_body_condition)

dat_final <- dplyr::left_join(dat_final, nb_indiv)


ggplot2::ggplot() +
  ggplot2::geom_point(data = dat_final, ggplot2::aes(x = reorder(most_precise_location, - mean_body_condition), y = mean_body_condition), alpha = 0) +
  ggplot2::geom_boxplot(data = dat_final %>%
                          dplyr::filter(nb_ind > 4), ggplot2::aes(x = reorder(most_precise_location, - mean_body_condition), y = mean_body_condition), fill = "grey80") +
  ggplot2::geom_point(data = dat_final %>%
                        dplyr::filter(nb_ind <= 4), ggplot2::aes(x = reorder(most_precise_location, - mean_body_condition), y = mean_body_condition)) +
  #add sample size
  ggplot2::geom_text(data = count, ggplot2::aes(label = paste("n =", Freq), x = most_precise_location, y = 0.99), size = 4, vjust = 0.5) +
  ggplot2::theme_light() +
  ggplot2::labs(x= "Geographic location") +
  ggplot2::labs(y= "Mean body condition") +
  # our data as points
  #ggplot2::stat_summary(data = median_data, fun = function(x) round(median(x), 4), geom = "text", 
  #                      ggplot2::aes(label = round(median_bc, 2), x = most_precise_location, y = median_bc), vjust = -1, size = 4) +
  ggplot2::theme(axis.text = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 15)) +
  ggplot2::coord_flip()

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_per_most_precise_location.png"), width = 9, height = 10)




########mean_body_condition per most_precise_location and country
#count sample size per most_precise_location
count <- as.data.frame(table(dat_final$most_precise_location))
names(count) <- c("most_precise_location", "Freq")

dat_final %>%
  #replace mtsamboro island by northern islands (Mayotte) due to sensitivity of precise geographic location
  dplyr::mutate(most_precise_location = dplyr::case_when(most_precise_location == "mtsamboro island" ~ "northern islands", 
                                                         TRUE ~ most_precise_location))  %>% 
  #correct one location name
  dplyr::mutate(most_precise_location = dplyr::case_when(most_precise_location == "Pulau Sibor" ~ "pulau Sibu", 
                                                         TRUE ~ most_precise_location))  -> dat_final_sensitive
nb_indiv <- dat_final_sensitive %>% 
  dplyr::group_by(most_precise_location) %>%
  dplyr::summarise(nb_ind = dplyr::n(),
                   mean_body_condition = mean_body_condition)

dat_final2 <- dplyr::left_join(dat_final_sensitive, nb_indiv)

ggplot2::ggplot() +
  ggplot2::geom_point(data = dat_final2, ggplot2::aes(x = most_precise_location, y = mean_body_condition), alpha = 0) +
  ggplot2::geom_boxplot(data = dat_final2 %>%
                          dplyr::filter(nb_ind > 4), ggplot2::aes(x = most_precise_location, y = mean_body_condition), fill = "grey80") +
  ggplot2::geom_point(data = dat_final2 %>%
                        dplyr::filter(nb_ind <= 4), ggplot2::aes(x = most_precise_location, y = mean_body_condition)) +
  ggplot2::facet_wrap(~country, scales = "free_x", strip.position = "top") +
  ggplot2::theme_light() +
  ggplot2::labs(x= "") +
  ggplot2::labs(y= "Mean body condition") +
  ggplot2::theme(axis.text = ggplot2::element_text(size = 8)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 55, hjust = 1)) +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 15)) 

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_per_most_precise_location_country.png"), width = 9, height = 10)



#variations of BCI between countries
dat_final %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(min = min(mean_body_condition),
                   max = max(mean_body_condition),
                   nb_ind = dplyr::n(),
                   mean = mean(mean_body_condition)) -> dat_countries

max(dat_countries$mean) - min(dat_countries$mean) # 0.1871758

#variations of BCI between locations of each country
dat_final %>% 
  dplyr::group_by(country, most_precise_location) %>% 
  dplyr::summarise(min = min(mean_body_condition),
                   max = max(mean_body_condition),
                   nb_ind = dplyr::n(),
                   mean = mean(mean_body_condition)) -> dat_locations

for (i in unique(dat_locations$country)){
  print(paste("-----------", i))
  diff <- max(dat_locations$mean[dat_locations$country == i]) - min(dat_locations$mean[dat_locations$country == i])  
  print(diff)
}
#these countries have a single location: Malaysia, Mayotte, PNG, Seychelles, Sri Lanka, Vanuatu




#########number of individual per type_individual 
#count sample size of individual per type_individual_refined
count <- as.data.frame(table(dat_final$type_individual_refined))
names(count) <- c("type_individual_refined", "Freq")

ggplot2::ggplot(dat_final, ggplot2::aes(x = forcats::fct_infreq(type_individual_refined))) + 
  ggplot2::labs(x= "Type individual") +
  ggplot2::labs(y= "Count") +
  ggplot2::scale_y_continuous(breaks = seq(0, 140, by = 20)) +
  ggplot2::geom_bar() +  
  ggplot2::geom_text(data = count, ggplot2::aes(label = paste("n =", Freq), x = type_individual_refined, y = 0.99), size = 5, vjust = 0) 

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_number_type_individual_refined.png"), width = 7, height = 5)



#########percentage of owner_type per country
dat_final_owner_type <- dat_final %>% 
  dplyr::group_by(country, owner_type) %>% 
  dplyr::summarise(count = dplyr::n()) %>% 
  dplyr::mutate(perc = count/sum(count))

barplot = ggplot2::ggplot(data = dat_final_owner_type, ggplot2::aes(fill = owner_type, x = country, y = perc*100)) +
  ggplot2::geom_bar(stat = "identity", colour = "white") +
  ggplot2::scale_fill_viridis_d() +
  ggplot2::labs(x= "", y = "Percent", fill = "Owner type") +
  ggplot2::coord_flip()

barplot
ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_percentage_owner_type_per_country.png"), barplot, width = 7, height = 5)


#########percentage of type_stage_refined 
dat_final_type_stage_refined <- dat_final %>%
  dplyr::mutate(type_stage_refined = forcats::fct_infreq(type_stage_refined)) %>%
  dplyr::count(type_stage_refined) %>%
  dplyr::mutate(pct = round(n / sum(n) * 100, 1))

barplot = ggplot2::ggplot(dat_final_type_stage_refined, ggplot2::aes(x = type_stage_refined, y = pct)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = "", y = "Percent")


barplot
ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_percent_type_stage_refined.png"), barplot, width = 8, height = 7)


#########percentage of owner_type 
dat_final_owner_type <- dat_final %>%
  dplyr::mutate(owner_type = forcats::fct_infreq(owner_type)) %>%
  dplyr::count(owner_type) %>%
  dplyr::mutate(pct = n / sum(n) * 100)

barplot = ggplot2::ggplot(dat_final_owner_type, ggplot2::aes(x = owner_type, y = pct)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::labs(x = "", y = "Percent") +
  ggplot2::coord_flip()

barplot
ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_percent_owner_type.png"), barplot, width = 8, height = 7)


#########number of owner_type 
barplot = ggplot2::ggplot(dat_final, ggplot2::aes(x = forcats::fct_infreq(owner_type))) +
  ggplot2::geom_bar() +
  ggplot2::labs(x = "") +
  ggplot2::coord_flip()

barplot
ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_number_owner_type.png"), barplot, width = 8, height = 7)



#########percentage of source per country
dat_final_source <- dat_final %>% 
  dplyr::group_by(country, source) %>% 
  dplyr::summarise(count = dplyr::n()) %>% 
  dplyr::mutate(perc = count/sum(count))

barplot = ggplot2::ggplot(data = dat_final_source, ggplot2::aes(fill = source, x = country, y = perc*100)) +
  ggplot2::geom_bar(stat = "identity", colour = "white") +
  ggplot2::scale_fill_viridis_d() +
  ggplot2::labs(x= "", y = "Percent", fill = "Source") +
  ggplot2::coord_flip()

barplot
ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_percentage_source_per_country.png"), barplot, width = 7, height = 5)



########number_unique_individuals per country 
dat_final_ind <- dat_final %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(number_individual = sum(number_individual)) %>% 
  dplyr::mutate(percent = number_individual / sum(number_individual) * 100)

barplot = ggplot2::ggplot(data = dat_final_ind, ggplot2::aes(x = reorder(country, number_individual), y = number_individual)) +
  ggplot2::geom_col() + 
  ggplot2::labs(x = "") + 
  ggplot2::coord_flip() +
  ggplot2::ylab("Number unique individuals") +
  ggplot2::scale_y_continuous(breaks = seq(0, 220, by = 5))

barplot
ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_number_indivuduals_per_country.png"), barplot, width = 7, height = 5)



########percent of number_unique_individuals per country 
barplot = ggplot2::ggplot(dat_final_ind, ggplot2::aes(x = reorder(country, percent), y = percent)) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "", y = "Percent unique individuals")

barplot
ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_percent_indivuduals_per_country.png"), barplot, width = 7, height = 5)



##########explore body condition per location, country, individual_type etc.------------------------------------------

##########mean body condition per location
dat_final_bc_per_location <- dat_final %>% 
  dplyr::group_by(most_precise_location) %>%
  dplyr::summarise(mean_body_condition)

ggplot2::ggplot(dat_final_bc_per_location, ggplot2::aes(x = most_precise_location, y = mean_body_condition)) + 
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7)) +
  ggplot2::ylab("Mean body condition") +
  ggplot2::xlab("") +
  ggplot2::geom_boxplot()

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_per_location.png"), width = 9, height = 5)



##########mean body condition adult vs calf per country
dat_final_bc_adult_vs_calf <- dat_final %>% 
  dplyr::group_by(country, type_stage) %>%
  dplyr::summarise(mean_body_condition)

ggplot2::ggplot(dat_final_bc_adult_vs_calf, ggplot2::aes(x = "", y = mean_body_condition, fill = factor(type_stage))) + 
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7)) +
  ggplot2::facet_wrap(~ country) +
  ggplot2::ylab("Mean body condition") +
  ggplot2::xlab("") +
  ggplot2::labs(fill = "Type stage") +
  ggplot2::scale_fill_brewer(palette = "GnBu") +
  ggplot2::geom_boxplot() 

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_per_country_adult_vs_calf.png"), width = 9, height = 5)


##########mean body condition per type stage per province admin
dat_final_bc_type_stage_per_admin <- dat_final %>% 
  dplyr::group_by(province_admin, type_stage) %>%
  dplyr::summarise(mean_body_condition)

ggplot2::ggplot(dat_final_bc_type_stage_per_admin, ggplot2::aes(x = "", y = mean_body_condition, fill = factor(type_stage))) + 
  ggplot2::facet_wrap(~province_admin) +
  ggplot2::ylab("Mean body condition") +
  ggplot2::scale_fill_brewer(palette = "GnBu") +
  ggplot2::xlab("") +
  ggplot2::labs(fill = "Type stage") +
  ggplot2::geom_boxplot()

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_per_type_stage_per_admin.png"), width = 9, height = 7)



##########mean body condition per type stage per province marsh
dat_final_bc_type_stage_per_marsh <- dat_final %>% 
  dplyr::group_by(province_marsh, type_stage) %>%
  dplyr::summarise(mean_body_condition)

ggplot2::ggplot(dat_final_bc_type_stage_per_marsh, ggplot2::aes(x = "", y = mean_body_condition, fill = factor(type_stage))) + 
  ggplot2::facet_wrap(~province_marsh) +
  ggplot2::ylab("Mean body condition") +
  ggplot2::scale_fill_brewer(palette = "GnBu") +
  ggplot2::xlab("") +
  ggplot2::labs(fill = "Type stage") +
  ggplot2::geom_boxplot()

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_per_type_stage_per_marsh.png"), width = 9, height = 7)



##########mean body condition per county
#count sample size of individual per type_individual_refined
count <- as.data.frame(table(dat_final$country))
names(count) <- c("country", "Freq")

dat_final_bc_per_country <- dat_final %>% 
  dplyr::group_by(country) %>%
  dplyr::summarise(mean_body_condition)

ggplot2::ggplot(dat_final_bc_per_country, ggplot2::aes(x = country, y = mean_body_condition)) + 
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9)) +
  ggplot2::labs(x = "") +
  ggplot2::labs(y = "Mean body condition") +
  ggplot2::geom_boxplot() +
  ggplot2::geom_text(data = count, ggplot2::aes(label = paste("n =", Freq), x = country, y = 0.99), size = 4, vjust = 0) 

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_per_country.png"), width = 9, height = 5)


##########mean body condition per type individual per country
dat_final_bc_type_stage_per_country <- dat_final %>% 
  dplyr::group_by(country, type_stage_refined) %>%
  dplyr::summarise(mean_body_condition, nb_ind = dplyr::n())

dat_final_bc_type_stage_per_country$type_stage_refined <- factor(dat_final_bc_type_stage_per_country$type_stage_refined, levels = c("adult_female", "juvenile", "unidentified"))

ggplot2::ggplot() + 
  ggplot2::geom_point(data = dat_final_bc_type_stage_per_country, ggplot2::aes(x = "", y = mean_body_condition, color = type_stage_refined), show.legend = FALSE, alpha = 0) +
  ggplot2::facet_wrap(~country, scales = "free_x") +
  ggplot2::ylab("Mean body condition") +
  ggplot2::xlab("") +
  ggplot2::labs(fill = "Type individual", color = "") +
  ggplot2::geom_boxplot(data = dat_final_bc_type_stage_per_country %>%
                          dplyr::filter(nb_ind > 4) %>%
                          dplyr::filter(country %in% c("Malaysia", "Mozambique", "Palau", "Saudi Arabia", "Thailand")), 
                        position = ggplot2::position_dodge(width = 1),
                        width = 0.3,
                        ggplot2::aes(x = "", y = mean_body_condition, fill = type_stage_refined)) + 
  ggplot2::geom_boxplot(data = dat_final_bc_type_stage_per_country %>%
                          dplyr::filter(nb_ind > 4) %>%
                          dplyr::filter(country %in% c("United Arab Emirates", "Philippines")), 
                        position = ggplot2::position_dodge(width = 1.3),
                        width = 0.5,
                        ggplot2::aes(x = "", y = mean_body_condition, fill = type_stage_refined)) + 
  ggplot2::geom_boxplot(data = dat_final_bc_type_stage_per_country %>%
                          dplyr::filter(nb_ind > 4) %>%
                          dplyr::filter(country %in% c("Malaysia", "Mozambique", "Palau", "Saudi Arabia", "Thailand", "United Arab Emirates", "Philippines") == FALSE), 
                        position = ggplot2::position_dodge(width = 1.2),
                        width = 0.8,
                        ggplot2::aes(x = "", y = mean_body_condition, fill = type_stage_refined)) + 
  ggplot2::geom_point(data = dat_final_bc_type_stage_per_country %>%
                        dplyr::filter(nb_ind <= 4),
                      position = ggplot2::position_dodge(width = 1.2),
                      show.legend = FALSE,
                      ggplot2::aes(x = "", y = mean_body_condition, color = type_stage_refined)) + 
  ggplot2::scale_fill_manual(values = c('adult_female' = 'coral1', 'juvenile' = 'aquamarine3', 'unidentified' = 'slateblue1')) +
  ggplot2::scale_color_manual(values = c('adult_female' = 'coral1', 'juvenile' = 'aquamarine3', 'unidentified' = 'slateblue1')) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white"),
                 panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
                 panel.grid.major.y = ggplot2::element_line(color = "gray50", linetype = "dashed", linewidth = 0.5))
# ggplot2::theme_minimal()


ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_per_type_stage_refined_per_country.png"), width = 8, height = 5)



##########mean body condition per type stage refined per county
dat_final_bc_type_stage_refined_per_country <- dat_final %>% 
  dplyr::group_by(country, type_stage_refined) %>%
  dplyr::summarise(mean_body_condition)

ggplot2::ggplot(dat_final_bc_type_stage_refined_per_country, ggplot2::aes(x = "", y = mean_body_condition, fill = factor(type_stage_refined))) + 
  ggplot2::facet_wrap(~country) +
  ggplot2::ylab("Mean body condition") +
  ggplot2::scale_fill_brewer(palette = "GnBu") +
  ggplot2::xlab("") +
  ggplot2::labs(fill = "Type stage refined") +
  ggplot2::geom_boxplot()

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_per_stage_refined_per_country.png"), width = 7, height = 5)



##########mean body condition per type individual per province marsh
dat_final_bc_type_individual_per_marsh <- dat_final %>% 
  dplyr::group_by(province_marsh, type_individual) %>%
  dplyr::summarise(mean_body_condition)

ggplot2::ggplot(dat_final_bc_type_individual_per_marsh, ggplot2::aes(x = "", y = mean_body_condition, fill = factor(type_individual))) + 
  ggplot2::facet_wrap(~province_marsh) +
  ggplot2::ylab("Mean body condition") +
  ggplot2::scale_fill_brewer(palette = "GnBu") +
  ggplot2::xlab("") +
  ggplot2::labs(fill = "Type individual") +
  ggplot2::geom_boxplot()

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_per_type_ind_per_marsh.png"), width = 7, height = 5)



##########mean body condition per type individual per province admin
dat_final_bc_type_individual_per_admin <- dat_final %>% 
  dplyr::group_by(province_admin, type_individual) %>%
  dplyr::summarise(mean_body_condition)

ggplot2::ggplot(dat_final_bc_type_individual_per_admin, ggplot2::aes(x = "", y = mean_body_condition, fill = factor(type_individual))) + 
  ggplot2::facet_wrap(~province_admin) +
  ggplot2::ylab("Mean body condition") +
  ggplot2::scale_fill_brewer(palette = "GnBu") +
  ggplot2::xlab("") +
  ggplot2::labs(fill = "Type individual") +
  ggplot2::geom_boxplot()

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_per_type_ind_per_admin.png"), width = 9, height = 7)


##########mean body condition per iucn status
dat_final_bc_per_iucn_status <- dat_final %>% 
  dplyr::group_by(iucn_status) %>%
  dplyr::summarise(mean_body_condition)

ggplot2::ggplot(dat_final_bc_per_iucn_status, ggplot2::aes(x = iucn_status, y = mean_body_condition)) +
  ggplot2::ylab("Mean body condition") +
  ggplot2::xlab("IUCN status") +
  ggplot2::geom_boxplot()

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_per_iucn_status.png"), width = 7, height = 5)


##########mean body condition per type individual per iucn status
dat_final_bc_per_iucn_status <- dat_final %>% 
  dplyr::group_by(iucn_status, type_individual) %>%
  dplyr::summarise(mean_body_condition)

ggplot2::ggplot(dat_final_bc_per_iucn_status, ggplot2::aes(x = iucn_status, y = mean_body_condition, fill = factor(type_individual))) +
  ggplot2::ylab("Mean body condition") +
  ggplot2::xlab("IUCN status") +
  ggplot2::scale_fill_brewer(palette = "GnBu") +
  ggplot2::labs(fill = "Type individual") +
  ggplot2::geom_boxplot()

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_per_type_individual_per_iucn_status.png"), width = 9, height = 6)


##########mean body condition per type stage per iucn status
dat_final_bc_per_iucn_status <- dat_final %>% 
  dplyr::group_by(iucn_status, type_stage) %>%
  dplyr::summarise(mean_body_condition)

ggplot2::ggplot(dat_final_bc_per_iucn_status, ggplot2::aes(x = iucn_status, y = mean_body_condition, fill = factor(type_stage))) +
  ggplot2::ylab("Mean body condition") +
  ggplot2::xlab("IUCN status") +
  ggplot2::scale_fill_brewer(palette = "GnBu") +
  ggplot2::labs(fill = "Type stage") +
  ggplot2::geom_boxplot()

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_per_type_stage_per_iucn_status.png"), width = 9, height = 6)



rm(list = ls())

load("dat_final.RData")


##########mean body condition per country (boxplot without color) 
count <- as.data.frame(table(dat_final$country))
names(count) <- c("country", "Freq")

median_data <- dat_final %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(median_bc = round(median(mean_body_condition), 3)) %>% 
  dplyr::arrange(desc(median_bc))

nb_individual <- dat_final %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(
    median_bc = round(median(mean_body_condition), 3),
    nb_ind = dplyr::n(), 
    mean_body_condition = mean_body_condition) %>%
  dplyr::arrange(desc(median_bc))

ordered_country <- factor(median_data$country, levels = median_data$country)

dat_final$country <- factor(dat_final$country, levels = ordered_country)
nb_individual$country <- factor(nb_individual$country, levels = ordered_country) #***

ggplot2::ggplot() +
  ggplot2::geom_point(data = nb_individual, ggplot2::aes(x = country, y = mean_body_condition), color = "lightgrey", alpha = 0) +
  ggplot2::geom_boxplot(data = nb_individual %>% 
                          dplyr::filter(nb_ind > 4), ggplot2::aes(x = country, y = mean_body_condition), fill = "grey80") +
  ggplot2::geom_point(data = nb_individual %>%
                        dplyr::filter(nb_ind <= 4), ggplot2::aes(x = country, y = mean_body_condition)) +
  ggplot2::theme_light() +
  ggplot2::labs(x = "", y = "Body Condition Index") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 55, hjust = 1, size = 12),
                 axis.title.y = ggplot2::element_text(size = 15),
                 legend.position = "bottom",
                 legend.text = ggplot2::element_text(size = 14), 
                 legend.title = ggplot2::element_text(size = 14)) +
  ggplot2::annotate("text", x = count$country[1], y = 1, label = paste0("n = ", count$Freq[1])) +
  ggplot2::annotate("text", x = count$country[2], y = 1, label = paste0("n = ", count$Freq[2])) +
  ggplot2::annotate("text", x = count$country[3], y = 1, label = paste0("n = ", count$Freq[3])) +
  ggplot2::annotate("text", x = count$country[4], y = 1, label = paste0("n = ", count$Freq[4])) +
  ggplot2::annotate("text", x = count$country[5], y = 1, label = paste0("n = ", count$Freq[5])) +
  ggplot2::annotate("text", x = count$country[6], y = 1, label = paste0("n = ", count$Freq[6])) +
  ggplot2::annotate("text", x = count$country[7], y = 1, label = paste0("n = ", count$Freq[7])) +
  ggplot2::annotate("text", x = count$country[8], y = 1, label = paste0("n = ", count$Freq[8])) +
  ggplot2::annotate("text", x = count$country[9], y = 1, label = paste0("n = ", count$Freq[9])) +
  ggplot2::annotate("text", x = count$country[10], y = 1, label = paste0("n = ", count$Freq[10])) +
  ggplot2::annotate("text", x = count$country[11], y = 1, label = paste0("n = ", count$Freq[11])) +
  ggplot2::annotate("text", x = count$country[12], y = 1, label = paste0("n = ", count$Freq[12])) +
  ggplot2::annotate("text", x = count$country[13], y = 1, label = paste0("n = ", count$Freq[13])) +
  ggplot2::annotate("text", x = count$country[14], y = 1, label = paste0("n = ", count$Freq[14])) +
  ggplot2::annotate("text", x = count$country[15], y = 1, label = paste0("n = ", count$Freq[15])) +
  ggplot2::annotate("text", x = count$country[16], y = 1, label = paste0("n = ", count$Freq[16])) +
  ggplot2::annotate("text", x = count$country[17], y = 1, label = paste0("n = ", count$Freq[17])) +
  ggplot2::annotate("text", x = count$country[18], y = 1, label = paste0("n = ", count$Freq[18])) +
  ggplot2::annotate("text", x = median_data$country[1], y = 0.84, label = median_data$median_bc[1]) +
  ggplot2::annotate("text", x = median_data$country[2], y = 0.82, label = median_data$median_bc[2]) +
  ggplot2::annotate("text", x = median_data$country[3], y = 0.81, label = median_data$median_bc[3]) +
  ggplot2::annotate("text", x = median_data$country[4], y = 0.775, label = median_data$median_bc[4]) +
  # ggplot2::annotate("text", x = median_data$country[5], y = 0.77, label = median_data$median_bc[5]) +
  # ggplot2::annotate("text", x = median_data$country[6], y = 0.765, label = median_data$median_bc[6]) +
  ggplot2::annotate("text", x = median_data$country[7], y = 0.735, label = median_data$median_bc[7]) +
  ggplot2::annotate("text", x = median_data$country[8], y = 0.75, label = median_data$median_bc[8]) +
  ggplot2::annotate("text", x = median_data$country[9], y = 0.745, label = median_data$median_bc[9]) +
  ggplot2::annotate("text", x = median_data$country[10], y = 0.742, label = median_data$median_bc[10]) +
  ggplot2::annotate("text", x = median_data$country[11], y = 0.733, label = median_data$median_bc[11]) +
  ggplot2::annotate("text", x = median_data$country[12], y = 0.725, label = median_data$median_bc[12]) +
  ggplot2::annotate("text", x = median_data$country[13], y = 0.72, label = median_data$median_bc[13]) +
  ggplot2::annotate("text", x = median_data$country[14], y = 0.70, label = median_data$median_bc[14]) +
  ggplot2::annotate("text", x = median_data$country[15], y = 0.683, label = median_data$median_bc[15]) 
# ggplot2::annotate("text", x = median_data$country[16], y = 0.65, label = median_data$median_bc[16]) +
# ggplot2::annotate("text", x = median_data$country[17], y = 0.645, label = median_data$median_bc[17]) +
# ggplot2::annotate("text", x = median_data$country[18], y = 0.645, label = median_data$median_bc[18]) 

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_BC_per_country.png"), width = 12, height = 8)



##########mean body condition per county and population status (boxplot with color of population status)
count <- as.data.frame(table(dat_final$country))
names(count) <- c("country", "Freq")

median_data <- dat_final %>%
  dplyr::group_by(country, iucn_status) %>%
  dplyr::summarise(median_bc = round(median(mean_body_condition), 3)) %>% 
  dplyr::arrange(desc(median_bc))

nb_individual <- dat_final %>%
  dplyr::group_by(country, iucn_status) %>%
  dplyr::mutate(nb_ind = dplyr::n()) %>%
  dplyr::summarise(
    median_bc = round(median(mean_body_condition), 3),
    nb_ind = dplyr::first(nb_ind),
    mean_body_condition = mean_body_condition) %>%
  dplyr::arrange(desc(median_bc))


ordered_country <- factor(median_data$country, levels = median_data$country)

dat_final$country <- factor(dat_final$country, levels = ordered_country)

colors <- c("NT" = "yellowgreen", "EN" = "orange", "CR" = "orangered", "DD" = "grey80")
iucn_levels <- c("CR", "EN", "NT", "DD")

nb_individual$country <- factor(nb_individual$country, levels = ordered_country) #***

ggplot2::ggplot() +
  ggplot2::geom_point(data = nb_individual, ggplot2::aes(x = country, y = mean_body_condition), alpha = 0) +
  ggplot2::geom_boxplot(data = nb_individual %>% 
                          dplyr::filter(nb_ind > 4), ggplot2::aes(x = country, y = mean_body_condition, fill = iucn_status)) +
  ggplot2::geom_point(data = nb_individual %>% 
                        dplyr::filter(nb_ind <= 4), ggplot2::aes(x = country, y = mean_body_condition, color = iucn_status)) +
  ggplot2::scale_fill_manual(values = colors, breaks = iucn_levels) +
  ggplot2::scale_color_manual(values = colors, breaks = iucn_levels) +
  ggplot2::theme_light() +
  ggplot2::labs(x = "", y = "Body Condition Index") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 55, hjust = 1, size = 9)) +
  ggplot2::guides(color = ggplot2::guide_legend("        Conservation status\n(IUCN 'official' or 'suggested')"),
                  fill = ggplot2::guide_legend("        Conservation status\n(IUCN 'official' or 'suggested')")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 15)) +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 14), legend.title = ggplot2::element_text(size = 14)) +
  ggplot2::annotate("text", x = count$country[1], y = 1, label = paste0("n = ", count$Freq[1])) +
  ggplot2::annotate("text", x = count$country[2], y = 1, label = paste0("n = ", count$Freq[2])) +
  ggplot2::annotate("text", x = count$country[3], y = 1, label = paste0("n = ", count$Freq[3])) +
  ggplot2::annotate("text", x = count$country[4], y = 1, label = paste0("n = ", count$Freq[4])) +
  ggplot2::annotate("text", x = count$country[5], y = 1, label = paste0("n = ", count$Freq[5])) +
  ggplot2::annotate("text", x = count$country[6], y = 1, label = paste0("n = ", count$Freq[6])) +
  ggplot2::annotate("text", x = count$country[7], y = 1, label = paste0("n = ", count$Freq[7])) +
  ggplot2::annotate("text", x = count$country[8], y = 1, label = paste0("n = ", count$Freq[8])) +
  ggplot2::annotate("text", x = count$country[9], y = 1, label = paste0("n = ", count$Freq[9])) +
  ggplot2::annotate("text", x = count$country[10], y = 1, label = paste0("n = ", count$Freq[10])) +
  ggplot2::annotate("text", x = count$country[11], y = 1, label = paste0("n = ", count$Freq[11])) +
  ggplot2::annotate("text", x = count$country[12], y = 1, label = paste0("n = ", count$Freq[12])) +
  ggplot2::annotate("text", x = count$country[13], y = 1, label = paste0("n = ", count$Freq[13])) +
  ggplot2::annotate("text", x = count$country[14], y = 1, label = paste0("n = ", count$Freq[14])) +
  ggplot2::annotate("text", x = count$country[15], y = 1, label = paste0("n = ", count$Freq[15])) +
  ggplot2::annotate("text", x = count$country[16], y = 1, label = paste0("n = ", count$Freq[16])) +
  ggplot2::annotate("text", x = count$country[17], y = 1, label = paste0("n = ", count$Freq[17])) +
  ggplot2::annotate("text", x = count$country[18], y = 1, label = paste0("n = ", count$Freq[18])) +
  ggplot2::annotate("text", x = median_data$country[1], y = 0.84, label = median_data$median_bc[1]) +
  ggplot2::annotate("text", x = median_data$country[2], y = 0.82, label = median_data$median_bc[2]) +
  ggplot2::annotate("text", x = median_data$country[3], y = 0.81, label = median_data$median_bc[3]) +
  ggplot2::annotate("text", x = median_data$country[4], y = 0.775, label = median_data$median_bc[4]) +
  # ggplot2::annotate("text", x = median_data$country[5], y = 0.77, label = median_data$median_bc[5]) +
  # ggplot2::annotate("text", x = median_data$country[6], y = 0.765, label = median_data$median_bc[6]) +
  ggplot2::annotate("text", x = median_data$country[7], y = 0.735, label = median_data$median_bc[7]) +
  ggplot2::annotate("text", x = median_data$country[8], y = 0.75, label = median_data$median_bc[8]) +
  ggplot2::annotate("text", x = median_data$country[9], y = 0.745, label = median_data$median_bc[9]) +
  ggplot2::annotate("text", x = median_data$country[10], y = 0.742, label = median_data$median_bc[10]) +
  ggplot2::annotate("text", x = median_data$country[11], y = 0.733, label = median_data$median_bc[11]) +
  ggplot2::annotate("text", x = median_data$country[12], y = 0.725, label = median_data$median_bc[12]) +
  ggplot2::annotate("text", x = median_data$country[13], y = 0.72, label = median_data$median_bc[13]) +
  ggplot2::annotate("text", x = median_data$country[14], y = 0.70, label = median_data$median_bc[14]) +
  ggplot2::annotate("text", x = median_data$country[15], y = 0.683, label = median_data$median_bc[15]) 
# ggplot2::annotate("text", x = median_data$country[16], y = 0.65, label = median_data$median_bc[16]) +
# ggplot2::annotate("text", x = median_data$country[17], y = 0.645, label = median_data$median_bc[17]) +
# ggplot2::annotate("text", x = median_data$country[18], y = 0.645, label = median_data$median_bc[18]) 

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_BC_per_country_per_iucn_status.png"), width = 12, height = 9)


#########mean_body_condition_type_stage_refined
#count sample size per type individual
dat_final <- dat_final %>% 
  dplyr::mutate(type_stage_refined_rename = dplyr::case_when(type_stage_refined == "adult_female" ~ "Adult female", 
                                                             type_stage_refined == "juvenile" ~ "Juvenile", 
                                                             type_stage_refined == "unidentified" ~ "Unidentified",
                                                             TRUE ~ NA_character_))

count <- as.data.frame(table(dat_final$type_stage_refined_rename))
names(count) <- c("type_individual", "Freq")

median_data <- dat_final %>%
  dplyr::group_by(type_stage_refined_rename) %>%
  dplyr::summarise(median_bc = round(median(mean_body_condition), 3))

dat_final %>%
  dplyr::group_by(type_stage_refined_rename) %>%
  dplyr::summarise(mean(mean_body_condition),
                   sd(mean_body_condition))

ggplot2::ggplot() +
  ggplot2::geom_boxplot(data = dat_final, ggplot2::aes(x = reorder(type_stage_refined_rename, - mean_body_condition), y = mean_body_condition), fill = "grey80") +
  ggplot2::geom_text(data = count, ggplot2::aes(label = paste("n =", Freq), x = type_individual, y = 0.99), size = 4, vjust = 0) +
  ggplot2::labs(x = "") + 
  ggplot2::theme_light() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::labs(y = "Body Condition Index") +
  ggplot2::stat_summary(data = median_data, fun = function(x) median(x), geom = "text", 
                        ggplot2::aes(label = median_bc, x = type_stage_refined_rename, y = median_bc), vjust = -1, size = 4) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10)) +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 15)) 

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_per_type_stage_refined.png"), width = 6, height = 5)



##########mean body condition per type individual per county
dat_final_bc_type_stage_per_country <- dat_final %>% 
  dplyr::group_by(country, type_stage_refined_rename) %>%
  dplyr::summarise(mean_body_condition)

ggplot2::ggplot(dat_final_bc_type_stage_per_country, ggplot2::aes(x = "", y = mean_body_condition, fill = factor(type_stage_refined_rename))) + 
  ggplot2::facet_wrap(~country) +
  ggplot2::ylab("Mean body condition") +
  ggplot2::scale_fill_brewer(palette = "GnBu") +
  ggplot2::xlab("") +
  ggplot2::labs(fill = "Type individual") +
  ggplot2::geom_boxplot()

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_per_type_stage_refined_per_country.png"), width = 8, height = 5)




##########mean body condition per social_media per country
dat_final_bc_social_media_per_country <- dat_final %>% 
  dplyr::group_by(country, social_media) %>%
  dplyr::summarise(mean_body_condition)

ggplot2::ggplot(dat_final_bc_social_media_per_country, ggplot2::aes(x = country, y = mean_body_condition, fill = factor(social_media))) + 
  ggplot2::ylab("Mean body condition") +
  ggplot2::xlab("") +
  ggplot2::labs(fill = "Videos from\nsocial media") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 55, hjust = 1, size = 9)) +
  ggplot2::geom_boxplot()

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "barplot_mean_body_condition_from_social_media_VS_surveys_per_country.png"), width = 8, height = 5)


##########mean body condition from social media vs survey
dat_final %>% 
  dplyr::group_by(social_media) %>%
  dplyr::summarise(mean(mean_body_condition))

ggplot2::ggplot(dat_final, ggplot2::aes(x = social_media, y = mean_body_condition)) +
  ggplot2::geom_boxplot(fill = "grey80") +
  ggplot2::labs(x = "Videos from social media", y = "Mean body condition") +
  ggplot2::theme_light() 

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "boxplot_mean_body_condition_from_social_media_VS_surveys.png"), width = 9, height = 7)



##########plot body condition female vs juvenile

mes_dat_final_img %>% 
  dplyr::filter(is.na(female_calf_couple_image_name) == FALSE)  %>% 
  dplyr::mutate(type_individual_new = dplyr::case_when(type_individual == "calf" ~ "juvenile",
                                                       type_individual == "female" ~ "female",
                                                       TRUE ~ type_individual)) %>% 
  #pivot wide (https://tidyr.tidyverse.org/articles/pivot.html) to get l_nb_pixels and w_nb_pixels columns  
  tidyr::pivot_wider(names_from = type_individual_new, 
                     values_from = body_condition) -> mes_dat_final_img2

mes_dat_final_img3 <- mes_dat_final_img2[,c("female_calf_couple_image_name", "female", "juvenile")]

mes_dat_final_img3 %>% 
  dplyr::group_by(female_calf_couple_image_name) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::filter(n == 2) -> id

mes_dat_final_img3 %>% 
  dplyr::filter(female_calf_couple_image_name %in% id$female_calf_couple_image_name)  %>% 
  dplyr::group_by(female_calf_couple_image_name) %>% 
  #summarize (after this step the number of rows of the dataframe is divided by 2)
  dplyr::summarise(female = mean(female, na.rm = TRUE), # here we use means as convenience functions to drop NAs, no real mean is calculated
                   juvenile = mean(juvenile, na.rm = TRUE)) %>% 
  dplyr::mutate(female_calf_couple_image_name_short = stringr::str_sub(female_calf_couple_image_name, 1, 10)) %>%   # extract first 10 characters
  #extract first mesurements of female/juvenile in sequence of images
  dplyr::group_by(female_calf_couple_image_name_short) %>% 
  dplyr::summarize(female_calf_couple_image_name_short = dplyr::first(female_calf_couple_image_name_short),
                   female = dplyr::first(female),
                   juvenile = dplyr::first(juvenile)) -> mes_dat_final_img4

dim(mes_dat_final_img4) #33 couples of female/juvenile 

#linear regression
mod = lm(female ~ juvenile, data = mes_dat_final_img4)
summary(mod) #R2 0.3222


#plot 
ggplot2::ggplot(data = mes_dat_final_img4, ggplot2::aes(x = female, y = juvenile)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = lm, colour = "dark grey") +
  ggplot2::xlim(c(0.55, 0.95)) +
  ggplot2::ylim(c(0.55, 0.95)) +
  ggplot2::xlab("Adult female BCI") +
  ggplot2::ylab("Juvenile BCI") +
  ggplot2::theme_light() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 15),
                 axis.text = ggplot2::element_text(size = 13))

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "plot_body_condition_female_VS_juvenile_couples.png"), width = 9, height = 7)



mes_dat_final_img3 %>% 
  dplyr::filter(female_calf_couple_image_name %in% id$female_calf_couple_image_name)  %>% 
  dplyr::group_by(female_calf_couple_image_name) %>% 
  #summarize (after this step the number of rows of the dataframe is divided by 2)
  dplyr::summarise(female = mean(female, na.rm = TRUE), # here we use means as convenience functions to drop NAs, no real mean is calculated
                   juvenile = mean(juvenile, na.rm = TRUE)) -> mes_dat_final_img4

dim(mes_dat_final_img4) #139 measurements of female/juvenile 

#linear regression
mod = lm(female ~ juvenile, data = mes_dat_final_img4)
summary(mod) #R2 0.4056


#plot 
ggplot2::ggplot(data = mes_dat_final_img4, ggplot2::aes(x = female, y = juvenile)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = lm, colour = "dark grey") +
  ggplot2::xlim(c(0.5, 1)) +
  ggplot2::ylim(c(0.5, 1)) +
  ggplot2::xlab("Adult female BCI") +
  ggplot2::ylab("Juvenile BCI") +
  ggplot2::theme_light() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 15),
                 axis.text = ggplot2::element_text(size = 13))

ggplot2::ggsave(here::here("outputs", "2-explore_measurement_tables", "plot_body_condition_female_VS_juvenile_measurements.png"), width = 9, height = 7)

