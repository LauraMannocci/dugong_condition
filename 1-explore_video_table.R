rm(list=ls())
#load pipe operator
library(magrittr)


#read and clean excel table -----------------------------------------------------------------
#read
vid_dat <- readxl::read_excel(here::here("data", "Media_Sorting_Table.xlsx"), sheet = "videos")

#view data structure
str(vid_dat)

#clean
vid_dat <- vid_dat %>% 
  #convert types
  dplyr::mutate(publication_date = as.Date(publication_date, format = "%d/%m/%Y")) %>% #resulting date is year/month/day
  dplyr::mutate(collection_date = as.Date(collection_date, format = "%d/%m/%Y")) %>%  #resulting date is year/month/day
  dplyr::mutate_at(c('approx_latitude', 'approx_longitude'), as.numeric) %>% #multiple columns at the same time 
  #add video_name_long (column added to later join with measurements table)
  dplyr::mutate(video_name_long = dplyr::case_when(is.na(file_created) == FALSE ~ file_created,
                                                   TRUE ~ file_original))  %>%    
  #extract years
  dplyr::mutate(collection_year = format(as.Date(collection_date, format = "%d/%m/%Y"),"%Y")) %>% 
  dplyr::mutate(publication_year = format(as.Date(publication_date, format = "%d/%m/%Y"),"%Y")) %>%
  #extract months TODO
  dplyr::mutate(collection_month = format(as.Date(collection_date, format = "%d/%m/%Y"),"%m")) %>%
  dplyr::mutate(publication_month = format(as.Date(publication_date, format = "%d/%m/%Y"),"%m")) %>% 
  #correct column
  dplyr::mutate(same_individual_measured = dplyr::case_when(same_individual_measured == "No" ~ "no",
                                                            TRUE ~ same_individual_measured)) %>% 
  #remove uninteresting columns (ex: email, ffmpeg etc)
  dplyr::select(-email,-ffmpeg_cut,-ffmpeg_command,-imagej_measurement,-directory,-link,-comment,-acknowledge) %>% 
  dplyr::mutate(number_video = 1) %>% 
  #genetic_cluster from Srinivas et al. 
  dplyr::mutate(genetic_cluster = dplyr::case_when(
    country %in% c("Mayotte", "Mozambique", "Seychelles") ~ "swio",
    country %in% c("United Arab Emirates","Saudi Arabia", "Qatar") ~ "nwio", 
    country %in% c("India", "Sri Lanka") ~ "sa",
    country %in% c("Thailand", "Philippines", "Indonesia", "Timor-Leste") ~ "sea",
    country %in% c("Australia", "New Caledonia", "Papua New Guinea", "Palau", "Vanuatu") ~ "pac", 
    TRUE ~ NA )) %>% 
  dplyr::mutate(genetic_cluster = as.factor(genetic_cluster))
  

#number of video used / not used 
nrow(vid_dat) #228 videos
prop.table(table(vid_dat$used)) #0.41% no and 0.59% yes
table(vid_dat$used) #94 no AND 134 yes


#filter data -----------------------------------------------------------------------------

vid_dat_final <- vid_dat %>% 
  dplyr::filter(quality_check == "yes") %>% #quality checked test passed
  dplyr::filter(duplicate == "no") %>% #non duplicate
  dplyr::filter(!is.na(country)) %>%  #country unidentified
  dplyr::filter(used == "yes")


#save check video table

save(vid_dat_final, file = "vid_dat_final.RData")
load("vid_dat_final.RData")




# exploration ----------------------------------------------------------------------------
#number of individuals per video 
summary(vid_dat_final$number_unique_individuals) #mean = 6.16, min = 1, max = 100
sd(vid_dat_final$number_unique_individuals)

#number of video from social media and survey per country
count_data <- vid_dat_final %>%
  dplyr::group_by(country, social_media) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::ungroup()

ggplot2::ggplot(count_data, ggplot2::aes(x = reorder(country, -count), y = count, fill = social_media)) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::labs(x = "", y = "Number of videos") +
  ggplot2::theme_light() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_number_videos_from_social_media_VS_surveys_per_country.png"), width = 7, height = 5)


#visualization percent of video from social media and survey per country
percent_video_per_social_media <- vid_dat_final %>% 
  dplyr::group_by(country, social_media) %>% 
  dplyr::summarise(count = dplyr::n()) %>% 
  dplyr::mutate(percentage = count/sum(count) * 100)

ggplot2::ggplot(percent_video_per_social_media, ggplot2::aes(fill = social_media, x = country, y = percentage)) +
  ggplot2::geom_bar(stat = "identity", position = "fill") +
  ggplot2::labs(x = "", y = "Percentage of videos", fill = "Videos from\nsocial media") +
  ggplot2::scale_fill_brewer(palette = "Oranges") +
  ggplot2::geom_text(ggplot2::aes(label = paste0(round(percentage), "%")), 
                     position = ggplot2::position_fill(vjust = 0.5), 
                     color = "black", size = 3, fontface = "bold") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_percent_videos_from_social_media_VS_surveys_per_country.png"), width = 9, height = 7)


#visualization percent of video from social media and survey per country version 2
percent_video_per_social_media <- vid_dat_final %>% 
  dplyr::group_by(country, social_media) %>% 
  dplyr::summarise(count = dplyr::n()) %>% 
  dplyr::mutate(percentage = count/sum(count) * 100)

ggplot2::ggplot(percent_video_per_social_media, ggplot2::aes(fill = social_media, x = country, y = percentage)) +
  ggplot2::geom_bar(stat = "identity", position = "fill") +
  ggplot2::labs(x = "", y = "Percentage of videos", fill = "") +
  ggplot2::scale_fill_brewer(palette = "Oranges", labels = c("Scientific surveys", "Social media")) +
  ggplot2::geom_text(ggplot2::aes(label = paste0(round(percentage), "%")), 
                     position = ggplot2::position_fill(vjust = 0.5), 
                     color = "black", size = 3, fontface = "bold") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 panel.background = ggplot2::element_blank())  

ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_percent_videos_from_social_media_VS_surveys_per_country2.png"), width = 9, height = 7)


#visualization of resolution of video from social media and survey
vid_dat_final2 <- vid_dat_final %>% 
  dplyr::mutate(video_resolution = video_width * video_height)

ggplot2::ggplot(vid_dat_final2, ggplot2::aes(x = social_media, y = video_resolution)) +
  ggplot2::geom_boxplot(fill = "grey80") +
  ggplot2::labs(x = "Videos from social media", y = "Resolution of videos") +
  ggplot2::theme_light() 

ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "boxplot_resolution_of_videos_from_social_media_VS_surveys_per_country.png"), width = 9, height = 7)


#number of video taken at same lat / lon
length(unique(vid_dat_final$approx_longitude)) # 72 videos 
length(unique(vid_dat_final$approx_latitude)) # 72 videos 


#number of video taken in the same place (most_precise_location)
length(unique(vid_dat_final$most_precise_location)) # 70 videos with same place

video_with_same_location <- vid_dat_final %>%
  dplyr::mutate(most_precise_date = dplyr::case_when(!is.na(collection_date) ~ collection_date, 
                                                     TRUE ~ publication_date)) %>% 
  dplyr::group_by(most_precise_location, most_precise_date) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::filter(count > 1) # 16 videos with same place and same date 


#barplots --------------------------------------------------------------------------------
########owner_type
count <- as.data.frame(table(vid_dat_final$owner_type))
names(count) <- c("owner_type", "Freq")

barplot =  ggplot2::ggplot(data = vid_dat_final, ggplot2::aes(x = forcats::fct_rev(forcats::fct_infreq(owner_type)))) + 
  ggplot2::geom_bar() +
  ggplot2::labs(x = "") +
  ggplot2::labs(y = "Count") +
  ggplot2::geom_text(data = count, ggplot2::aes(label = paste("n =", Freq), x = owner_type, y = 0.99), size = 4, vjust = 0) +
  ggplot2::coord_flip()

barplot
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_owner_type.png"), barplot, width = 7, height = 5)


########publication_year
count <- as.data.frame(table(vid_dat_final$publication_year))
names(count) <- c("publication_year", "Freq")

barplot =  ggplot2::ggplot(data = vid_dat_final, ggplot2::aes(x = publication_year)) + 
  ggplot2::geom_bar() +
  ggplot2::labs(x = "") +
  ggplot2::labs(y = "Count") +
  ggplot2::geom_text(data = count, ggplot2::aes(label = paste("n =", Freq), x = publication_year, y = 0.99), size = 4, vjust = 0) 

barplot
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_publication_year.png"), barplot, width = 7, height = 5)


########collection_year 
count <- as.data.frame(table(vid_dat_final$collection_year))
names(count) <- c("collection_year", "Freq")

barplot =  ggplot2::ggplot(data = vid_dat_final, ggplot2::aes(x = collection_year)) + 
  ggplot2::geom_bar() +
  ggplot2::labs(x = "") +
  ggplot2::labs(y = "Count") +
  ggplot2::geom_text(data = count, ggplot2::aes(label = paste("n =", Freq), x = collection_year, y = 0.99), size = 4, vjust = 0) 

barplot
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_collection_year.png"), barplot, width = 7, height = 5)


########social_media 
count <- as.data.frame(table(vid_dat_final$social_media))
names(count) <- c("social_media", "Freq")

barplot =  ggplot2::ggplot(data = vid_dat_final, ggplot2::aes(x = social_media)) + 
  ggplot2::geom_bar() +
  ggplot2::labs(x = "Videos from social media") +
  ggplot2::labs(y = "Count") +
  ggplot2::geom_text(data = count, ggplot2::aes(label = paste("n =", Freq), x = social_media, y = 0.99), size = 4, vjust = 0) 

barplot
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_social_media.png"), barplot, width = 7, height = 5)


########source 
count <- as.data.frame(table(vid_dat_final$source))
names(count) <- c("source", "Freq")

barplot =  ggplot2::ggplot(data = vid_dat_final, ggplot2::aes(x = forcats::fct_rev(forcats::fct_infreq(source)))) + 
  ggplot2::geom_bar() +
  ggplot2::labs(x = "") +
  ggplot2::labs(y = "Count") +
  ggplot2::geom_text(data = count, ggplot2::aes(label = paste("n =", Freq), x = source, y = 0.99), size = 4, vjust = 0) +
  ggplot2::coord_flip()

barplot
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_source.png"), barplot, width = 7, height = 5)


########presence_calf 
barplot = ggplot2::ggplot(data = vid_dat_final, ggplot2::aes(x = presence_calf)) +
  ggplot2::geom_bar() +
  ggplot2::labs(x = "Presence calf")

ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_presence_calf.png"), barplot, width = 7, height = 5)
barplot


########number_calf per location 
vid_dat_final_ind <- vid_dat_final %>% 
  dplyr::group_by(most_precise_location) %>% 
  dplyr::summarise(number_calf= sum(number_calf, na.rm = TRUE)) %>% 
  dplyr::filter(number_calf>0)

barplot = ggplot2::ggplot(data = vid_dat_final_ind, ggplot2::aes(x = reorder(most_precise_location, number_calf), y = number_calf)) +
  ggplot2::geom_col() +
  ggplot2::labs(x = "Number of calf per location") +
  ggplot2::geom_text(data = vid_dat_final_ind, ggplot2::aes(label = paste("n =", number_calf), x = most_precise_location, y = 0.99), size = 3, vjust = 0) +
  ggplot2::coord_flip()

barplot
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_number_calf_per_loccation.png"), barplot, width = 7, height = 5)


########sum of number of females/calves/unidentified/males
vid_dat_final_sum <- vid_dat_final %>%
  dplyr::mutate(females = sum(!is.na(number_female))) %>%
  dplyr::mutate(calves = sum(!is.na(number_calf))) %>%
  dplyr::mutate(unidentified = sum(!is.na(number_unidentified))) %>%
  dplyr::mutate(males = sum(!is.na(number_male))) %>%
  dplyr::select(females, calves, unidentified, males)  %>%
  dplyr::distinct() %>%
  t() %>%  #transpose (columns become rows - this structure is needed for the plot)
  as.data.frame() %>% #need to convert back to dataframe
  tibble::rownames_to_column("type") #rownames become columns

names(vid_dat_final_sum) <- c("type", "numbers")


barplot =  ggplot2::ggplot(data = vid_dat_final_sum, ggplot2::aes(x = reorder(type, -numbers), y = numbers)) +
  ggplot2::geom_col() +
  ggplot2::geom_text(data = vid_dat_final_sum, ggplot2::aes(label = paste("n =", numbers), x = type, y = 0.99), size = 4, vjust = 0) +
  ggplot2::labs(x = "")
barplot

ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_sum_ind.png"), barplot, width = 7, height = 5)  


########number_unique_individuals per country 
vid_dat_final_ind <- vid_dat_final %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(number_unique_individuals = sum(number_unique_individuals))

barplot = ggplot2::ggplot(data = vid_dat_final_ind, ggplot2::aes(x = reorder(country, number_unique_individuals), y = number_unique_individuals)) +
  ggplot2::geom_col() + 
  ggplot2::labs(x = "") + 
  ggplot2::coord_flip() +
  ggplot2::ylab("Number unique individuals") +
  ggplot2::scale_y_continuous(breaks = seq(0, 220, by = 20))

ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_number_indivuduals_per_country.png"), barplot, width = 7, height = 5)
barplot


#########number_unique_individuals per location 
vid_dat_final_ind_loc <- vid_dat_final %>% 
  dplyr::group_by(most_precise_location) %>% 
  dplyr::summarise(number_unique_individuals = sum(number_unique_individuals))

barplot = ggplot2::ggplot(data = vid_dat_final_ind_loc, ggplot2::aes(x = reorder(most_precise_location, number_unique_individuals), y = number_unique_individuals)) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(x= "", y = "Number individual") +
  ggplot2::geom_text(data = vid_dat_final_ind_loc, ggplot2::aes(label = paste("n =", number_unique_individuals), x = most_precise_location, y = 0.99), size = 3, vjust = 0) +
  ggplot2::scale_y_continuous(breaks = seq(0, 220, by = 20))

ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_number_indivuduals_per_location.png"), barplot, width = 8, height = 10)
barplot


########number of video per location
vid_dat_final_video <- vid_dat_final %>% 
  dplyr::group_by(most_precise_location) %>% 
  dplyr::summarise(number_video = sum(number_video))

barplot = ggplot2::ggplot(data = vid_dat_final_video, ggplot2::aes(x = reorder(most_precise_location, number_video), y = number_video)) +
  ggplot2::geom_col() +
  ggplot2::labs(x = "", y = "Number video") +
  ggplot2::coord_flip() +
  ggplot2::geom_text(data = vid_dat_final_video, ggplot2::aes(label = paste("n =", number_video), x = most_precise_location, y = 0.99), size = 3, vjust = 0) +
  ggplot2::scale_y_continuous(breaks = seq(0, 7, by = 1))
  
barplot
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_number_video_per_location.png"), barplot, width = 8, height = 10)

 

########number of video per country
vid_dat_final_video <- vid_dat_final %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(number_video = sum(number_video)) %>% 
  dplyr::mutate(percent = round(number_video / sum(number_video) * 100, 2))

barplot = ggplot2::ggplot(data = vid_dat_final_video, ggplot2::aes(x = reorder(country, number_video), y = number_video)) +
  ggplot2::geom_col() +
  ggplot2::labs(x = "", y = "Number of video")+
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(breaks = seq(0, 30, by = 2))

barplot
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_number_video_per_country.png"), barplot, width = 7, height = 5)


########percent of video per country
vid_dat_final_video <- vid_dat_final %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(number_video = sum(number_video)) %>% 
  dplyr::mutate(pct = round(number_video / sum(number_video) * 100, 1))

barplot = ggplot2::ggplot(data = vid_dat_final_video, ggplot2::aes(x = reorder(country, number_video), y = pct)) +
  ggplot2::geom_col() +
  ggplot2::labs(x = "", y = "Percent of video")+
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(breaks = seq(0, 30, by = 2))

barplot
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_percent_video_per_country.png"), barplot, width = 7, height = 5)


#########number_individual_measured per country
vid_dat_final_ind_measured_country <- vid_dat_final %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(number_individual_measured = sum(number_individual_measured))

 
barplot <- ggplot2::ggplot(data = vid_dat_final_ind_measured_country, ggplot2::aes(x = reorder(country, number_individual_measured), y = number_individual_measured)) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(x= "", y = "Number individual measured") +
  ggplot2::scale_y_continuous(breaks = seq(0, 60, by = 5))

barplot
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_number_individual_measured_per_country.png"), barplot, width = 7, height = 5)


########percent of number_individual_measured per country
vid_dat_final_ind_measured_country <- vid_dat_final %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(number_individual_measured = sum(number_individual_measured)) %>% 
  dplyr::mutate(pct = round(number_individual_measured / sum(number_individual_measured) * 100, 1))

barplot = ggplot2::ggplot(data = vid_dat_final_ind_measured_country, ggplot2::aes(x = reorder(country, number_individual_measured), y = pct)) +
  ggplot2::geom_col() +
  ggplot2::labs(x = "", y = "Percent individual measured")+
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(breaks = seq(0, 30, by = 2))

barplot
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_percent_individual_measured_per_country.png"), barplot, width = 7, height = 5)



#########number_individual_measured per location 
vid_dat_final_ind_measured_loc <- vid_dat_final %>% 
  dplyr::group_by(most_precise_location) %>% 
  dplyr::summarise(number_individual_measured = sum(number_individual_measured))

barplot = ggplot2::ggplot(data = vid_dat_final_ind_measured_loc, ggplot2::aes(x = reorder(most_precise_location, number_individual_measured), y = number_individual_measured)) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(x= "", y = "Number individual measured") +
  ggplot2::geom_text(data = vid_dat_final_ind_measured_loc, ggplot2::aes(label = paste("n =", number_individual_measured), x = most_precise_location, y = 0.99), size = 2, vjust = 0) +
  ggplot2::scale_y_continuous(breaks = seq(0, 31, by = 2))

barplot
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_indivudual_measured_per_location.png"), barplot, width = 7, height = 10)


#########percentage of owner_type per country
vid_dat_final_owner_type <- vid_dat_final %>% 
  dplyr::group_by(owner_type, country) %>% 
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(percentage = count / sum(count) * 100)

barplot <- ggplot2::ggplot(data = vid_dat_final_owner_type, ggplot2::aes(fill = owner_type, x = country, y = percentage)) +
  ggplot2::geom_bar(stat = "identity", position = "fill", colour = "white") +
  ggplot2::scale_fill_brewer(palette = "Purples") +
  ggplot2::labs(x= "", y = "Percent") +
  ggplot2::coord_flip()

barplot
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "barplot_percentage_owner_type_per_country.png"), barplot, width = 7, height = 5)



#make maps --------------------------------------------------------------------------------
#load world map
#medium resolution
world <- rnaturalearth::ne_countries(scale = 'medium',  type = "countries", returnclass = 'sf')
#high resolution -> for zooms
world_large <- rnaturalearth::ne_countries(scale = 'large',  type = "countries", returnclass = 'sf') #error for Camille


########map of points ---------------------------------------------------
vid_dat_final_video <- vid_dat_final %>% 
  dplyr::group_by(approx_latitude, approx_longitude) %>% 
  dplyr::summarise(number_video = sum(number_video))

countries_confirmed <- c("Australia", "Bahrain", "Brunei", "Cambodia", "Comoros", "Djibouti", "Egypt", "Eritrea", "India", 
                         "Indonesia", "Japan", "Kenya", "Saudi Arabia", "Madagascar", "Mayotte", "Malaysia", "Mozambique", "New Caledonia", 
                         "Palau", "Papua New Guinea" , "Philippines", "Qatar", "Seychelles", "Singapore", "Solomon Is.", "Sri Lanka",
                         "Sudan", "Thailand", "Timor-Leste", "United Arab Emirates", "Tanzania", "Vanuatu", "Vietnam", "Yemen")


counties_suggested <- c("Bangladesh", "Iran", "Israel", "Kuwait", "Myanmar", "Oman", "Somalia", "Somaliland")

countries_confirmed_w <- world[world$name %in% countries_confirmed, ]
counties_suggested_w <- world[world$name %in%  counties_suggested, ]



map = 
  #basemap
  ggplot2::ggplot(world) + 
  ggplot2::geom_sf() + 
  ggplot2::geom_sf(fill = "gray93", color = "grey20", size = 0.5) + 
  ggplot2::geom_sf(data = countries_confirmed_w, fill = "grey40", color = "grey20", size = 0.5) +
  ggplot2::geom_sf(data = counties_suggested_w, fill = "grey70", color = "grey20", size = 0.5) +
  ggplot2::coord_sf(xlim=c(35,166), ylim = c(27, -40)) + #limit to Indo-Pacific
  ggplot2::scale_x_continuous(breaks = seq(35, 166, by = 10)) + #graduations on x
  ggplot2::scale_y_continuous(breaks = seq(-40, 27, by = 10)) + #graduations on y
  #our data as points
  ggplot2::geom_point(data = vid_dat_final, ggplot2::aes(approx_longitude, approx_latitude), size = 1.5, color = "red", alpha = 0.6) +
  ggplot2::theme(axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_text(size = 7),
                 panel.background = ggplot2::element_blank())
                 
map
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "map_points.png"), map, width = 7, height = 5)



########maps per country (polygons) -----------------------------------------------

####number of video per country
vid_dat_final %>%
  dplyr::rename("name" = country) %>% #need to rename column to correspond to world data
  dplyr::group_by(name) %>%
  dplyr::summarise(n = dplyr::n()) -> dat_count


#join our data to continent data
world %>%
  dplyr::group_by(name) %>%
  dplyr::full_join(dat_count, by = "name") -> world_count


map = ggplot2::ggplot() +
  ggplot2::geom_sf(data = world_count, ggplot2::aes(fill = n)) + #number as color scale
  viridis::scale_fill_viridis(alpha = 1, begin = 0, end = 1, direction = -1, discrete = FALSE, option = "D", na.value = "grey80", name = "Nb. of \nvideos") + 
  ggplot2::coord_sf(xlim = c(35,166), ylim = c(27, -40)) + #limit to Indo-Pacific
  ggplot2::theme(axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks =  ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 legend.position = "bottom")

map
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "map_videos_per_country.png"), map, width = 7, height = 5)



####number of video Vanuatu and New Caledonia (zoom with high resolution world map)

#join our data to continent data
world_large %>%
  dplyr::group_by(name) %>%
  dplyr::full_join(dat_count, by = "name") -> world_large_count

map = ggplot2::ggplot() +
  ggplot2::geom_sf(data = world_large_count, ggplot2::aes(fill = n)) + #number as color scale
  viridis::scale_fill_viridis(alpha = 1, begin = 0, end = 1, direction = -1, discrete = FALSE, option = "D", na.value = "grey80", name = "Nb. of \nvideos") + 
  ggplot2::coord_sf(ylim = c(-13.5, -23), xlim = c(163.5, 171)) + #limit to Indo-Pacific
  ggplot2::theme(axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks =  ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 legend.position = "none") #remove legend

map
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "map_videos_per_country_Vanuatu_New_Caledonia.png"), map, width = 7, height = 5)



####number of video Palau (zoom with high resolution world map)

map = ggplot2::ggplot() +
  ggplot2::geom_sf(data = world_large_count, ggplot2::aes(fill = n)) + #number as color scale
  viridis::scale_fill_viridis(alpha = 1, begin = 0, end = 1, direction = -1, discrete = FALSE, option = "D", na.value = "grey80", name = "Nb. of \nvideos") + 
  ggplot2::coord_sf(ylim = c(6.8, 8.3), xlim = c(134, 135)) + #limit to Indo-Pacific
  ggplot2::theme(axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks =  ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 legend.position = "none") #remove legend

map
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "map_videos_per_country_Palau.png"), map, width = 7, height = 5)


####number of video Mayotte (zoom with high resolution world map)

map = ggplot2::ggplot() +
  ggplot2::geom_sf(data = world_large_count, fill = "yellow") + #we put yellow as color (for one video) directly as no country exists for Mayotte in world data
  viridis::scale_fill_viridis(alpha = 1, begin = 0, end = 1, direction = -1, discrete = FALSE, option = "D", na.value = "grey80", name = "Nb. of \nvideos") + 
  ggplot2::coord_sf(ylim = c(-12.6, -13.1), xlim = c(44.7, 45.5)) + #limit to Indo-Pacific
  ggplot2::theme(axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks =  ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 legend.position = "none") #remove legend

map
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "map_videos_per_country_Mayotte.png"), map, width = 7, height = 5)


####number of video Aldabra  (zoom with high resolution world map)

map = ggplot2::ggplot() +
  ggplot2::geom_sf(data = world_large_count, ggplot2::aes(fill = n)) + #number as color scale
  viridis::scale_fill_viridis(alpha = 1, begin = 0, end = 1, direction = -1, discrete = FALSE, option = "D", na.value = "grey80", name = "Nb. of \nvideos") + 
  ggplot2::coord_sf(ylim = c(-9.3, -9.5), xlim = c(46.1, 46.6)) + #limit to Indo-Pacific
  ggplot2::theme(axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks =  ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 legend.position = "none") #remove legend

map
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "map_videos_per_country_Aldabra.png"), map, width = 7, height = 5)



####number of individual measured per country  
vid_dat_final %>%
  dplyr::rename("name" = country) %>% #need to rename column to correspond to world data
  dplyr::group_by(name) %>%
  dplyr::summarise(tot_ind_measured = sum(number_individual_measured)) -> dat_count_ind


#join our data to continent data
world %>%
  dplyr::group_by(name) %>%
  dplyr::full_join(dat_count_ind, by = "name") -> world_count_ind


map = ggplot2::ggplot() +
  ggplot2::geom_sf(data = world_count_ind, ggplot2::aes(fill = tot_ind_measured)) + #number as color scale
  viridis::scale_fill_viridis(alpha = 1, begin = 0, end = 1, direction = -1, discrete = FALSE, option = "D", na.value = "grey80", name = "Nb. of \nindividuals \nmeasured") + 
  ggplot2::coord_sf(xlim = c(35,166), ylim = c(27, -40)) + #limit to Indo-Pacific
  ggplot2::theme(axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks =  ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(), 
                 legend.position = "bottom")

map
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "map_individual_measured_per_country.png"), map, width = 7, height = 5)



####number of individual measured Vanuatu New Caledonia  

#join our data to continent data
world_large %>%
  dplyr::group_by(name) %>%
  dplyr::full_join(dat_count_ind, by = "name") -> world_large_count_ind

map = ggplot2::ggplot() +
  ggplot2::geom_sf(data = world_large_count_ind, ggplot2::aes(fill = tot_ind_measured)) + #number as color scale
  viridis::scale_fill_viridis(alpha = 1, begin = 0, end = 1, direction = -1, discrete = FALSE, option = "D", na.value = "grey80", name = "Nb. of \nindividuals \nmeasured") + 
  ggplot2::coord_sf(ylim = c(-13.5, -23), xlim = c(163.5, 171)) + 
  ggplot2::theme(axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks =  ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 legend.position = "none") #remove legend

map
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "map_individual_measured_per_country_Vanuatu_New_Caledonia.png"), map, width = 7, height = 5)


####number of individual measured Palau  

map = ggplot2::ggplot() +
  ggplot2::geom_sf(data = world_large_count_ind, ggplot2::aes(fill = tot_ind_measured)) + #number as color scale
  viridis::scale_fill_viridis(alpha = 1, begin = 0, end = 1, direction = -1, discrete = FALSE, option = "D", na.value = "grey80", name = "Nb. of \nindividuals \nmeasured") + 
  ggplot2::coord_sf(ylim = c(6.8, 8.3), xlim = c(134, 135)) + 
  ggplot2::theme(axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks =  ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 legend.position = "none") #remove legend

map
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "map_individual_measured_per_country_Palau.png"), map, width = 7, height = 5)



####number of individual measured Mayotte (zoom with high resolution world map)

map = ggplot2::ggplot() +
  ggplot2::geom_sf(data = world_large_count_ind, fill = "yellow") + #we put yellow as color (for one individual) directly as no country exists for Mayotte in world data
  viridis::scale_fill_viridis(alpha = 1, begin = 0, end = 1, direction = -1, discrete = FALSE, option = "D", na.value = "grey80", name = "Nb. of \nvideos") + 
  ggplot2::coord_sf(ylim = c(-12.6, -13.1), xlim = c(44.7, 45.5)) + #limit to Indo-Pacific
  ggplot2::theme(axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks =  ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 legend.position = "none") #remove legend

map
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "map_individual_measured_per_country_Mayotte.png"), map, width = 7, height = 5)


####number of individual measured Aldabra  (zoom with high resolution world map)

map = ggplot2::ggplot() +
  ggplot2::geom_sf(data = world_large_count_ind, ggplot2::aes(fill = tot_ind_measured)) + #number as color scale
  viridis::scale_fill_viridis(alpha = 1, begin = 0, end = 1, direction = -1, discrete = FALSE, option = "D", na.value = "grey80", name = "Nb. of \nvideos") + 
  ggplot2::coord_sf(ylim = c(-9.3, -9.5), xlim = c(46.1, 46.6)) + #limit to Indo-Pacific
  ggplot2::theme(axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks =  ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 legend.position = "none") #remove legend

map
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "map_individual_measured_per_country_Aldabra.png"), map, width = 7, height = 5)




########## MAP ##############################################################
########maps number of videos per location (proportionals points) ---------------
vid_dat_final_video <- vid_dat_final %>% 
  dplyr::group_by(approx_latitude, approx_longitude) %>% 
  dplyr::summarise(number_video = sum(number_video))

#load world map (medium resolution)
world <- rnaturalearth::ne_countries(scale = 'medium',  type = "countries", returnclass = 'sf')

#select countries where dugong populations are confirmed and suggested 
countries_confirmed <- c("Australia", "Bahrain", "Brunei", "Cambodia", "Comoros", "Djibouti", "Egypt", "Eritrea", "India", 
                         "Indonesia", "Japan", "Kenya", "Saudi Arabia", "Madagascar", "Mayotte", "Malaysia", "Mozambique", "New Caledonia", 
                         "Palau", "Papua New Guinea" , "Philippines", "Qatar", "Seychelles", "Singapore", "Solomon Is.", "Sri Lanka",
                         "Sudan", "Thailand", "Timor-Leste", "United Arab Emirates", "Tanzania", "Vanuatu", "Vietnam", "Yemen")


counties_suggested <- c("Bangladesh", "Iran", "Israel", "Kuwait", "Myanmar", "Oman", "Somalia", "Somaliland")

countries_confirmed_w <- world[world$name %in% countries_confirmed, ]
counties_suggested_w <- world[world$name %in%  counties_suggested, ]


map <- 
  # basemap
  ggplot2::ggplot(world) + 
  ggplot2::geom_sf(fill = "gray93", color = "grey20", size = 0.5) + 
  ggplot2::geom_sf(data = countries_confirmed_w, fill = "grey40", color = "grey20", size = 0.5) +
  ggplot2::geom_sf(data = counties_suggested_w, fill = "grey70", color = "grey20", size = 0.5) +
  ggplot2::coord_sf(xlim=c(34,165), ylim = c(28, -40)) + # limit to Indo-Pacific
  ggplot2::scale_x_continuous(breaks = seq(34, 165, by = 10)) + 
  ggplot2::scale_y_continuous(breaks = seq(-40, 28, by = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10)) + 
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10)) + 
  # our data as points
  ggplot2::geom_point(data = vid_dat_final_video, 
                      ggplot2::aes(approx_longitude, approx_latitude, size = number_video, color = number_video), alpha = 0.7) +
  ggplot2::scale_color_continuous(type = "viridis", direction = -1, breaks = seq(1, 12, length.out = 12), limits = c(1, 12)) +
  ggplot2::scale_size_continuous(breaks = seq(1, 12), range = c(3, 9)) +
  ggplot2::theme(axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_text(size = 15),
                 panel.background = ggplot2::element_rect(fill = "white"),
                 legend.position = "bottom") +
  ggplot2::guides(color = ggplot2::guide_legend("Number of videos"), size = ggplot2::guide_legend("Number of videos")) #to have one scale

map
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "map_videos_per_location.png"), map, width = 8, height = 6)




########maps number individuals measured per location (proportionals points) ---------------
vid_dat_final_ind_measured_loc <- vid_dat_final %>% 
  dplyr::group_by(approx_latitude, approx_longitude) %>% 
  dplyr::summarise(number_individual_measured = sum(number_individual_measured))

map <- 
  # basemap
  ggplot2::ggplot(world) + 
  ggplot2::geom_sf(fill = "gray93", color = "grey20", size = 0.5) + 
  ggplot2::geom_sf(data = countries_confirmed_w, fill = "grey40", color = "grey20", size = 0.5) +
  ggplot2::geom_sf(data = counties_suggested_w, fill = "grey70", color = "grey20", size = 0.5) +
  ggplot2::coord_sf(xlim=c(34,165), ylim = c(28, -40)) + # limit to Indo-Pacific
  ggplot2::scale_x_continuous(breaks = seq(34, 165, by = 10)) + 
  ggplot2::scale_y_continuous(breaks = seq(-40, 28, by = 10)) + 
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10)) + 
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10)) + 
  # our data as points
  ggplot2::geom_point(data = vid_dat_final_ind_measured_loc, 
                      ggplot2::aes(approx_longitude, approx_latitude, size = number_individual_measured, color = number_individual_measured), alpha = 0.7) +
  ggplot2::scale_color_continuous(type = "viridis", direction = -1, breaks = seq(1, 23, by = 2), limits = c(1, 23)) +
  ggplot2::scale_size_continuous(breaks = seq(1, 23, by = 2), range = c(3, 9)) +
  ggplot2::theme(axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_text(size = 9),
                 panel.background = ggplot2::element_rect(fill = "white"),
                 legend.position = "bottom") +
  ggplot2::guides(color = ggplot2::guide_legend("Number of individuals measured"), 
                  size = ggplot2::guide_legend("Number of individuals measured")) #to have one scale

map
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "map_individual_measured_per_location.png"), map, width = 8, height = 6)



########map number individuals measured per province admin (proportionals points) ---------------
load("dat_final.RData")
dat_final_province_admin <- dat_final %>% 
  dplyr::group_by(province_admin, lat_province_admin, lon_province_admin) %>%
  dplyr::summarise(number_individual = sum(number_individual)) %>% 
  dplyr::mutate_at(c('lat_province_admin', 'lon_province_admin'), as.numeric) 


map <- 
  # basemap
  ggplot2::ggplot(world) + 
  ggplot2::geom_sf(fill = "gray93", color = "grey20", size = 0.5) + 
  ggplot2::geom_sf(data = countries_confirmed_w, fill = "grey40", color = "grey20", size = 0.5) +
  ggplot2::geom_sf(data = counties_suggested_w, fill = "grey70", color = "grey20", size = 0.5) +
  ggplot2::coord_sf(xlim=c(34,165), ylim = c(28, -40)) + # limit to Indo-Pacific
  ggplot2::scale_x_continuous(breaks = seq(34, 165, by = 10)) + # graduations on x
  ggplot2::scale_y_continuous(breaks = seq(-40, 28, by = 10)) +  # graduations on y
  # our data as points
  ggplot2::geom_point(data = dat_final_province_admin, 
                      ggplot2::aes(lon_province_admin, lat_province_admin, size = number_individual, color = number_individual), alpha = 0.6) +
  ggplot2::scale_color_continuous(type = "viridis", direction = -1, breaks = seq(1, 32, by = 3), limits = c(1, 32)) +
  ggplot2::scale_size_continuous(breaks = seq(1, 32, by = 3), range = c(3, 9)) +
  ggplot2::theme(axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_text(size = 9),
                 panel.background = ggplot2::element_rect(fill = "white"),
                 legend.position = "bottom") +
  ggplot2::guides(color = ggplot2::guide_legend("Number of individuals measured"), 
                  size = ggplot2::guide_legend("Number of individuals measured")) #to have one scale

map
ggplot2::ggsave(here::here("outputs", "1-explore_video_table", "map_individual_measured_per_province_admin.png"), map, width = 8, height = 6)








