rm(list=ls())
library(magrittr)




#data we want to extract gdp per capita
#https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?locations=NC&view=map 

#---------------------------------------------- load dugong observation data ----------------------------------------------------------------------

load("dat_final.RData")





#---------------------------------------------- load gdp file  ---------------------------------------------------------------------- 

directory <- here::here("data/envir_data/gdp/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_133.csv")

dat_gdp_per_capita <- readr::read_csv(directory, skip = 4)



#------------------------------------------------------------------------------------------------------------
#rename Country Name in dat_gdp_per_capita as country_gdp to take left join with column country in dat_final 
dat_gdp_per_capita <- dat_gdp_per_capita %>% 
  dplyr::rename(country_gdp = "Country Name") %>% 
  dplyr::select('country_gdp', `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`) %>% 
  dplyr::rename(gdp_2016 = `2016`,
                gdp_2017 = `2017`,
                gdp_2018 = `2018`,
                gdp_2019 = `2019`,
                gdp_2020 = `2020`,
                gdp_2021 = `2021`,
                gdp_2022 = `2022`)

#left join on country_gdp and assign "France" GDP to Mayotte individual 
dat_final_gdp_per_capita <- dat_final %>%
  dplyr::mutate(country_gdp = dplyr::if_else(country == "Mayotte", "France", country)) %>%
  dplyr::left_join(dat_gdp_per_capita, by = c("country_gdp")) %>% 
  dplyr::mutate(year = substr(most_precise_date, 1, 4))


#mean of gdp accross year and assign correct gdp
dat_final_gdp_per_capita <- dat_final_gdp_per_capita %>%
  dplyr::rowwise() %>%
  dplyr::mutate(gdp_mean_2016_2022 = mean(c(gdp_2016, gdp_2017, gdp_2018, gdp_2019, gdp_2020, gdp_2021, gdp_2022),  na.rm = TRUE)) %>% 
  dplyr::mutate(gdp_per_capita = dplyr::case_when(
    year == 2016 ~ gdp_2016,
    year == 2017 ~ gdp_2017,
    year == 2018 ~ gdp_2018,
    year == 2019 ~ gdp_2019,
    year == 2020 ~ gdp_2020,
    year == 2021 ~ gdp_2021,
    year == 2022 ~ gdp_2022,
    year %in% c(2023, 2024) ~ gdp_mean_2016_2022,
    TRUE ~ NA_real_  
  )) %>% 
  dplyr::select(-c(gdp_2016, gdp_2017, gdp_2018, gdp_2019, gdp_2020, gdp_2021, gdp_2022, gdp_mean_2016_2022, country_gdp, year))



save(dat_final_gdp_per_capita, file = "dat_final_gdp_per_capita.RData")


