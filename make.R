#Run the scripts in the following order
#Each script generates its specific outputs and RData objects (to be read by subsequent scripts)

source("1-explore_video_table_not_sensitive.R") #reads and explores the video metadata (NB: sensitive data on gepgraphic coordinates have been removed from the input file)

source("2-explore_measurement_tables.R") #compiles all measurements data, calculate body condition index and explore the results

source("3-preprocess_environmental_data.R") #pre-processes the environmental data (gdp per capita, gravity, mpas, seagrass, sst, turbidity) by creating intermediate RData objects. Note that some environmental datasets were stored on an external hard disk due to their large size.

source("4-process_environmental_data_gdp_per_capita.R") #processes the gdp data

source("4-process_environmental_data_gravity.R") #processes the gravity data

source("4-process_environmental_data_mpas_parallel.R") #processes the mpas data (requires parallel processing)

source("4-process_environmental_data_seagrass_parallel.R") #processes the seagrass data (requires parallel processing)

source("4-process_environmental_data_sst.R") #processes the sst data 

source("4-process_environmental_data_turbidity.R") #processes the turbidity data 

source("5-explore_mpas_details.R") #explore the processes mpas data

source("6-merge_explore_environmental_dat_final.R") #merge and explore the environmental variables

source("7-join_explore_environmental_mes_dat_final_img_without_interaction_double_re.R") #join the environmental variables to the measurements data and run GLMMs

source("7-join_explore_environmental_mes_dat_final_img_with_interaction_double_re.R") #join the environmental variables to the measurements data and run GLMMs including an interaction between gravity and country

source("8-statistical_tests.R") #run various statistical tests

source("9-test_sensibility_nb_pixels.R") #run sensitivity analyses

source("10-compare_bci_pixels_meters.R") #compare bci calculated from measurements in pixels and meters
