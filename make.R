
#Input data are located at:

#- data/Media_Sorting_Table_no_sensitive_data.xlsx: VIDEO DATA 
#table listing all videos of dugongs. 
#In this xlsx file, each line represents one video of dugongs and their associated metadata. 
#NB: The specific geographic locations of dugong videos have been excluded from the data as they represent sensitive information.

#- data/social_media: IMAGE DATA
#each subfolder includes individual csv files corresponding to images. 
#Each csv file is named after the name of the image it describes. In each of these csv files, 
#each line of the table represent a measurement of a dugong (l for straight length and w for maximum width)
#on the given image with associated information on image quality and individual class. 
#the column label includes the individual identifier.

#- data/envir_data: ENVIRONMENTAL DATA
#each subfolder represents the data for extracting the different environmental variables used in the analysis.
#data/region_rdata.ods contains geographical bounding boxes to ease the extraction of environmental data.


#To reproduce the analysis, run the scripts in the following order
#Note that each script generates its specific outputs and RData objects (to be read by subsequent scripts)


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
