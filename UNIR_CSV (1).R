library("dplyr")                                    # Load dplyr package
library("plyr")                                     # Load plyr package
library("readr") 

dirs <- file.path(getwd(), "raw_dataA")
dirs # This is the path were the codes will be stored
dir.create(dirs) # Function for creating the directory where each dataset will be saved
data_all <- list.files(path = "/raw_dataA",  # Identify all CSV files
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  rbind.fill() 

data_all                                            # Print data to RStudio console

readr::write_csv(data_all, file.path(dirs, "BIEN.csv"))
