library(tidyverse)

setwd("W:/Alkor cruises/2024/April/Hydrografie abbildungen_AL610/")

source("angle2dec.R")



# set to folder with TOB files
input_folder <- "W:/Alkor cruises/2024/April/Data/CTD/TOB/"
# set to output folder or leave at default to save files in original folder
# be careful as default only works in ONLY TOB files are in folder
output_folder <- "W:/Alkor cruises/2024/April/Hydrografie abbildungen_AL610/"

#####################################################

{ # RUN HERE to run the whole script
  
  in_files <- list.files(input_folder)
  out_files <- gsub(".TOB", ".csv", in_files)
  
  #prepare column names
  col_names_df <- c("ID","Pressure..db","Temp..degC","Leitf..mS.cm","SALIN..ppt","AO2_%..%","AO2ml..ml.l","Bottm",
                    "date","time","Lat..Deg.N","Long..Deg.E","Sigma", "Sound", "RawO2..mV","T_o2"
  )
  
  col_names_master <- c("ID", "Pressure..db", "Temp..degC", "SALIN..ppt", 
                        "Day", "Month", "Year", "time", "Lat..Deg.N", 
                        "Long..Deg.E", "O2ml..ml.l", "Station")
  
  #prepare master sheet
  master <- data.frame(matrix(ncol = 15, nrow = 0))
  colnames(master) <- col_names_master
  
  n <- 0
  
  for (file in in_files){
    # keep count
    n = n+1
    print(paste("Processing:", in_files[n]))
    
    # read table
    df <- read.table(paste0(input_folder, "/", file),  skip =  43)
    
    # give meaningful column names
    names(df) <- col_names_df
    
    # reformatting date
    df <- df %>% separate(date, c("Day", "Month", "Year"))
    
    # reformatting lat and lon to dec
    long <- df$Long..Deg.E
    long_new <- c()
    for (long_sub in long){
      new_long_sub <- strtrim(long_sub, nchar(long_sub)-1)
      new_long_sub <- new_long_sub %>% 
        str_replace("(.{2})", "\\1 ") %>% 
        str_trim()
      long_new <- append(long_new, new_long_sub)
    }
    df$Long..Deg.E <- long_new
    df$Long..Deg.E <- angle2dec(df$Long..Deg.E)
    
    lat <- df$Lat..Deg.N
    lat_new <- c()
    for (lat_sub in lat){
      new_lat_sub <- strtrim(lat_sub, nchar(lat_sub)-1)
      new_lat_sub <- new_lat_sub %>% 
        str_replace("(.{2})", "\\1 ") %>% 
        str_trim()
      lat_new <- append(lat_new, new_lat_sub)
    }
    df$Lat..Deg.N <- lat_new
    df$Lat..Deg.N <- angle2dec(df$Lat..Deg.N)
    
    
    # subset data to only include useful columns
    df_useful <- subset(df, select = c("ID","Pressure..db","Temp..degC","Leitf..mS.cm","SALIN..ppt","AO2_%..%","AO2ml..ml.l",
                                       "Day", "Month", "Year","time","Lat..Deg.N","Long..Deg.E"))
    
    # add to master
    master <- rbind(master, df_useful)
    
    # save individual file as csv
    write.csv(df_useful, 
              file = paste0(output_folder, "/", out_files[n]), row.names = FALSE)
    
    print(paste("Finished. File saved as:", out_files[n]))
  }
  
  # save master as csv
  write.csv(master,
            file = paste0(output_folder, "/master.csv"), row.names = FALSE)
  print(paste("Done. Master file saved as: master.csv"))
}
