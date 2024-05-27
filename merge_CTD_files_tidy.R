library(tidyverse)

# get all files ending in "TOB"
files <- dir(pattern = "TOB")

# my imagination of what "angle2dec" is supposed to do
# (take a character in the form of "xxxx.xxx[E/N]" and
# convert it into y.yyyy, where .yy = xx.xx/60 )
# !!! careful - not taking E/W or N/S into account!!!
angle2dec_kh <- \(ang){
  tibble(str = str_replace(ang, "([0-9*])([0-9]{2}.[0-9*]).*","\\1,\\2")) |> 
    separate(str, into = c("deg","min"), sep = ",", convert = TRUE) |> 
    mutate(deg = deg + min/60) |> 
    pluck("deg")
} 

#prepare column names
col_names_df <- c("ID","Pressure..db","Temp..degC","Leitf..mS.cm","SALIN..ppt","AO2_%..%","AO2ml..ml.l","Bottm",
                  "date","time","Lat..Deg.N","Long..Deg.E","Sigma", "Sound", "RawO2..mV","T_o2")

# wrap tasks for table import and conversion into dedicated function
read_tob <- \(file, out_path = "."){
  # logging message
  cat(paste("Processing:", file, "\\n"))
  # read in TOB file
  data_current <- read_table(file,
                             skip = 43,
                             col_names = col_names_df,
                             col_types = "ddddddddttccdddd") |> 
    # separate the date column
    # !!! I believe this is a bug - I think the column does not contain DATE but TIME - double check this !!!
    separate(date, into = c("Day", "Month", "Year")) |> 
    # convert long/lat to decimal format
    mutate( across(c(`Long..Deg.E`, `Lat..Deg.N`), angle2dec_kh),
            file = file ) |> 
    # select desired columns (as in "df_useful")
    select( ID, `Pressure..db`, `Temp..degC`, `Leitf..mS.cm`, 
            `SALIN..ppt`, `AO2_%..%`, `AO2ml..ml.l`, Day,
            Month, Year, time, `Lat..Deg.N`, `Long..Deg.E`, file)
  
  # convert input file name to "csv" (change suffix)
  out_file <- str_replace(file, "TOB$", "csv")
  # export converted singel file
  write_csv(data_current, file = str_c(out_path, "/", out_file))
  # logging message
  cat(paste("Finished. File saved as:", out_file, "\\n"))
  # return the converted data frame
  # (to be able to merge it into the master table)
  return(data_current)
}

# set global output folder (here: current working directory)
output_folder <- "."

# loop import function over files and merge
# resulting tables by rows to create master table
data_master <- files |> map_dfr(read_tob, out_path = output_folder)

# save master as csv
write_csv(data_master, file = paste0(output_folder, "/master.csv"))
# logging message
print(paste("Done. Master file saved as: master.csv"))
