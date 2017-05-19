# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Convert all CSV files produced by Eric in Matlab into feather
#               format which is much faster to work with.
#               
#               Data are from: 
#               taku-eirikr.takuvik.ulaval.ca/Volumes/output-prod/Takuvik/Teledetection/Couleur/SORTIES/35_3_2/NOCLIM/TimeSeries
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

csv_to_feather <- function(file) {
  
  df <- read_csv(file, col_names = FALSE)
  
  df <- setNames(
    df,
    c(
      "yyyy",
      "pixel",
      "lat",
      "lon",
      "ddd",
      "daylength",
      "ice_conc",
      "bloomstate",
      "chl",
      "chl_flag",
      "pp",
      "pp_flag",
      "province"
    )
  ) 
  
  bloom <- c(
    "0" = "prebloom",
    "10" = "prebloom",
    "1" = "bloom",
    "11" = "bloom",
    "2" = "postbloom",
    "12" = "postbloom",
    "4" = "winter",
    "14"  = "winter",
    "5" = "fall_bloom",
    "15" = "fall_bloom",
    "3" = "postbloom_scm"
  )
  
  df <- mutate(df, bloom_title = bloom[as.character(bloomstate)])
  
  destfile <- paste0(tools::file_path_sans_ext(file), ".feather")
  
  write_feather(df, destfile)
  
}

## Decompress files
files <- list.files("data/", "csv.gz", full.names = TRUE)
walk(files, R.utils::gunzip)

## Resave to feather
files <- list.files("data/", "csv$", full.names = TRUE)
walk(files, csv_to_feather)
