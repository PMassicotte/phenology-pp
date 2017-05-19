files <- list.files("data/", "csv$", full.names = TRUE)

df <- read_csv("data/out2AM2015.csv", col_names = FALSE)

df <- setNames(df,
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

write_feather(df, "data/out2AM2015.feather")
