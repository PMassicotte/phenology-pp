rm(list = ls())

dat <- read_feather("data/out2AM2015.feather")

dat <- dat %>% 
  filter(!is.nan(pp)) %>% ## Check with Eric why some pp have NaN value (comes from Matlab interpolation?)
  group_by(lat, lon) %>% 
  nest()


# filter data -------------------------------------------------------------

n <- 943
df <- dat$data[[n]]

df %>% 
  ggplot(aes(x = ddd, y = pp)) +
  geom_point(aes(color = bloom_title)) +
  geom_point(data = filter(df, pp_flag == 0), aes(x = ddd, y = pp))

## Remove boundary pixels that were estimated by quadratic function

keep_valid_range <- function(df) {
  
  df <- df %>% 
    filter(ddd >= min(ddd[pp_flag == 0]) & ddd <= max(ddd[pp_flag == 0]))
  
  return(df)
  
}

## Make sure we have at least 50 days between "real" pp observation. This is
## done to avoid interpolation on too many points.

max_inteprolated_days <- function(df) {
  
  df <- df %>% 
    filter(pp_flag == 0) %>% 
    mutate(max_day = dplyr::lead(ddd) - ddd) 
  
  max_day <- max(df$max_day, na.rm = TRUE)
  
  return(max_day)
  
}

## Process 
dat <- dat %>% 
  mutate(data =  map(data, keep_valid_range)) %>% 
  mutate(max_day = map(data, max_inteprolated_days)) %>% 
  unnest(max_day)

dat %>%
  ggplot(aes(x = max_day)) +
  geom_histogram(binwidth = 1) +
  labs(title = paste0(stringr::str_wrap("Maximum number of interpolated PP values between two real observations by pixel.", 70), "\n"),
       subtitle = "For year 2015 (binwidth = 1)") +
  scale_x_continuous(breaks = round(seq(0, 360, by = 20)))

ggsave("graphs/max_number_of_interpolated_days.pdf")
