rm(list = ls())

ice <-
  read_csv(
    "data/Trend_Growing_Season_1998-2015_Zhang_Full_Details.csv",
    col_names = c(
      "lon",
      "lat",
      "lbound",
      "trend",
      "trendp",
      "ubound",
      "tau",
      "sig",
      "nruns",
      "autocor",
      "valid_frac",
      "linear",
      "intercept"
    )
  )


shp <- rgdal::readOGR("data/Carte WWF source/", "RACER_Study_Units_160210_Mar")

ice <- SpatialPointsDataFrame(cbind(ice$lon, ice$lat), data = ice, proj4string = CRS("+init=epsg:4326"))
ice <- spTransform(ice, CRS(proj4string(shp)))

res <- sp::over(ice, shp)

ice_anomaly <- bind_cols(data.frame(ice), res) %>% 
  drop_na(Name) %>% 
  janitor::clean_names() %>%
  as_tibble()


# Stats by region ---------------------------------------------------------

ice_anomaly %>% 
  mutate(trend = ifelse(is.na(trend), 0, trend)) %>% 
  group_by(name) %>% 
  summarise(
    mean_trend = mean(trend),
    median_trend = median(trend),
    n = n()
  ) %>% 
  write_csv("data/ice_anomaly_stats_by_region.csv")

# anomaly by region -------------------------------------------------------

ice_anomaly2 <- ice_anomaly %>%
  mutate(trend = ifelse(is.na(trend), 0, trend)) %>%
  SpatialPointsDataFrame(cbind(.$lon, .$lat),
                         data = .,
                         proj4string = CRS("+init=epsg:4326")) %>%
  spTransform(CRS(proj4string(shp))) %>%
  as_data_frame()

wmap <- rworldmap::getMap(resolution = "high")
wmap <- spTransform(wmap, CRS = proj4string(shp))
wmap <- fortify(wmap)

grat <- readOGR("/media/work/postdoctorat/denmark/project cdoc/cdoc/dataset/shapefiles/ne_110m_graticules_all/", 
                layer = "ne_110m_graticules_15") 
grat <- spTransform(grat, CRS = proj4string(shp))
grat_df <- fortify(grat)

mycol <- c("blue4", "blue3", "blue2", "white",              # minus
           "red", "red2", "red4")                          # plus

p <- wmap %>%
  ggplot(aes(x = long, y = lat, group = group))  +
  geom_polygon(fill = "gray75") +
  coord_fixed(xlim = c(-4200000, 3000000),
              ylim = c(-3000000, 4500000)) +
  geom_point(
    data = filter(ice_anomaly2, trend != 0),
    aes(x = coords.x1, y = coords.x2, color = trend),
    alpha = 1/5,
    inherit.aes = FALSE,
    size = 0.01
  ) +
  geom_path(data = shp, aes(x = long, y = lat), color = "black", size = 0.25) +
  scale_colour_gradientn(
    colours = mycol[-1],
    breaks = c(-30, -20, 0, 20, 40),
    guide = guide_colorbar(nbin = 255, direction = "horizontal")
  ) +
  geom_path(
    data = grat_df,
    aes(long, lat, group = group, fill = NULL),
    lty = 2,
    color = "grey50",
    size = 0.1
  ) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(plot.background = element_rect(fill = "white")) +
  theme(legend.position = "top") +
  xlab("Longitude") +
  ylab("Longitude") +
  labs(color = bquote("Open water period trends"~(d%*%y^{-1})))  +
  theme(text = element_text(family = "Helvetica", size = 14)) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("graphs/fig1a.pdf")
