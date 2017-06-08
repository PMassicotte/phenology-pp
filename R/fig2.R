rm(list = ls())

files <- list.files("data/clean/", "merged", full.names = TRUE)

df <- map(files, read_feather) %>% 
  set_names(basename(files)) %>% 
  bind_rows(.id = "file")

p <- df %>% 
  filter(ice_opening_duration >= 0 & ice_opening_duration <= 300) %>% 
  ggplot(aes(x = ice_opening_duration, y = yearly_pp)) +
  geom_point(aes(color = lat)) +
  facet_grid(name~file, scales = "free") +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  geom_smooth(method = "lm") +
  viridis::scale_color_viridis()

ggsave("graphs/ice_opening_vs_yearly_pp.png", width = 40, height = 30)

p <- df %>% 
  filter(ice_opening_duration >= 0 & ice_opening_duration <= 300) %>% 
  mutate(year = stringr::str_extract(file, "\\d{4}")) %>% 
  ggplot(aes(x = ice_opening_duration, y = yearly_pp / 1000, color = year)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~name, scales = "free") +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  geom_smooth(method = "lm") 

ggsave("graphs/ice_opening_vs_yearly_pp2.png", width = 12, height = 8)


# fig2 --------------------------------------------------------------------

dat <- df %>% 
  filter(ice_opening_duration >= 10 & ice_opening_duration <= 300) %>% 
  mutate(year = stringr::str_extract(file, "\\d{4}")) %>% 
  mutate(year = parse_number(year)) %>% 
  # filter(year == 2015) %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(mod = map(data, ~lm(.$yearly_pp ~ .$ice_opening_duration -1))) %>% 
  mutate(param = map(mod, broom::tidy)) %>% 
  # mutate(slope = map(mod, function(x){coef(x)[2]})) %>% 
  # unnest(slope) %>% 
  mutate(pred = map2(data, mod, modelr::add_predictions)) 

length(which(dat$slope < 0))

p <- dat %>% 
  filter(name != "White Sea") %>% 
  unnest(pred) %>% 
  ggplot(aes(x = ice_opening_duration, y = pred / 1000, color = name)) +
  geom_line() +
  xlab("Duration of growing season (days)") +
  ylab(bquote(Annual~primary~production~(g%*%C%*%m^{-2}%*%y^{-1}))) +
  labs(color = "Arctic regions") +
  guides(color = guide_legend(ncol = 1)) +
  theme(text = element_text(family = "Helvetica", size = 14)) 
  
ggsave("graphs/fig2.pdf", width = 12, height = 8)


# fig2b -------------------------------------------------------------------

shp <- rgdal::readOGR("data/Carte WWF source/", "RACER_Study_Units_160210_Mar") 

grat <- readOGR("/media/work/postdoctorat/denmark/project cdoc/cdoc/dataset/shapefiles/ne_110m_graticules_all/", 
                layer = "ne_110m_graticules_15") 

grat <- spTransform(grat, CRS = proj4string(shp))
grat_df <- fortify(grat)

wmap <- rworldmap::getMap(resolution = "high")
wmap <- spTransform(wmap, CRS = proj4string(shp))
wmap <- fortify(wmap)

p <- shp %>% 
  fortify(region = "Name") %>% 
  filter(id != "White Sea") %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = id)) +
  geom_polygon(color = "black", size = 0.25) +
  geom_polygon(data = wmap, aes(x = long, y = lat, group = group), inherit.aes = FALSE, fill = "gray75") +
  geom_path(
    data = grat_df,
    aes(long, lat),
    lty = 2,
    color = "grey50",
    size = 0.1, 
    inherit.aes = FALSE
  ) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(plot.background = element_rect(fill = "white")) +
  # theme(legend.position = "top") +
  xlab("Longitude") +
  ylab("Longitude") +
  labs(fill = "Arctic regions") +
  theme(text = element_text(family = "Helvetica", size = 14)) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_fixed(xlim = c(-4200000, 3000000),
              ylim = c(-3000000, 4500000)) +
  guides(fill = guide_legend(ncol = 1)) 

ggsave("graphs/fig2b.pdf", width = 12, height = 8)
