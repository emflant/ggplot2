

source('./geo_core.R')

raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
raster_rs = raster::stack(raster_filepath)
raster_df = raster::as.data.frame(x = raster_rs, xy = T)
# rs = raster::raster(raster_filepath)
raster_df |> head()
raster_rs

raster_bbox = raster::bbox(raster_rs)
raster_bbox |> as_tibble() |> 
  mutate(m1 = purrr::map2(min, max, c)) |> 
  mutate(m2 = purrr::map_dbl(m1, mean)) |> 
  pull(m2)

ggplot(raster_df) + 
  geom_raster(aes(x, y, fill = srtm)) +
  annotate("text", x = -113.0458, y = 37.3225, label = v_water_mark, na.rm = T,
            colour = "gray100",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  coord_fixed() +
  theme_bw(base_family = v_font_bold2) +
  theme(axis.title = element_blank(),
        plot.margin = margin(0.01,0.1,0.1,0.1,"in"))


ggsave(filename = "./2024/20240228/v2-01.png", 
       device = grDevices::png,
       width = 5, height = 3.8, dpi = 120, units = "in")


ggplot(raster_df) + 
  geom_raster(aes(x, y, fill = srtm)) +
  scale_fill_viridis_c()

ggplot(raster_df) + 
  geom_raster(aes(x, y, fill = srtm)) +
  annotate("text", x = -113.0458, y = 37.3225, label = v_water_mark, na.rm = T,
           colour = "gray100",size = 7, alpha = 0.3,
           family = v_font_heavy) +
  scale_fill_viridis_c(begin = 0) +
  coord_fixed() +
  theme_bw(base_family = v_font_bold2) +
  theme(axis.title = element_blank(),
        plot.margin = margin(0.01,0.1,0.1,0.1,"in"))

ggsave(filename = "./2024/20240228/v2-02.png", 
       device = grDevices::png,
       width = 5, height = 3.8, dpi = 120, units = "in") 


#######################################################################

raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
# terra::rast(raster_filepath)
terra_rs = terra::rast(raster_filepath)
terra_df = terra::as.data.frame(terra_rs, xy = T)

terra_rs
terra_df |> head()

ggplot() + 
  geom_raster(data = terra_df, aes(x, y, fill = srtm))  +
  annotate("text", x = -113.0458, y = 37.3225, label = v_water_mark, na.rm = T,
           colour = "gray100",size = 7, alpha = 0.3,
           family = v_font_heavy) +
  scale_fill_viridis_c(option = "B") +
  coord_fixed() +
  theme_bw(base_family = v_font_bold2) +
  theme(axis.title = element_blank(),
        plot.margin = margin(0.01,0.1,0.1,0.1,"in"))


ggsave(filename = "./2024/20240228/v2-03.png", 
       device = grDevices::png,
       width = 5, height = 3.8, dpi = 120, units = "in") 

#######################################################################

set.seed(1234)
my_raster = raster::raster(nrows = 10, ncols = 15,
               xmn = 0, xmx = 15, 
               ymn = 0, ymx = 10, 
               crs = "EPSG:5179",
               vals = sample(1:150, 150))

names(my_raster) = "values"
my_raster

# my_raster |> names()
df_my_raster = raster::as.data.frame(x = my_raster, xy = T)
df_my_raster |> head()

ggplot() + 
  geom_raster(data = df_my_raster, aes(x, y, fill = values))  +
  annotate("text", x = 7.5, y = 5, label = v_water_mark, na.rm = T,
           colour = "gray0",size = 7, alpha = 0.3,
           family = v_font_heavy) +
  scale_fill_viridis_c(option = "A", begin = 0.5) +
  scale_x_continuous(expand = expansion(0.05)) +
  scale_y_continuous(expand = expansion(0.075)) +
  coord_fixed() +
  theme_bw(base_family = v_font_bold2) +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        plot.margin = margin(0.01,0.1,0.1,0.1,"in"))

ggsave(filename = "./2024/20240228/v2-09.png", 
       device = grDevices::png,
       width = 5, height = 2.8, dpi = 180, units = "in") 
