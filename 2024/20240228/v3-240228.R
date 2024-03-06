

source('./geo_core.R')


#######################################################################

raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
# terra::rast(raster_filepath)
terra_rs = terra::rast(raster_filepath)
terra_df = terra::as.data.frame(terra_rs, xy = T)

terra_rs
terra_df |> head()

ga = ggplot() + 
  geom_raster(data = terra_df, aes(x, y, fill = srtm))  +
  annotate("text", x = -113.0458, y = 37.3225, label = 'option = "A"', na.rm = T,
           colour = "gray100",size = 7, alpha = 0.6,
           family = v_font_heavy) +
  scale_fill_viridis_c(option = "A") +
  coord_fixed() +
  theme_bw(base_family = v_font_bold2) +
  theme(axis.title = element_blank(),
        legend.position = "none")


gb = ggplot() + 
  geom_raster(data = terra_df, aes(x, y, fill = srtm))  +
  annotate("text", x = -113.0458, y = 37.3225, label = 'option = "B"', na.rm = T,
           colour = "gray100",size = 7, alpha = 0.6,
           family = v_font_heavy) +
  scale_fill_viridis_c(option = "B") +
  coord_fixed() +
  theme_bw(base_family = v_font_bold2) +
  theme(axis.title = element_blank(),
        legend.position = "none")

gc = ggplot() + 
  geom_raster(data = terra_df, aes(x, y, fill = srtm))  +
  annotate("text", x = -113.0458, y = 37.3225, label = 'option = "C"', na.rm = T,
           colour = "gray100",size = 7, alpha = 0.6,
           family = v_font_heavy) +
  scale_fill_viridis_c(option = "C", begin = 0) +
  coord_fixed() +
  theme_bw(base_family = v_font_bold2) +
  theme(axis.title = element_blank(),
        legend.position = "none")


gd = ggplot() + 
  geom_raster(data = terra_df, aes(x, y, fill = srtm))  +
  annotate("text", x = -113.0458, y = 37.3225, label = 'option = "D"', na.rm = T,
           colour = "gray100",size = 7, alpha = 0.6,
           family = v_font_heavy) +
  scale_fill_viridis_c(option = "D", begin = 0) +
  coord_fixed() +
  theme_bw(base_family = v_font_bold2) +
  theme(axis.title = element_blank(),
        legend.position = "none")

ge = ggplot() + 
  geom_raster(data = terra_df, aes(x, y, fill = srtm))  +
  annotate("text", x = -113.0458, y = 37.3225, label = 'option = "E"', na.rm = T,
           colour = "gray100",size = 7, alpha = 0.6,
           family = v_font_heavy) +
  scale_fill_viridis_c(option = "E", begin = 0) +
  coord_fixed() +
  theme_bw(base_family = v_font_bold2) +
  theme(axis.title = element_blank(),
        legend.position = "none")

gf = ggplot() + 
  geom_raster(data = terra_df, aes(x, y, fill = srtm))  +
  annotate("text", x = -113.0458, y = 37.3225, label = 'option = "F"', na.rm = T,
           colour = "gray100",size = 7, alpha = 0.6,
           family = v_font_heavy) +
  scale_fill_viridis_c(option = "F", begin = 0) +
  coord_fixed() +
  theme_bw(base_family = v_font_bold2) +
  theme(axis.title = element_blank(),
        legend.position = "none")

gg = ggplot() + 
  geom_raster(data = terra_df, aes(x, y, fill = srtm))  +
  annotate("text", x = -113.0458, y = 37.3225, label = 'option = "G"', na.rm = T,
           colour = "gray100",size = 7, alpha = 0.6,
           family = v_font_heavy) +
  scale_fill_viridis_c(option = "G", begin = 0) +
  coord_fixed() +
  theme_bw(base_family = v_font_bold2) +
  theme(axis.title = element_blank(),
        legend.position = "none")


gh = ggplot() + 
  geom_raster(data = terra_df, aes(x, y, fill = srtm))  +
  annotate("text", x = -113.0458, y = 37.3225, label = 'option = "H"', na.rm = T,
           colour = "gray0",size = 7, alpha = 0.6,
           family = v_font_heavy) +
  scale_fill_viridis_c(option = "H", begin = 0) +
  coord_fixed() +
  theme_bw(base_family = v_font_bold2) +
  theme(axis.title = element_blank(),
        legend.position = "none")
# gh
gi = ggplot() + 
  geom_raster(data = terra_df, aes(x, y, fill = srtm))  +
  annotate("text", x = -113.0458, y = 37.3225, label = v_water_mark, na.rm = T,
           colour = "gray100",size = 7, alpha = 0.6,
           family = v_font_heavy) +
  scale_fill_gradient(low = "gray0", high = "gray100") +
  coord_fixed() +
  theme_bw(base_family = v_font_bold2) +
  theme(axis.title = element_blank(),
        legend.position = "none")


ga + gb + gc + gd + 
  ge + gf + gg + gh + gi + plot_layout(ncol = 3) 

ggsave(filename = "./2024/20240228/v3-01.png", 
       device = grDevices::png,
       width = 9, height = 9, dpi = 120, units = "in") 


ggsave(filename = "./2024/20240228/v3-02.png", 
       device = grDevices::png,
       width = 9, height = 9, dpi = 180, units = "in") 


ggsave(filename = "./2024/20240228/v3-03.png", 
       device = grDevices::png,
       width = 9, height = 9, dpi = 90, units = "in") 
