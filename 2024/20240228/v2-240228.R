

source('./geo_core.R')

raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
raster_filepath

rs = raster::stack(raster_filepath)
rs |> names()
data_rs = raster::as.data.frame(x = rs, xy = T)

ggplot() + 
  geom_raster(data = data_rs, aes(x, y, fill = srtm)) +
  coord_fixed() +
  theme_bw(base_family = v_font_bold2) +
  theme(axis.title = element_blank())


ggplot() + 
  geom_raster(data = data_rs, aes(x, y, fill = srtm)) +
  coord_fixed() +
  theme_bw(base_family = v_font_bold2) +
  theme(axis.title = element_blank())

# terra::rast(nrows = 6, ncols = 6, 
#                   xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
#                   vals = 1:36)

set.seed(1234)
my_raster = raster::raster(nrows = 6, ncols = 6,
               xmn = -3, xmx = 3, 
               ymn = -3, ymx = 3, 
               crs = "EPSG:5179",
               vals = sample(1:36, 36))

names(my_raster) = "values"
# my_raster |> names()
df_my_raster = raster::as.data.frame(x = my_raster, xy = T)
df_my_raster



ggplot() + 
  geom_raster(data = df_my_raster, aes(x, y, fill = values)) +
  scale_fill_viridis_c(option = "D", begin = 0.2) +
  coord_fixed()
  

