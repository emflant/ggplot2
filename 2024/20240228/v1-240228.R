source('./core.R')
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")

my_rast = terra::rast(raster_filepath)

my_rast_df = terra::as.data.frame(my_rast, xy = T)
str(my_rast_df)
plot(my_rast)


my_rast
ggplot(my_rast_df) +
  geom_raster(aes(x,y, fill = srtm)) 


geom_raster()
geom_st
stack()

rs = raster::stack(raster_filepath)
rs
rs |> raster::as.data.frame(x = _, xy = T) |> lengths()
rs |> raster::as.data.frame(x = _, xy = T)
rs |> raster::as.data.frame(x = _, xy = T)



data_rs = raster::as.data.frame(x = rs, xy = T)
data_rs
ggplot() + 
  geom_raster(data = data_rs, aes(x, y, fill = srtm)) +
  coord_fixed() +
  theme_bw()


ggplot() + 
  geom_raster(data = data_rs, aes(x, y, fill = srtm)) +
  coord_fixed() +
  theme_bw()


RasterStack
df <- data.frame(x = 1:10, y = (1:10)^2)
df
ggplot(df, aes(x, y)) +
  # geom_point() +
  stat_function(fun = ~ .x^2)


ggplot(tibble(x = -3:3), aes(x)) +
  # geom_point() +
  stat_function(fun = ~ .x^2) +
  coord_fixed() +
  theme_bw()

ggplot(tibble(x = -3:3, r = 1), aes(x)) +
  # geom_point() +
  stat_function(fun = ~ .x^2, n = 11) +
  coord_fixed() +
  theme_bw()


