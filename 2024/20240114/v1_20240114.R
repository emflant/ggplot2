source('./geo_core.R')

dokdo = get_dokdo()

ggplot()+
  geom_sf(data = dokdo)
sig_map = get_map_sig(vkeep = 0.05)

# sig_map |> st_crs()

ggplot() + 
  geom_sf(data = sig_map) 

# Simple feature geometries (sfg) 클래스.
st_point(c(128, 37))

# Simple feature columns (sfc) 클래스.
st_point(c(128, 37)) |> 
  st_sfc(crs = "EPSG:4326")

# Simple feature (sf) 클래스.
st_point(c(128, 37)) |> 
  st_sfc(crs = "EPSG:4326") |> 
  st_sf(geometry = _) 

st_point(c(128, 37))

# Simple feature (sf) 클래스.
# t1 = tibble(n = 1:2, lon = c(128, 125), lat = c(37, 35))
tibble(lon = c(128, 125), lat = c(37, 35)) |> 
  mutate(x = map2(lon, lat, c)) |> 
  mutate(geom = map(x, st_point)) |> 
  pull(geom) |> 
  st_sfc(crs = "EPSG:4326")  |> 
  st_sf(geometry = _) 


data.frame(lon = c(127, 129), lat = c(37, 36)) |> 
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326")

world
geom
pull
sum(1, 2, 3)

st_point(c(128, 37)) |> 
  st_sfc(crs = "EPSG:4326") |> 
  st_sf(geometry = _) |> 
  as_tibble()
a
data.frame(lon = 128, lat = 37) |> 
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326")

tibble(lon = 128, lat = 37) |> 
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") |> class()

tibble(lon = 128, lat = 37) |> 
  st_as_sf(coords = c("lon", "lat")) |> 
  st_set_crs("EPSG:4326")

length(colours())

identity(12)

points = data.frame(lon = c(127, 129), lat = c(37.5, 35.5)) |> 
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") 
# points
# sig_map
# st_bbox(sig_map)

xmid = mean(c(st_bbox(sig_map)$xmin, st_bbox(sig_map)$xmax))
ymid = mean(c(st_bbox(sig_map)$ymin, st_bbox(sig_map)$ymax))
# sig_map
ggplot() +
  geom_sf(data = sig_map) +
  geom_sf(data = points) +
  annotate("text", x = xmid, y = ymid, alpha = 0.5,
           label = v_water_mark, colour = "gray0",size = 7,
           family = v_font_heavy) +
  theme_bw() +
  theme(axis.title = element_blank())



ggsave(filename = "./2024/20240114/v01-01.png", 
       device = grDevices::png,
       width = 6, height = 5, dpi = 180, units = "in")



ggplot() +
  geom_sf(data = sig_map) +
  geom_sf(data = points, size = 3, shape = 1, colour = "blue") +
  annotate("text", x = xmid, y = ymid, alpha = 0.5,
           label = v_water_mark, colour = "gray0",size = 7,
           family = v_font_heavy) +
  theme_bw() +
  theme(axis.title = element_blank())

ggsave(filename = "./2024/20240114/v01-02.png", 
       device = grDevices::png,
       width = 6, height = 5, dpi = 180, units = "in")



ggplot() +
  geom_sf(data = sig_map) +
  geom_sf(data = points, size = 3, shape = 1, colour = "blue") +
  geom_point(data = points, aes(geometry = geometry),
             shape = 1, size = 3, colour = "blue",
             stat = "sf_coordinates") +
  annotate("text", x = xmid, y = ymid, alpha = 0.5,
           label = v_water_mark, colour = "gray0",size = 7,
           family = v_font_heavy) +
  theme_bw() +
  theme(axis.title = element_blank())

ggsave(filename = "./2024/20240114/v01-03.png", 
       device = grDevices::png,
       width = 6, height = 5, dpi = 180, units = "in")

ggplot() + 
  geom_sf(data = sig_map, linewidth = 0.3, 
          linetype = "solid", colour = "gray0") +
  geom_point(data = p1, aes(geometry = geometry), 
             shape = 1, size = 3, colour = "blue",
             stat = "sf_coordinates")
geom_segment
stat_sf_coordinates(data = p1, aes(geometry = geometry))
# rm(p)

