source('./geo_core.R')
# ggplot2 - 경도/위도값을 EPSG:5179 좌표계로 변환하기

map_nocrs = read_sf('~/Documents/map/sig_20230729/sig.shp') |> 
  mutate(SIG_KOR_NM = iconv(SIG_KOR_NM, from = "EUC-KR", to = 'UTF-8')) |> 
  ms_simplify(keep = 0.001, keep_shapes = T)

bb = st_bbox(map_nocrs)  
bb
xmean = mean(c(bb[1], bb[3]))
ymean = mean(c(bb[2], bb[4]))
st_crs(map_nocrs)

map_5179 = map_nocrs |> 
  st_set_crs(5179)

st_bbox(map_5179)  
st_crs(map_5179)

# map2 = map1 |> 
#   
#   ms_simplify(keep = 0.001, keep_shapes = T) |> 
#   st_set_crs(5179)
# 
# map3 = map1 |> 
#   mutate(SIG_KOR_NM = iconv(SIG_KOR_NM, from = "EUC-KR", to = 'UTF-8')) |> 
#   ms_simplify(keep = 0.001, keep_shapes = T) 
geom_text
map_nocrs
ggplot(map_nocrs) + 
  geom_sf() +
  annotate("text", x = xmean, y = ymean, alpha = 0.7,
           label = v_water_mark, colour = "gray0", size = 7,
           family = "AppleSDGothicNeo-Heavy") +
  labs(title = "(지도 1-1) CRS = NA") +
  theme_bw(base_family = v_font_bold) +
  theme(axis.title = element_blank()) +

ggplot(map_5179) + 
  geom_sf() +
  annotate("text", x = xmean, y = ymean, alpha = 0.7,
           label = v_water_mark, colour = "gray0", size = 7,
           family = "AppleSDGothicNeo-Heavy") +
  labs(title = "(지도 1-2) CRS = EPSG:5179") +
  theme_bw(base_family = "AppleSDGothicNeo-Bold") +
  theme(axis.title = element_blank()) +
  plot_layout(ncol = 2)

# g1 + g2 + plot_layout(ncol = 2)

ggsave(filename = "./2024/20240113/v01-01.png", 
       device = grDevices::png,
       width = 10, height = 5, dpi = 180, units = "in")





ggplot(map_5179) + 
  geom_sf() +
  coord_sf(datum = st_crs(4326)) +
  annotate("text", x = 1050000, y = 1800000, alpha = 0.7,
           label = v_water_mark, colour = "gray0", size = 7,
           family = "AppleSDGothicNeo-Heavy") +
  labs(title = "(지도 2-1) datum = 4326") +
  theme_bw(base_family = "AppleSDGothicNeo-Bold") +
  theme(axis.title = element_blank()) +
ggplot(map_5179) + 
  geom_sf() +
  coord_sf(datum = st_crs(5179)) +
  annotate("text", x = 1050000, y = 1800000, alpha = 0.7,
           label = v_water_mark, colour = "gray0", size = 7,
           family = "AppleSDGothicNeo-Heavy") +
  labs(title = "(지도 2-2) datum = 5179") +
  theme_bw(base_family = "AppleSDGothicNeo-Bold") +
  theme(axis.title = element_blank()) +
  plot_layout(ncol = 2)

ggsave(filename = "./2024/20240113/v01-02.png", 
       device = grDevices::png,
       width = 10, height = 5, dpi = 180, units = "in")




ggplot(map_5179) + 
  geom_sf() +
  coord_sf(crs = st_crs(5179)) +
  annotate("text", x = 1050000, y = 1800000, alpha = 0.7,
           label = v_water_mark, colour = "gray0", size = 7,
           family = "AppleSDGothicNeo-Heavy") +
  labs(title = "(지도 3-1) crs = 5179") +
  theme_bw(base_family = "AppleSDGothicNeo-Bold") + 
  theme(axis.title = element_blank()) +
ggplot(map_5179) + 
  geom_sf() +
  coord_sf(crs = st_crs(4326)) +
  annotate("text", x = 128, y = 36.1, alpha = 0.7,
           label = v_water_mark, colour = "gray0",size = 7,
           family = "AppleSDGothicNeo-Heavy") +
  labs(title = "(지도 3-2) crs = 4326") +
  theme_bw(base_family = "AppleSDGothicNeo-Bold") +
  theme(axis.title = element_blank()) +
  plot_layout(ncol = 2)
  
ggsave(filename = "./2024/20240113/v01-03.png", 
       device = grDevices::png,
       width = 10, height = 5, dpi = 180, units = "in")





ggplot(map_5179) + 
  geom_sf() +
  coord_sf(crs = st_crs(5179)) +
  annotate("text", x = xmean, y = ymean, alpha = 0.5,
           label = v_water_mark, colour = "gray0", size = 5,
           family = "AppleSDGothicNeo-Heavy") +
  labs(title = "(지도 3-1) crs = 5179") +
  theme_void(base_family = "AppleSDGothicNeo-Bold") +
  theme(axis.title = element_blank(),
        panel.grid = element_line()) 

ggsave(filename = "./2024/20240113/v01-04.png", 
       device = grDevices::png,
       width = 6, height = 5, dpi = 180, units = "in")


# projected 값을 경도/위도로 바꾸기.
mpoint = tibble(x = xmean, y = ymean) |> 
  st_as_sf(coords = c("x", "y")) |> 
  st_set_crs("EPSG:5179") |> 
  st_transform("EPSG:4326")

st_bbox(mpoint)

map_5179
ggplot(map_5179) + 
  geom_sf() +
  coord_sf(crs = st_crs(4326)) +
  annotate("text", x = 127.77198, y = 35.90002, alpha = 0.5,
           label = v_water_mark, colour = "gray0", size = 5,
           family = "AppleSDGothicNeo-Heavy") +
  labs(title = "(지도 3-2) crs = 4326") +
  theme_void(base_family = "AppleSDGothicNeo-Bold") +
  theme(axis.title = element_blank(),
        panel.grid = element_line()) 


ggsave(filename = "./2024/20240113/v01-05.png", 
       device = grDevices::png,
       width = 6, height = 5, dpi = 180, units = "in")

st_bbox(map_5179)


xmean

# projected 값을 경도/위도로 바꾸기.
tibble(x = xmean, y = ymean) |> 
  st_as_sf(coords = c("x", "y")) |> 
  st_set_crs("EPSG:5179") |> 
  st_transform("EPSG:4326")

# 경도 위도를 EPSG:5179 거리체계로 
# 위도 : latitude 가로줄, y축.
# 경도 : longitude 세로줄, x축.
lim_data = tibble(lon = c(126, 127), lat = c(36, 37)) |> 
  st_as_sf(coords = c("lon", "lat")) |> 
  st_set_crs("EPSG:4326") |> 
  st_transform("EPSG:5179")

lim_data
tibble(lon = c(126, 127), lat = c(36, 37)) |> 
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") 

tibble(lon = c(126, 127), lat = c(36, 37)) |> 
  st_as_sf(coords = c("lon", "lat")) |> 
  st_set_crs("EPSG:4326") 


