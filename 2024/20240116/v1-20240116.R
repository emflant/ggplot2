source('./geo_core.R')



# dokdo1 = read_sf('~/Documents/map/li_20230729/li.shp') |> 
#   mutate(LI_KOR_NM = iconv(LI_KOR_NM, from = "EUC-KR", to = 'UTF-8')) |> 
#   st_set_crs(5179) |> 
#   filter(str_detect(LI_KOR_NM, '독도리'))
  # ms_simplify(keep = 0.1, keep_shapes = T)  |> 
  

# dokdo2 = dokdo1 |> 
#   ms_simplify(keep = 0.001, keep_shapes = T)

dokdo_1 = get_dokdo(vkeep = 1)

center = dokdo_1 |> 
  st_geometry() |> 
  st_centroid() 
dokdo_watermark = st_set_geometry(dokdo_1, center)
# bb = st_bbox(dokdo_1)
# mean(c(bb[1], bb[3]))
# mean(c(bb[2], bb[4]))


# st_centroid(dokdo_1)

ggplot() + 
  geom_sf(data = dokdo_1) + 
  labs(title = "대한민국 독도") +
  geom_sf_text(data = dokdo_watermark, label = v_water_mark,
               colour = "gray0",size = 7, alpha = 0.3,
               family = v_font_heavy) +
  theme_bw(base_family = v_font_bold) +
  theme(title = element_text(size = 15),
        axis.title = element_blank())
  
ggsave(filename = "./2024/20240116/v01-01.png", 
       device = grDevices::png,
       width = 5, height = 5, dpi = 180, units = "in")



dokdo_0004 = get_dokdo(vkeep = 0.004)

center_0004 = dokdo_0004 |> 
  st_geometry() |> 
  st_centroid() 
dokdo_watermark_0004 = st_set_geometry(dokdo_0004, center_0004)

ggplot() + 
  geom_sf(data = dokdo_0004) +
  geom_sf_text(data = dokdo_watermark_0004, label = v_water_mark,
               colour = "gray0",size = 7, alpha = 0.3,
               family = v_font_heavy) +
  theme_bw(base_family = v_font_bold) +
  theme(title = element_text(size = 15),
        axis.title = element_blank())

ggsave(filename = "./2024/20240116/v01-02.png", 
       device = grDevices::png,
       width = 5, height = 4, dpi = 180, units = "in")

sig_map = get_map_sig()

get_center(sig_map)

get_center(dokdo_0004)

ggplot() + 
  geom_sf(data = sig_map) +
  geom_sf(data = dokdo_0004) +
  geom_point(data = dokdo_0004, aes(geometry = geometry), 
             shape = 1, size = 7, colour = "red",
             stat = "sf_coordinates") +
  geom_text(data = get_center(sig_map), aes(x,y), label = v_water_mark,
            colour = "gray0",size = 7, alpha = 0.5,
            family = v_font_heavy) +
  theme_bw(base_family = v_font_bold) +
  theme(title = element_text(size = 15),
        axis.title = element_blank())

ggsave(filename = "./2024/20240116/v01-04.png", 
       device = grDevices::png,
       width = 6, height = 5, dpi = 180, units = "in")


# dokdo = sig_map |> 
#   filter(str_detect(SIG_KOR_NM, '울릉군'))

geom_dokdo = st_geometry(dokdo_0004)
geom_dokdo_centroid = st_centroid(geom_dokdo)
geom_dokdo_scale = (geom_dokdo - geom_dokdo_centroid) * 12 + geom_dokdo_centroid
# geom_dokdo_scale
dokdo_scale = st_set_geometry(dokdo, geom_dokdo_scale) |> 
  st_set_crs(5179)


dokdo_scale = map_scale(dokdo_0004, scale = 12)

ggplot() + 
  geom_sf(data = sig_map) +
  geom_sf(data = dokdo_scale) +
  geom_point(data = dokdo_0004, aes(geometry = geometry), 
             shape = 1, size = 7, colour = "red",
             stat = "sf_coordinates") +
  geom_text(data = get_center(sig_map), aes(x,y), label = v_water_mark,
            colour = "gray0",size = 7, alpha = 0.5,
            family = v_font_heavy) +
  theme_bw(base_family = v_font_bold) +
  theme(title = element_text(size = 15),
        axis.title = element_blank())


ggsave(filename = "./2024/20240116/v01-05.png", 
       device = grDevices::png,
       width = 6, height = 5, dpi = 180, units = "in")


ggplot() + 
  geom_sf(data = dokdo) +
  coord_sf(crs = 'EPSG:4326', xlim = c(131.86, 131.88),
           ylim = c(37.23, 37.25))
