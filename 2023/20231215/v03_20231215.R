
# Sys.setlocale("LC_ALL", "ko_KR.UTF-8")
korea = read_sf('~/Documents/map/sig_20230729/sig.shp')

korea2 = korea |> 
  st_set_crs(5179) |> 
  mutate(SIG_KOR_NM = iconv(SIG_KOR_NM, from = "EUC-KR", to = 'UTF-8')) |> 
  mutate(CTPRVN_CD = str_sub(SIG_CD, 1,2), .before = SIG_CD)


# 경도 위도를 EPSG:5179 거리체계로 
# 위도 : latitude 가로줄, y축.
# 경도 : longitude 세로줄, x축.
tibble(lon = c(126, 127), lat = c(36, 37)) |> 
  st_as_sf(coords = c("lon", "lat")) |> 
  st_set_crs("EPSG:4326") |> 
  st_transform("EPSG:5179")

korea2_sim = ms_simplify(korea2, keep = 0.01, keep_shapes = T)
korea2_sim


# ggplot(korea2_sim) +
#   geom_sf()



sido_info = read_sf('~/Documents/map/sido_20230729/ctprvn.dbf') |> 
  st_drop_geometry() |> 
  mutate(CTP_KOR_NM = iconv(CTP_KOR_NM, from = "EUC-KR", to = 'UTF-8'))

korea3 = korea2_sim |> left_join(sido_info) |> 
  select(CTPRVN_CD, CTP_ENG_NM, CTP_KOR_NM, SIG_CD, SIG_ENG_NM, SIG_KOR_NM, geometry)
# korea3 |> distinct(CTPRVN_CD, CTP_KOR_NM)
# korea3 |> 
#   filter(CTPRVN_CD %in% c('11', '28', '41', '43'))


  

ggplot(korea3 |> filter(CTPRVN_CD %in% c('11', '28', '41', '43', '51'))) +
  geom_sf(aes(fill = CTP_KOR_NM)) +
  scale_fill_brewer(palette = "YlGnBu") +
  # scale_fill_manual() +
  coord_sf(crs = sf::st_crs(4326),
           xlim = c(126, 128), ylim = c(37, 38)) +
  theme_bw(base_family = 'BMJUAOTF') +
  theme(legend.title = element_blank())

ggsave(filename = "./2023/20231215/v03_20231215-02.png", 
       device = grDevices::png,
       width = 8, height = 4, dpi = 180, units = "in")

ggsave(filename = "./2023/20231215/v03_20231215-03.png", 
       device = grDevices::png,
       width = 8, height = 4, dpi = 180, units = "in")

ggsave(filename = "./2023/20231215/v03_20231215-04.png", 
       device = grDevices::png,
       width = 8, height = 4, dpi = 240, units = "in")


sido_info
