source('./geo_core.R')

sido_map = read_sf('~/Documents/map/sido_20230729/ctprvn.shp') |> 
  st_set_crs(5179) |> 
  mutate(CTP_KOR_NM = iconv(CTP_KOR_NM, from = "EUC-KR", to = 'UTF-8'))

# sido_map

sido_map_sim = ms_simplify(sido_map, keep = 0.01, keep_shapes = T)
sido_map_sim
# 11, 28, 41, 43, 
# sido_map_sim2 = sido_map_sim |> 
#   filter(CTPRVN_CD %in% c('11', '28', '41', '43', '51'))
sido_map_sim
ggplot(sido_map_sim) +
  geom_sf()
# sido_map_sim
ggplot(sido_map_sim) +
  geom_sf() +
  coord_sf(crs = sf::st_crs(4326)) +
  theme_bw(base_family = "Gmarket Sans TTF Bold") +
  # labs(title = "한글입니다.") +
  theme(legend.position = "none")

# extrafont::fonts()]

png(filename = "./2023/20231215/v1_20231215-01.png",
    width = 8, height = 8, res = 120, units = "in")
g1
dev.off()

ggsave(filename = "./2023/20231215/v1_20231215-03.png",
       width = 8, height = 8,units = "in", device = ragg::agg_png, dpi = 240, res = 240)

ggsave(filename = "./2023/20231215/v1_20231215-02.png", 
       device = grDevices::png,
       width = 8, height = 8, dpi = 120, units = "in")

packageVersion('ggplot2')
ggplot2