source('./geo_core.R')

sido_info = read_sf('~/Documents/map/sido_20230729/ctprvn.dbf') |> 
  st_drop_geometry() |> 
  mutate(CTP_KOR_NM = iconv(CTP_KOR_NM, from = "EUC-KR", to = 'UTF-8'))

sido_info
# aaaa = korea2_sim |> left_join(sido_info) |>
#   select(CTPRVN_CD, CTP_ENG_NM, CTP_KOR_NM, SIG_CD, SIG_ENG_NM, SIG_KOR_NM, geometry)

map1 = read_sf('~/Documents/map/sig_20230729/sig.shp')

map1

map2 = map1 |> 
  mutate(SIG_KOR_NM = iconv(SIG_KOR_NM, from = "EUC-KR", to = 'UTF-8')) |> 
  st_set_crs(5179) |> 
  ms_simplify(keep = 0.001, keep_shapes = T) |> 
  mutate(CTPRVN_CD = str_sub(SIG_CD, 1,2), .before = SIG_CD) |> 
  left_join(sido_info) |>
  select(CTPRVN_CD, CTP_ENG_NM, CTP_KOR_NM, SIG_CD, SIG_ENG_NM, SIG_KOR_NM, geometry)


map3 = map2 |> 
  mutate(CTPRVN_NUM = as.numeric(CTPRVN_CD))

map3 |> 
  colnames()
# [1] "CTPRVN_CD"  "CTP_ENG_NM" "CTP_KOR_NM" "SIG_CD"     
# "SIG_ENG_NM" "SIG_KOR_NM" "geometry"   "CTPRVN_NUM"

ggplot(map3) +
  geom_sf() +
  coord_sf(crs = sf::st_crs(4326))+
  annotate("text", x = 128, y = 36.1, alpha = 0.5,
           label = v_water_mark, colour = "gray0",size = 7,
           family = "AppleSDGothicNeo-Heavy") +
  theme(axis.title = element_blank())
  # labs(title = "(지도 3-2) crs = 4326") +
  # theme_bw(base_family = "AppleSDGothicNeo-Bold") +
  
  
# EPSG:5179
  
ggsave(filename = "./2024/20240111/v01-01.png", 
       device = grDevices::png,
       width = 6, height = 5, dpi = 180, units = "in")

map3

# ggplot(map3) +
#   geom_sf(aes(fill = SIG_CD)) +
#   theme(legend.position = "none")
  

ggplot(map3) +
  geom_sf(aes(fill = SIG_CD)) +
  theme_bw(base_family = "AppleSDGothicNeo-Bold") +
  theme(legend.position = "none") +
  annotate("text", x = 1050000, y = 1800000, alpha = 0.5,
           label = v_water_mark, colour = "gray0", size = 7,
           family = "AppleSDGothicNeo-Heavy") +
  theme(axis.title = element_blank()) 

ggsave(filename = "./2024/20240111/v01-02.png", 
       device = grDevices::png,
       width = 6, height = 5, dpi = 180, units = "in")


ggplot(map3) +
  geom_sf(aes(fill = SIG_CD)) +
  scale_fill_viridis_d() +
  theme_bw(base_family = "AppleSDGothicNeo-Bold") +
  theme(legend.position = "none")

ggsave(filename = "./2024/20240111/v01-03.png", 
       device = grDevices::png,
       width = 6, height = 5, dpi = 180, units = "in")

ggplot(map3) +
  geom_sf(aes(fill = SIG_CD)) +
  scale_fill_viridis_d(begin = 0.5, end = 1) +
  theme_bw(base_family = "AppleSDGothicNeo-Bold") +
  theme(legend.position = "none")

ggsave(filename = "./2024/20240111/v01-04.png", 
       device = grDevices::png,
       width = 6, height = 5, dpi = 180, units = "in")

 ############################################################
# "magma" (or "A")
# "inferno" (or "B")
# "plasma" (or "C")
# "viridis" (or "D")
# "cividis" (or "E")
# "rocket" (or "F")
# "mako" (or "G")
# "turbo" (or "H")

layout = "
AAABBB
AAABBB
"

g1 = 
  ggplot(map3) +
  geom_sf(aes(fill = SIG_CD)) +
  scale_fill_viridis_d(option = "A") +
  labs(title = "magma (or A)") +
  theme_bw(base_family = "AppleSDGothicNeo-Bold") +
  theme(legend.position = "none", axis.title = element_blank()) +
  annotate("text", x = 1050000, y = 1830000, alpha = 0.7,
           label = v_water_mark, colour = "gray0", size = 6,
           family = "AppleSDGothicNeo-Heavy")


g2 = ggplot(map3) +
  geom_sf(aes(fill = SIG_CD)) +
  scale_fill_viridis_d(option = "B") +
  labs(title = "inferno (or B)") +
  theme_bw(base_family = "AppleSDGothicNeo-Bold") +
  theme(legend.position = "none", axis.title = element_blank()) +
  annotate("text", x = 1050000, y = 1830000, alpha = 0.7,
           label = v_water_mark, colour = "gray0", size = 6,
           family = "AppleSDGothicNeo-Heavy")



g1 + g2 + plot_layout(design =  layout)

ggsave(filename = "./2024/20240111/v01-11.png", 
       device = grDevices::png,
       width = 8, height = 4, dpi = 180, units = "in")

    # plot_spacer() +
#   theme(plot.background = element_rect(fill = NA, color = NA)) +
#   inset_element(plot_donut2(), left = 0, right = 0.5, bottom = 0.47, top = 1.02) +


################################################



my_plot = function(type, vtitle){
  ggplot(map3) +
    geom_sf(aes(fill = SIG_CD)) +
    scale_fill_viridis_d(option = type) +
    labs(title = vtitle) +
    theme_bw(base_family = "AppleSDGothicNeo-Bold") +
    theme(legend.position = "none", axis.title = element_blank()) +
    annotate("text", x = 1050000, y = 1830000, alpha = 0.7,
             label = v_water_mark, colour = "gray0", size = 6,
             family = "AppleSDGothicNeo-Heavy")
}

# magma (or A)
# inferno (or B)
my_plot("A", "magma (or A)") + 
  my_plot("B", "inferno (or B)") + 
  plot_layout(design =  layout)

ggsave(filename = "./2024/20240111/v01-11.png", 
       device = grDevices::png,
       width = 8, height = 4, dpi = 180, units = "in")

# plasma (or C)
# viridis (or D)
my_plot("C", "plasma (or C)") + 
  my_plot("D", "viridis (or D)") + 
  plot_layout(design =  layout)

ggsave(filename = "./2024/20240111/v01-12.png", 
       device = grDevices::png,
       width = 8, height = 4, dpi = 180, units = "in")

# cividis (or E)
# rocket (or F)
my_plot("E", "cividis (or E)") + 
  my_plot("F", "rocket (or F)") + 
  plot_layout(design =  layout)

ggsave(filename = "./2024/20240111/v01-13.png", 
       device = grDevices::png,
       width = 8, height = 4, dpi = 180, units = "in")

# mako (or G)
# turbo (or H)
my_plot("G", "mako (or G)") + 
  my_plot("H", "turbo (or H)") + 
  plot_layout(design =  layout)

ggsave(filename = "./2024/20240111/v01-14.png", 
       device = grDevices::png,
       width = 8, height = 4, dpi = 180, units = "in")

# g1 + g2 + plot_layout(design =  layout)

#########################################


g1 = ggplot(map3) +
  geom_sf(aes(fill = SIG_CD)) +
  scale_fill_viridis_d() +
  labs(title = "begin = 0, end = 1") +
  theme_bw(base_family = v_font_bold) +
  theme(legend.position = "none", axis.title = element_blank()) +
  annotate("text", x = 1050000, y = 1830000, alpha = 0.7,
           label = v_water_mark, colour = "gray0", size = 6,
           family = v_font_heavy)

g2 = ggplot(map3) +
  geom_sf(aes(fill = SIG_CD)) +
  labs(title = "begin = 0.5, end = 1") +
  scale_fill_viridis_d(begin = 0.5, end = 1) +
  theme_bw(base_family = "AppleSDGothicNeo-Bold") +
  theme(legend.position = "none", axis.title = element_blank()) +
  annotate("text", x = 1050000, y = 1830000, alpha = 0.7,
           label = v_water_mark, colour = "gray0", size = 6,
           family = "AppleSDGothicNeo-Heavy")

g1 + g2 + plot_layout(design =  layout)

ggsave(filename = "./2024/20240111/v01-21.png", 
       device = grDevices::png,
       width = 8, height = 4, dpi = 180, units = "in")
 