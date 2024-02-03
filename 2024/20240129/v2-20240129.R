source('./geo_core.R')

sido = sido_with_dokdo()

sido1 = sido |> 
  filter(CTPRVN_CD == '36' | CTPRVN_CD == '44')

sido1
get_center(sido1)

st_centroid(sido1)
ggplot() +
  geom_sf(data = sido1, aes(fill = CTPRVN_CD)) + 
  # geom_sf_text(data = st_centroid(sido1), aes(label = CTP_KOR_NM),
  #              family = v_font_base) +
  geom_text(data = get_center(sido1), aes(x,y), label = v_water_mark,
            colour = "gray0",size = 7, alpha = 0.1,
            family = v_font_heavy) +
  scale_fill_brewer(palette = "BuPu", direction = -1) +
  theme_bw(base_family = v_font_bold) +
  theme(legend.position = "none", 
        title = element_blank(),
        plot.margin = margin(0,0.1,0.1,0.1, "in"))

ggsave(filename = "./2024/20240129/v02-01.png", 
       device = grDevices::png,
       width = 6, height = 5, dpi = 180, units = "in")



## 44.충청남도
sido44 = sido |> filter(CTPRVN_CD == '44')
sido44
## 36.세종특별자치시.
sido36 = sido |> filter(CTPRVN_CD == '36')
sido36
st_geometry(sido44)
geom_sido44_2 = st_union(st_geometry(sido44), st_geometry(sido36))

geom_sido44_2
sido44_2 = st_set_geometry(sido44, geom_sido44_2)
sido44_2



ggplot() +
  geom_sf(data = sido44_2, aes(fill = CTPRVN_CD)) + 
  geom_text(data = get_center(sido1), aes(x,y), label = v_water_mark,
            colour = "gray0",size = 7, alpha = 0.1,
            family = v_font_heavy) +
  scale_fill_brewer(palette = "BuPu", direction = 1) +
  theme_bw(base_family = v_font_bold) +
  theme(legend.position = "none", 
        title = element_blank(),
        plot.margin = margin(0,0.1,0.1,0.1, "in"))

ggsave(filename = "./2024/20240129/v02-02.png", 
       device = grDevices::png,
       width = 6, height = 5, dpi = 180, units = "in")


sido = sido_with_dokdo()

sido3 = sido_with_dokdo() |> 
  filter(CTPRVN_CD == '36' | CTPRVN_CD == '44') |> 
  summarise(CTPRVN_CD = '44') |> 
  left_join(st_drop_geometry(sido), by = join_by(CTPRVN_CD)) |> 
  select(1, 3:4, 2)
  
sido3

ggplot() +
  geom_sf(data = sido3, aes(fill = CTPRVN_CD)) + 
  geom_text(data = get_center(sido3), aes(x,y), label = v_water_mark,
            colour = "gray0",size = 7, alpha = 0.1,
            family = v_font_heavy) +
  scale_fill_brewer(palette = "BuPu", direction = 1) +
  # coord_sf(datum = st_crs(5179)) +
  # coord_sf(datum = st_crs(5179), xlim = c(975720, 975740),
  #          ylim = c(1856740, 1856760)) + 
  theme_bw(base_family = v_font_bold) +
  theme(legend.position = "none", 
        title = element_blank(),
        plot.margin = margin(0,0.1,0.1,0.1, "in"))

