source('./geo_core.R')


dokdo_scale = map_scale(get_dokdo(), scale = 12)

sido1 = read_sf('~/Documents/map/sido_20230729/ctprvn.shp')
sido1 |> head(3)
sido1 |> str
str(sido1)
##################################################################

sido2 = sido1 |> 
  mutate(CTP_KOR_NM = iconv(CTP_KOR_NM, from = "EUC-KR", to = 'UTF-8'))
sido1
sido2 |> head(3)

##################################################################
sido3 = sido2 |> 
  st_set_crs(5179)
sido3
sido3 |> head(3)

tic()
ggplot(sido3) +
  geom_sf()
toc() # 12.677 sec elapsed

##################################################################

# 독도 
ggplot(sido3 |> filter(CTP_KOR_NM == '경상북도') |> 
         ms_simplify(keep = 1, keep_shapes = T)) +
  geom_sf() +
  coord_sf(crs = st_crs(4326),
           xlim = c(131.855, 131.878),
           ylim = c(37.237, 37.248)) +
  theme_bw()


##################################################################

sido4 = sido3 |> 
  ms_simplify(keep = 0.01, keep_shapes = T)


ggplot() +
  geom_sf(data = sido4) +
  geom_sf(data = dokdo_scale) +
  geom_text(data = get_center(sido4), aes(x,y), label = v_water_mark,
            colour = "gray0",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  theme_bw(base_family = v_font_bold) +
  theme(axis.title = element_blank())

ggsave(filename = "./2023/20231215/v06_20231215-01.png", 
       device = grDevices::png,
       width = 6, height = 5, dpi = 180, units = "in")
##################################################################

# ggplot(sido4) +
#   geom_sf() +
#   coord_sf(crs = st_crs(4326))
# 
# ggsave(filename = "./2023/20231215/v06_20231215-02.png", 
#        device = grDevices::png,
#        width = 5, height = 5, dpi = 120, units = "in")

##################################################################

ggplot(sido4) +
  geom_sf() +
  coord_sf(crs = sf::st_crs(4326),
           xlim = c(126, 128), 
           ylim = c(37, 38))

ggsave(filename = "./2023/20231215/v06_20231215-02.png", 
       device = grDevices::png,
       width = 8, height = 4, dpi = 120, units = "in")

##################################################################

ggplot(sido4 |> filter(CTPRVN_CD %in% c('11', '28', '41', '43', '51'))) +
  geom_sf(aes(fill = CTP_KOR_NM)) +
  scale_fill_brewer(palette = "YlGnBu") +
  coord_sf(crs = sf::st_crs(4326),
           xlim = c(126, 128), ylim = c(37, 38)) +
  theme_bw(base_family = 'BMJUAOTF') +
  theme(legend.title = element_blank())

ggsave(filename = "./2023/20231215/v06_20231215-03.png", 
       device = grDevices::png,
       width = 8, height = 4, dpi = 120, units = "in")


##################################################################

ggplot(sido4) +
  geom_sf() +
  coord_sf(crs = st_crs(4326),
           xlim = c(131.855, 131.875),
           ylim = c(37.237, 37.248)) 
