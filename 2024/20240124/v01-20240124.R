source('./geo_core.R')


sig = sig_with_dokdo()
sig
as.numeric()
ggplot(sig) +
  geom_sf(aes(fill = SIG_CD), colour = "gray100") +
  # scale_fill_gradient() +
  scale_fill_viridis_d(begin = 0.3) +
  theme_void() +
  theme(legend.position = "none")


sido = read_sf('~/Documents/map/sido_20230729/ctprvn.shp') |> 
  mutate(CTP_KOR_NM = iconv(CTP_KOR_NM, from = "EUC-KR", to = 'UTF-8')) |> 
  ms_simplify(keep = 0.001, keep_shapes = T)  |> 
  st_set_crs(5179)

sido  = sido_with_dokdo()



dokdo = get_dokdo() |> 
  map_scale(12)

sido = get_map_sido()

sido47 = sido |> 
  filter(str_detect(CTP_KOR_NM, '경상북도'))

geom_sido47 = sido47 |> 
  st_geometry()

geom_dokdo = dokdo |> 
  st_geometry()


geom_sido47_2 = st_union(geom_sido47, geom_dokdo)
sido47_2 = st_set_geometry(sido47, geom_sido47_2)

sido2 = sido |> 
  filter(!str_detect(CTP_KOR_NM, '경상북도')) |> 
  union_all(sido47_2)




ggplot(sido) +
  geom_sf()
