source('./geo_core.R')


map_dong1 = read_sf('~/data/map/census_2023/bnd_dong_00_2023_2023/bnd_dong_00_2023_2023_2Q.shp') |> 
  clean_names()

map_sido1 = read_sf('~/data/map/census_2023/bnd_sido_00_2023_2023/bnd_sido_00_2023_2023_2Q.shp') |> 
  clean_names() |> 
  st_drop_geometry()
map_sido1

map_sigungu1 = read_sf('~/data/map/census_2023/bnd_sigungu_00_2023_2023/bnd_sigungu_00_2023_2023_2Q.shp') |> 
  clean_names() |> 
  st_drop_geometry()
map_sigungu1

map_dong2 = map_dong1 |> mutate(sido_cd = str_sub(adm_cd, 1,2), .before = geometry) |> 
  mutate(sigungu_cd = str_sub(adm_cd,1,5), .before = geometry)

map_dong2_seoul = map_dong2 |> filter(str_detect(sido_cd, '11'))
map_dong2_seoul
######################################################################################

seongeogu_seoul = read_excel('~/data/map/vote/seongeogu_seoul.xlsx', skip = 3) |> 
  janitor::clean_names()

seongeogu_seoul1 = seongeogu_seoul |> 
  mutate(dong1 = map_vec(eubmyeondongmyeong, str_split, ',')) |> 
  unnest_longer(dong1) |> 
  select(-4) |> 
  mutate(dong1 = map_chr(dong1, trimws))


seongeogu_seoul2 = seongeogu_seoul1 |> 
  mutate(gusigunmyeong = ifelse(str_detect(dong1, '(성동구)'), '성동구', gusigunmyeong)) |> 
  mutate(dong1 = str_replace(dong1, '\\(성동구\\)', ''))

seongeogu_seoul3 = seongeogu_seoul2 |> 
  mutate(dong2 = str_extract(dong1, '제\\d+.*동$')) |> 
  mutate(dong3 = str_sub(dong2, 2)) |> 
  mutate(dong4 = str_replace(dong1, dong2, '')) |> 
  mutate(dong5 = str_c(dong4, dong3)) |> 
  mutate(dong = ifelse(is.na(dong5), dong1, dong5), .before = dong1)

seongeogu_seoul4 = seongeogu_seoul3 |> 
  left_join(map_sigungu1 |> 
              filter(str_detect(sigungu_cd, '^11')), 
            by = join_by(gusigunmyeong == sigungu_nm)) |> 
  select(1,3,4,11)

######################################################################################

map_dong2_seoul2 = map_dong2_seoul |> 
  left_join(seongeogu_seoul4, by = join_by(adm_nm == dong, sigungu_cd)) |> 
  relocate(geometry, .after = last_col())

map_dong2_seoul2

map_dong2_seoul3 = map_dong2_seoul2 |> 
  group_by(seongeogumyeong) |> 
  summarise() 

map_dong2_seoul4 = map_dong2_seoul3 |> 
  mutate(sig = str_replace(seongeogumyeong, '(갑|을|병|정|무)$', ''), .before = geometry)

map_dong2_seoul5 = map_dong2_seoul4 |> 
  ms_simplify(keep = 0.01)

map_seongeogu = function(vmap, vTitle = "종로구", voption = "D"){
  design <- "
    1222
    1222
  "
  
  vmap_2 = vmap |> filter(str_detect(sig, vTitle))
  
  g1_bg = "gray90"
  
  g1 = ggplot() +
    geom_sf(data = vmap, fill = 'gray70', colour = g1_bg) +
    geom_sf(data = vmap_2, fill = "gray40", colour = g1_bg) +
    labs(title = "서울특별시\n49개 선거구 지도") +
    # scale_fill_viridis_d(option = "D", begin = 0, end = 1) +
    # facet_wrap(vars(sig)) +
    theme_void(base_family = v_font_gmarket_bold) +
    theme(legend.position = "none", 
          plot.background = element_rect(fill = g1_bg, colour = g1_bg),
          plot.title = element_text(hjust = 0, size = 12, colour = "gray30"),
          plot.margin = margin(0.1,0.15,0.1,0.1,"in"))
  
  g2_bg = "gray97"
  
  g2 = ggplot() +
    # geom_sf(data = map_dong1_seoul4, colour = "gray100", fill = 'gray90', linewidth = 0.3) +
    geom_sf(data = vmap_2, aes(fill = seongeogumyeong), colour = g2_bg, linewidth = 0.4) +
    geom_text(data = get_center(vmap_2), aes(x,y), label = v_water_mark, na.rm = T,
              colour = "gray0",size = 7, alpha = 0.2,
              family = v_font_heavy) +
    scale_fill_viridis_d(option = voption, begin = 0.3, end = 0.8) +
    labs(title = str_c(vTitle, " 선거구")) +
    # facet_wrap(vars(sig)) +
    theme_void(base_family = v_font_gmarket_bold) +
    theme(legend.title = element_blank(),
          legend.text = element_text(family = v_font_gmarket_medium),
          legend.position = "bottom",
          plot.background = element_rect(fill = g2_bg, colour = g2_bg),
          plot.margin = margin(0.1,0.1,0.2,0.1,"in"),
          plot.title = element_text(hjust = 0.5, size = 15, colour = "gray30"))
  
  g1 + g2 + plot_layout(design = design)
}





map_seongeogu(map_dong2_seoul4, "강서구")
ggsave(filename = "./2024/20240306/v3-01.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 

map_seongeogu(map_dong2_seoul5, "강서구")

ggsave(filename = "./2024/20240306/v3-02.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 

map_seongeogu(map_dong2_seoul4, "송파구")
ggsave(filename = "./2024/20240306/v3-11.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 

map_seongeogu(map_dong2_seoul5, "송파구")

ggsave(filename = "./2024/20240306/v3-12.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 




map_seongeogu(map_dong2_seoul4, "노원구")
ggsave(filename = "./2024/20240306/v3-21.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 

map_seongeogu(map_dong2_seoul5, "노원구")

ggsave(filename = "./2024/20240306/v3-22.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 


map_seongeogu(map_dong2_seoul4, "강남구", "B")
ggsave(filename = "./2024/20240306/v3-31.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 

map_seongeogu(map_dong2_seoul5, "강남구", "B")

ggsave(filename = "./2024/20240306/v3-32.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 



map_seongeogu(map_dong2_seoul4, "은평구", "E")
ggsave(filename = "./2024/20240306/v3-41.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 

map_seongeogu(map_dong2_seoul5, "은평구", "E")

ggsave(filename = "./2024/20240306/v3-42.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 
########################################################################
# st_write(map_dong2_seoul5, './2024/20240306/sf/map_seongeogu_0_01x.shp', 
#          layer_options = "ENCODING=UTF-8")
# map_dong2_seoul5
# read_sf('./2024/20240306/sf/map_seongeogu_0_01x.shp')


# st_write(map_dong2_seoul4, './2024/20240306/sf/map_seongeogu_1x.shp', 
#          layer_options = "ENCODING=UTF-8")

read_sf('./2024/20240306/sf/map_seongeogu_0_01x.shp')

########################################################################