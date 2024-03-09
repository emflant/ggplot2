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


map_dong2

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

map_seongeogu = function(vmap, vTitle = "종로구"){
  design <- "
    1222
    1222
  "
  
  g1 = ggplot() +
    geom_sf(data = vmap, colour = "gray90", fill = 'gray70') +
    geom_sf(data = vmap |> filter(str_detect(sig, vTitle)),
            fill = "gray40", colour = "gray90") +
    labs(title = "서울특별시\n49개 선거구 지도") +
    # scale_fill_viridis_d(option = "D", begin = 0, end = 1) +
    # facet_wrap(vars(sig)) +
    theme_void(base_family = v_font_gmarket_bold) +
    theme(legend.position = "none", 
          plot.background = element_rect(fill = "gray90", colour = "gray90"),
          plot.title = element_text(hjust = 0, size = 12, colour = "gray30"),
          plot.margin = margin(0.1,0.15,0.1,0.1,"in"))
  
  
  g2 = ggplot() +
    # geom_sf(data = map_dong1_seoul4, colour = "gray100", fill = 'gray90', linewidth = 0.3) +
    geom_sf(data = vmap |> filter(str_detect(sig, vTitle)),
            aes(fill = seongeogumyeong), colour = "gray100", linewidth = 0.4) +
    scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) +
    labs(title = str_c(vTitle, " 선거구")) +
    # facet_wrap(vars(sig)) +
    theme_void(base_family = v_font_gmarket_bold) +
    theme(legend.title = element_blank(),
          legend.text = element_text(family = v_font_gmarket_medium),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 15, colour = "gray30"))
  
  g1 + g2 + plot_layout(design = design)
}

map_seongeogu(map_dong2_seoul4, "강서구")
ggsave(filename = "./2024/20240306/v2-01.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 

map_seongeogu(map_dong2_seoul5, "강서구")

ggsave(filename = "./2024/20240306/v2-02.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 



map_seongeogu(map_dong2_seoul4, "송파구")
ggsave(filename = "./2024/20240306/v2-11.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 

map_seongeogu(map_dong2_seoul5, "송파구")

ggsave(filename = "./2024/20240306/v2-12.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 




map_seongeogu(map_dong2_seoul4, "노원구")
ggsave(filename = "./2024/20240306/v2-21.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 

map_seongeogu(map_dong2_seoul5, "노원구")

ggsave(filename = "./2024/20240306/v2-22.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 


map_seongeogu(map_dong2_seoul4, "강남구")
ggsave(filename = "./2024/20240306/v2-31.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 

map_seongeogu(map_dong2_seoul5, "강남구")

ggsave(filename = "./2024/20240306/v2-32.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 

########################################################################
st_write(map_dong2_seoul5, './2024/20240306/sf/map_seongeogu_0_01x.shp', 
         layer_options = "ENCODING=UTF-8")
map_dong2_seoul5
read_sf('./2024/20240306/sf/map_seongeogu_0_01x.shp')


st_write(map_dong2_seoul4, './2024/20240306/sf/map_seongeogu_1x.shp', 
         layer_options = "ENCODING=UTF-8")

read_sf('./2024/20240306/sf/map_seongeogu_1x.shp')

########################################################################