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

seongeogu_seoul = read_excel('~/data/map/vote/seongeogu-seoul.xlsx', skip = 3) |> 
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
# seongeogu_seoul3
seongeogu_seoul4 = seongeogu_seoul3 |> 
  left_join(map_sigungu1 |> 
              filter(str_detect(sigungu_cd, '^11')), 
            by = join_by(gusigunmyeong == sigungu_nm)) |> 
  select(1,3,4,11)

######################################################################################

map_dong2_seoul2 = map_dong2_seoul |> 
  ms_simplify(keep = 0.01) |> 
  left_join(seongeogu_seoul4, by = join_by(adm_nm == dong, sigungu_cd)) |> 
  relocate(geometry, .after = last_col())

map_dong2_seoul2

map_dong2_seoul3 = map_dong2_seoul2 |> 
  group_by(seongeogumyeong) |> 
  summarise() 

map_dong2_seoul4 = map_dong2_seoul3 |> 
  mutate(sig = str_replace(seongeogumyeong, '(갑|을|병|정|무)$', ''), .before = geometry) |> 
  mutate(knd1 = str_extract(seongeogumyeong, '(갑|을|병|정|무)$'), .before = geometry) |> 
  mutate(knd2 = factor(knd1, levels = c("갑", "을", "병", "정", "무")), .before = geometry) |> 
  arrange(sig, knd2) |> 
  mutate(knd3 = row_number(), .before = geometry)
  
map_dong2_seoul4

map_dong2_seoul4
map_dong2_seoul4 |> print(n = Inf)
# a = c("을", "갑", "정", "무", "병")
# 
# factor(a)
# factor(a, levels = c("갑", "을", "병", "정", "무"))
# 
# factor(a, levels = c("갑", "을", "병", "정", "무")) |> order()

map_dong2_seoul4

map_dong2_seoul4 |>  print(n = Inf)
# map_dong2_seoul5 = map_dong2_seoul4 |> 
#   ms_simplify(keep = 0.01)
map_dong2_seoul3
map_dong2_seoul2

# map_dong2_seoul4 |> filter(str_detect(seongeogumyeong, "강서구")) |> 
ggplot() + 
  geom_sf(data = map_dong2_seoul4 |> filter(str_detect(seongeogumyeong, "강서구")), 
          aes(fill = reorder(seongeogumyeong, knd3) ) ) +
  scale_fill_viridis_d(option = "E", begin = 0.3, end = 0.8) +
  theme_bw(base_family = v_font_bold2) +
  theme(title = element_blank())

ggplot() + 
  geom_sf(data = map_dong2_seoul4 |> filter(str_detect(seongeogumyeong, "강서구")), 
          aes(fill = reorder(seongeogumyeong, knd3) ) ) +
  scale_fill_viridis_d(option = "E", begin = 0.3, end = 0.8) +
  theme_bw(base_family = v_font_bold2) +
  theme(title = element_blank())


map_seongeogu = function(vmap, vdongmap, vTitle = "종로구", voption = "D", vbegin = 0.3, vend = 0.8){
  design <- "
    112222
    112222
    112222
    112222
  "
  
  vmap_2 = vmap |> filter(str_detect(sig, vTitle))
  
  g1_bg = "gray90"
  
  g1 = ggplot() +
    geom_sf(data = vmap, fill = 'gray70', colour = g1_bg) +
    geom_sf(data = vmap_2, fill = "gray40", colour = g1_bg) +
    labs(title = "서울특별시\n48개 선거구 지도") +
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
    geom_sf(data = vmap_2, aes(fill = reorder(seongeogumyeong, knd3)), 
            colour = g2_bg, linewidth = 0.5) +
    geom_sf(data = vdongmap |> filter(str_detect(seongeogumyeong, vTitle)), fill = NA,
            colour = g2_bg, linewidth = 0.1) +
    geom_text(data = get_center(vmap_2), aes(x,y), label = v_water_mark, na.rm = T,
              colour = "gray0",size = 5, alpha = 0.5,
              family = v_font_heavy) +
    scale_fill_viridis_d(option = voption, begin = vbegin, end = vend) +
    labs(title = str_c(vTitle, " 선거구")) +
    theme_void(base_family = v_font_gmarket_bold) +
    theme(legend.title = element_blank(),
          legend.text = element_text(family = v_font_gmarket_medium),
          legend.position = "bottom",
          plot.background = element_rect(fill = g2_bg, colour = g2_bg),
          plot.margin = margin(0.1,0.1,0.2,0.1,"in"),
          plot.title = element_text(hjust = 0.5, size = 15, colour = "gray30"))
  
  g1 + g2 + plot_layout(design = design)
}



map_seongeogu(map_dong2_seoul4, map_dong2_seoul2, "강서구", voption = "G", vbegin = 0.25, vend = 0.75)

ggsave(filename = "./2024/20240306/v9-01.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 

map_seongeogu(map_dong2_seoul4, map_dong2_seoul2, "양천구", voption = "G", vbegin = 0.25, vend = 0.5)

ggsave(filename = "./2024/20240306/v9-02.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 









map_seongeogu2 = function(vmap, vdongmap, vTitle = "고양시", voption = "RdYlBu", vrev = F){
  design <- "
    1222
    1222
  "
  
  vmap_2 = vmap |> filter(str_detect(sig, vTitle))
  vn = nrow(vmap_2)
  vcolours4 = RColorBrewer::brewer.pal(n = 4, name = voption)  |> rev()
  vcolours = vcolours4
  if(vn > 4) {
    vcolours = RColorBrewer::brewer.pal(n = vn, name = voption) |> rev()
  } else if(vn == 3) {
    # vcolours = vcolours4[c(1,2,4)]
  } else if(vn == 2) {
    # vcolours = vcolours4[c(1,4)]
  }
  
  if(vrev){
    vcolours = rev(vcolours)
  }
  
  g1_bg = "gray90"
  
  # g1 = ggplot() +
  #   geom_sf(data = vmap, fill = 'gray70', colour = g1_bg) +
  #   geom_sf(data = vmap_2, fill = "gray40", colour = g1_bg) +
  #   labs(title = "서울특별시\n49개 선거구 지도") +
  #   # scale_fill_viridis_d(option = "D", begin = 0, end = 1) +
  #   # facet_wrap(vars(sig)) +
  #   theme_void(base_family = v_font_gmarket_bold) +
  #   theme(legend.position = "none", 
  #         plot.background = element_rect(fill = g1_bg, colour = g1_bg),
  #         plot.title = element_text(hjust = 0, size = 12, colour = "gray30"),
  #         plot.margin = margin(0.1,0.15,0.1,0.1,"in"))
  
  g2_bg = "gray20"
  
  g2 = ggplot() +
    # geom_sf(data = map_dong1_seoul4, colour = "gray100", fill = 'gray90', linewidth = 0.3) +
    geom_sf(data = vmap_2, aes(fill = reorder(seongeogumyeong, knd3)), 
            colour = g2_bg, linewidth = 0.5) + #
    geom_sf(data = vdongmap |> filter(str_detect(seongeogumyeong, vTitle)), fill = NA,
            colour = g2_bg, linewidth = 0.1) + #
    geom_text(data = get_center(vmap_2), aes(x,y), label = v_water_mark, na.rm = T,
              colour = "gray0",size = 5, alpha = 0.4,
              family = v_font_heavy) +
    # scale_fill_brewer(palette = voption, direction = vdirection, type = "div" ) +
    scale_fill_manual(values = vcolours) +
    labs(title = str_c(vTitle, " 선거구")) +
    theme_void(base_family = v_font_gmarket_bold) +
    theme(legend.title = element_blank(),
          legend.text = element_text(family = v_font_gmarket_medium, colour = "gray80", size = 12),
          legend.margin = margin(0.3,0,0.3,0,"in"),
          legend.position = "bottom",
          plot.background = element_rect(fill = g2_bg, colour = g2_bg),
          plot.margin = margin(0.3,0.5,0.3,0.5,"in"),
          # plot.margin = margin(0,0,0,0,"in"),
          plot.title = element_text(hjust = 0.5, size = 20, colour = "gray80",
                                    margin = margin(0.3,0,0.3,0,"in")))
  
  # g1 + g2 + plot_layout(design = design)
  g2
}

# Diverging
# BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
# 
# Qualitative
# Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
# 
# Sequential
# Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, 
# PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd
map_seongeogu2(map_dong2_seoul4, map_dong2_seoul2, "송파구", voption = "Blues")

ggsave(filename = "./2024/20240306/v9-11.png", 
       device = grDevices::png,
       width = 6, height = 6, dpi = 180, units = "in") 

map_seongeogu2(map_dong2_seoul4, map_dong2_seoul2, "강남구", voption = "YlOrBr")

ggsave(filename = "./2024/20240306/v9-12.png", 
       device = grDevices::png,
       width = 6, height = 6, dpi = 180, units = "in") 


map_seongeogu2(map_dong2_seoul4, map_dong2_seoul2, "강서구", voption = "Blues")

ggsave(filename = "./2024/20240306/v9-13.png", 
       device = grDevices::png,
       width = 6, height = 6, dpi = 180, units = "in") 

map_seongeogu2(map_dong2_seoul4, map_dong2_seoul2, "마포구", voption = "YlOrBr")

ggsave(filename = "./2024/20240306/v9-14.png", 
       device = grDevices::png,
       width = 6, height = 6, dpi = 180, units = "in") 

map_seongeogu2(map_dong2_seoul4, map_dong2_seoul2, "서대문구", voption = "Blues")

ggsave(filename = "./2024/20240306/v9-15.png", 
       device = grDevices::png,
       width = 6, height = 6, dpi = 180, units = "in") 

map_dong2_seoul4 |> filter(knd1 == '')

# |> filter(str_detect(sig, vTitle))
g2_bg = "gray20"
# vmap_2 = map_dong2_seoul4
# vdongmap = map_dong2_seoul2
map_dong2_seoul4
map_dong2_seoul2

