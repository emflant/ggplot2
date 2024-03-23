
source('./geo_core.R')

# fs = list.files('~/Downloads', full.names = T) |> enframe(name = NULL) |> 
#   mutate(value2 = map_chr(value, URLdecode), .before = value) |> 
#   mutate(value2 = map_chr(value2, stringi::stri_trans_general, "Hangul-Latin"), .before = value)
# 
# fs = list.files('~/Downloads', full.names = T) |> enframe(name = NULL) |> 
#   mutate(value2 = map_chr(value, URLdecode), .before = value)
# 
# fs
# purrr::map2_lgl(fs$value, fs$value2, file.copy, copy.date = T)

# file.copy(from = '/Users/jmlee/Downloads/%EA%B0%95%EC%9B%90%ED%8A%B9%EB%B3%84%EC%9E%90%EC%B9%98%EB%8F%84.zip',
#           to = '/Users/jmlee/Downloads/gang-wonteugbyeoljachido.zip', copy.date = T)

# URLdecode('%EB%B6%80%EC%82%B0%EA%B4%91%EC%97%AD%EC%8B%9C')


# 법정구역시도 TL_SCCO_CTPRVN
scco_ctprvn = read_sf("~/data/map/map_202402/서울특별시/11000/", "TL_SCCO_CTPRVN") |> 
  mutate(CTP_KOR_NM = iconv(CTP_KOR_NM, from = "EUC-KR", to = 'UTF-8')) |> 
  clean_names()

ggplot(scco_ctprvn) +
  geom_sf() +
  geom_text(data = get_center(scco_sig), aes(x,y - 15000), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  theme_bw(base_family = v_font_bold) +
  theme(title = element_blank())

ggsave(filename = "./2024/20240321/v1-TL_SCCO_CTPRVN.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 180, units = "in") 

# 법정구역시군구(TL_SCCO_SIG)

scco_sig = read_sf("~/data/map/map_202402/서울특별시/11000/", "TL_SCCO_SIG") |> 
  mutate(SIG_KOR_NM = iconv(SIG_KOR_NM, from = "EUC-KR", to = 'UTF-8')) |> 
  clean_names()

scco_sig |> filter(sig_cd == '11650')

ggplot(scco_sig) +
  geom_sf() +
  geom_text(data = get_center(scco_sig), aes(x,y - 15000), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  theme_bw(base_family = v_font_bold) +
  theme(title = element_blank())

ggsave(filename = "./2024/20240321/v1-TL_SCCO_SIG.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 180, units = "in") 


# 법정구역읍면동(TL_SCCO_EMD)
scco_emd = read_sf("~/data/map/map_202402/서울특별시/11000/", "TL_SCCO_EMD") |> 
  mutate(EMD_KOR_NM = iconv(EMD_KOR_NM, from = "EUC-KR", to = 'UTF-8')) |> 
  clean_names()

scco_emd
ggplot(scco_emd) +
  geom_sf() +
  geom_text(data = get_center(scco_sig), aes(x,y - 15000), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  theme_bw(base_family = v_font_bold) +
  theme(title = element_blank())

ggsave(filename = "./2024/20240321/v1-TL_SCCO_EMD.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 180, units = "in") 


# 기초구간(TL_SPRD_INTRVL)
# sprd_intrvl = read_sf("~/data/map/map_202402/서울특별시/11000/", "TL_SPRD_INTRVL") |> 
#   clean_names()
# 
# ggplot(sprd_intrvl) +
#   geom_sf() +
#   theme_bw()
# 
# ggsave(filename = "./2024/20240321/v1-TL_SPRD_INTRVL.png", 
#        device = grDevices::png,
#        width = 8, height = 6, dpi = 180, units = "in") 
# 
# ggplot(sprd_intrvl |> filter(sig_cd =='11650')) +
#   geom_sf() +
#   theme_bw()

# ggsave(filename = "./2024/20240321/v1-TL_SPRD_INTRVL-2.png", 
#        device = grDevices::png,
#        width = 8, height = 6, dpi = 180, units = "in") 


# 도로구간(TL_SPRD_MANAGE)
sprd_manage = read_sf("~/data/map/map_202402/서울특별시/11000/", "TL_SPRD_MANAGE") |> 
  mutate(ALWNC_RESN = iconv(ALWNC_RESN, from = "EUC-KR", to = 'UTF-8')) |> 
  mutate(MVMN_RESN = iconv(MVMN_RESN, from = "EUC-KR", to = 'UTF-8')) |> 
  mutate(RBP_CN = iconv(RBP_CN, from = "EUC-KR", to = 'UTF-8')) |> 
  mutate(REP_CN = iconv(REP_CN, from = "EUC-KR", to = 'UTF-8')) |> 
  mutate(RN = iconv(RN, from = "EUC-KR", to = 'UTF-8')) |> 
  clean_names()
sprd_manage

ggplot(sprd_manage) +
  geom_sf() +
  geom_text(data = get_center(sprd_manage), aes(x,y - 15000), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  theme_bw() +
  theme(title = element_blank())

ggsave(filename = "./2024/20240321/v1-TL_SPRD_MANAGE-1.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 180, units = "in") 
ggsave(filename = "./2024/20240321/v1-TL_SPRD_MANAGE-1-120.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 120, units = "in") 

sprd_manage2 = sprd_manage |> filter(sig_cd =='11650')
ggplot(sprd_manage2) +
  geom_sf() +
  geom_text(data = get_center(sprd_manage2), aes(x,y - 5000), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  theme_bw()+
  theme(title = element_blank())
ggsave(filename = "./2024/20240321/v1-TL_SPRD_MANAGE-2.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 180, units = "in") 

ggsave(filename = "./2024/20240321/v1-TL_SPRD_MANAGE-2-120.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 120, units = "in") 

# sprd |> filter(rn_cd == '4118463')
# ggplot(sprd |> filter(rn_cd == '4118463')) +
#   geom_sf() +
#   coord_sf(xlim = c(964720, 964720 + 40), ylim = c(1954950,1954950 +50))



# 기초구역(TL_KODIS_BAS)
# kodis_bas = read_sf("~/data/map/map_202402/서울특별시/11000/", "TL_KODIS_BAS") |> 
#   mutate(CTP_KOR_NM = iconv(CTP_KOR_NM, from = "EUC-KR", to = 'UTF-8')) |> 
#   mutate(SIG_KOR_NM = iconv(SIG_KOR_NM, from = "EUC-KR", to = 'UTF-8')) |> 
#   mutate(MVMN_RESN = iconv(MVMN_RESN, from = "EUC-KR", to = 'UTF-8')) |> 
#   clean_names()
# 
# kodis_bas

# 기초구역 TL_KODIS_BAS
# ggplot(kodis_bas) +
#   geom_sf() +
#   geom_text(data = get_center(kodis_bas), aes(x,y - 15000), label = v_water_mark, na.rm = T,
#             colour = "gray0",size = 5, alpha = 0.3,
#             family = v_font_heavy) +
#   theme_bw() +
#   theme(title = element_blank())
# 
# ggsave(filename = "./2024/20240321/v1-TL_KODIS_BAS.png", 
#        device = grDevices::png,
#        width = 8, height = 6, dpi = 180, units = "in") 



# 실폭도로 TL_SPRD_RW
sprd_rw = read_sf("~/data/map/map_202402/서울특별시/11000/", "TL_SPRD_RW")  |> 
  clean_names()
  # ms_simplify(keep = 0.01, keep_shapes = T)
sprd_rw
ggplot(sprd_rw) +
  geom_sf() +
  geom_text(data = get_center(sprd_rw), aes(x,y - 14000), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  theme_bw(base_family = v_font_bold) +
  theme(title = element_blank())

ggsave(filename = "./2024/20240321/v1-TL_SPRD_RW-1.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 180, units = "in") 

ggsave(filename = "./2024/20240321/v1-TL_SPRD_RW-1-120.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 120, units = "in") 


sprd_rw2 = sprd_rw |> filter(sig_cd =='11650')
ggplot(sprd_rw2) +
  geom_sf() +
  geom_text(data = get_center(sprd_rw2), aes(x,y - 5000), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  theme_bw(base_family = v_font_bold) +
  theme(title = element_blank())

ggsave(filename = "./2024/20240321/v1-TL_SPRD_RW-2.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 180, units = "in") 

ggsave(filename = "./2024/20240321/v1-TL_SPRD_RW-2-120.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 120, units = "in") 

tb1 = tibble(x = 957000, y = 1942100)

ggplot() +
  geom_sf(data = sprd_rw2) +
  geom_text(data = tb1, aes(x,y), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  coord_sf(xlim = c(956000, 956000 + 2000),
           ylim = c(1942000, 1942000 + 1600)) +
  theme_bw(base_family = v_font_bold) +
  theme(title = element_blank())

ggsave(filename = "./2024/20240321/v1-TL_SPRD_RW-3.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 180, units = "in") 
ggsave(filename = "./2024/20240321/v1-TL_SPRD_RW-3-120.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 120, units = "in") 


# get_center(sprd_rw2)
ggplot(sprd_rw |> filter(sig_cd =='11650')) +
  geom_sf(fill = "gray80", colour = "gray80") +
  geom_text(data = get_center(sprd_rw2), aes(x,y - 5000), label = v_water_mark, na.rm = T,
            colour = "gray100",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  geom_text(data = get_center(sprd_rw2), aes(x + 2500, y + 4000), label = "서초구 도로지도", na.rm = T,
            colour = "gray80",size = 7, 
            family = v_font_gmarket_bold) +
  # labs(title = "서초구 실폭도로") +
  theme_void(base_family = v_font_gmarket_bold) +
  theme(plot.background = element_rect(fill = "gray20", colour = "gray20"),
        title = element_text(colour = "gray80"))
  
ggsave(filename = "./2024/20240321/v1-TL_SPRD_RW-4.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 180, units = "in") 

ggsave(filename = "./2024/20240321/v1-TL_SPRD_RW-4-120.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 120, units = "in") 


# get_center(sprd_rw2)
ggplot(sprd_rw |> filter(sig_cd =='11650')) +
  geom_sf(fill = "gray90", colour = "gray90") +
  geom_text(data = get_center(sprd_rw2), aes(x,y - 6000), label = v_water_mark, na.rm = T,
            colour = "gray100",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  geom_text(data = get_center(sprd_rw2), aes(x + 2300, y + 4000), label = "서초구 도로지도", na.rm = T,
            colour = "gray90",size = 7, 
            family = v_font_gmarket_bold) +
  # labs(title = "서초구 실폭도로") +
  theme_void(base_family = v_font_gmarket_bold) +
  theme(plot.background = element_rect(fill = "gray20", colour = "gray20"),
        title = element_text(colour = "gray90"))

ggsave(filename = "./2024/20240321/v1-TL_SPRD_RW-4-240.png", 
       device = grDevices::png,
       width = 6, height = 8, dpi = 240, units = "in") 



# get_center(sprd_rw2)
ggplot(sprd_rw |> filter(sig_cd =='11650')) +
  geom_sf(fill = "gray90", colour = "gray90") +
  geom_text(data = get_center(sprd_rw2), aes(x,y - 5000), label = v_water_mark, na.rm = T,
            colour = "gray100",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  geom_text(data = get_center(sprd_rw2), aes(x + 2300, y + 4000), label = "서초구 도로지도", na.rm = T,
            colour = "gray90",size = 8, 
            family = v_font_gmarket_bold) +
  # labs(title = "서초구 실폭도로") +
  theme_void(base_family = v_font_gmarket_bold) +
  theme(plot.background = element_rect(fill = "gray20", colour = "gray20"),
        title = element_text(colour = "gray90"))

ggsave(filename = "./2024/20240321/v1-TL_SPRD_RW-4-240-2.png", 
       device = grDevices::png,
       width = 8, height = 8, dpi = 240, units = "in") 



ggplot(sprd_rw) +
  geom_sf(fill = "gray80", colour = "gray80") +
  geom_text(data = get_center(sprd_rw), aes(x,y - 14000), label = v_water_mark, na.rm = T,
            colour = "gray100",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  theme_void() +
  theme(plot.background = element_rect(fill = "gray20", colour = "gray20"))

ggsave(filename = "./2024/20240321/v1-TL_SPRD_RW-5.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 180, units = "in") 

ggsave(filename = "./2024/20240321/v1-TL_SPRD_RW-5-120.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 120, units = "in") 







# 건물(TL_SPBD_BULD)
spbd_buld = read_sf("~/data/map/map_202402/서울특별시/11000/", "TL_SPBD_BULD")  |> 
  clean_names() |> 
  mutate(buld_nm = iconv(buld_nm, from = "EUC-KR", to = 'UTF-8')) |> 
  mutate(buld_nm_dc = iconv(buld_nm_dc, from = "EUC-KR", to = 'UTF-8'))

spbd_buld
spbd_buld |> filter(sig_cd =='11650')
spbd_buld |> filter(emd_cd == '11110104')
spbd_buld
spbd_buld2 = spbd_buld |> 
  filter(sig_cd == '11650')

ggplot(spbd_buld) +
  geom_sf() +
  geom_text(data = get_center(spbd_buld), aes(x,y - 15000), label = v_water_mark, na.rm = T,
                       colour = "gray0",size = 7, alpha = 0.3,
                       family = v_font_heavy) +
  theme_bw() +
  theme(title = element_blank())



ggsave(filename = "./2024/20240321/v1-TL_SPBD_BULD-1.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 180, units = "in")

ggsave(filename = "./2024/20240321/v1-TL_SPBD_BULD-1-120.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 120, units = "in")

ggplot(spbd_buld2) +
  geom_sf() +
  geom_text(data = get_center(spbd_buld2), aes(x,y - 000), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  theme_bw() +
  theme(title = element_blank())



ggsave(filename = "./2024/20240321/v1-TL_SPBD_BULD-2.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 180, units = "in") 

ggsave(filename = "./2024/20240321/v1-TL_SPBD_BULD-2-120.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 120, units = "in") 

ggplot(spbd_buld2) +
  geom_sf() +
  annotate("text", x = 957000, y = 1942100, label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  coord_sf(xlim = c(956000, 956000 + 2000),
           ylim = c(1942000, 1942000 + 1600)) +
  theme_bw() +
  theme(title = element_blank())

ggsave(filename = "./2024/20240321/v1-TL_SPBD_BULD-3.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 180, units = "in") 
ggsave(filename = "./2024/20240321/v1-TL_SPBD_BULD-3-120.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 120, units = "in") 



ggplot() +
  # geom_sf(data = sprd_rw |> filter(sig_cd =='11650')) +
  geom_sf(data = sprd_manage |> filter(sig_cd =='11650')) +
  geom_sf(data = spbd_buld |> filter(sig_cd == '11650')) +
  coord_sf(xlim = c(956000, 956000 + 2000),
           ylim = c(1942000, 1942000 + 2000)) +
  theme_bw()




ggplot() +
  geom_sf(data = sprd_rw |> filter(sig_cd =='11650')) +
  # geom_sf(data = spbd_buld |> filter(sig_cd == '11650')) +
  annotate("text", x = 956000 + 1000, y = (1942000 + 100), label = v_water_mark, na.rm = T,
           colour = "gray0",size = 7, alpha = 0.3,
           family = v_font_heavy) +
  coord_sf(xlim = c(956000, 956000 + 2000),
           ylim = c(1942000, 1942000 + 1600)) +
  theme_bw(base_family = v_font_bold) +
  theme(title = element_blank())

ggsave(filename = "./2024/20240321/v1-11-120.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 120, units = "in") 


ggplot() +
  geom_sf(data = sprd_rw |> filter(sig_cd =='11650')) +
  geom_sf(data = spbd_buld |> filter(sig_cd == '11650')) +
  annotate("text", x = 956000 + 1000, y = (1942000 + 100), label = v_water_mark, na.rm = T,
           colour = "gray0",size = 7, alpha = 0.3,
           family = v_font_heavy) +
  coord_sf(xlim = c(956000, 956000 + 2000),
           ylim = c(1942000, 1942000 + 1600)) +
  theme_bw(base_family = v_font_bold) +
  theme(title = element_blank())

ggsave(filename = "./2024/20240321/v1-12-120.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 120, units = "in") 


ggplot() +
  geom_sf(data = sprd_rw |> filter(sig_cd =='11650'),
          fill = NA, colour = "gray80") +
  geom_sf(data = spbd_buld |> filter(sig_cd == '11650'),
          fill = NA, colour = "gray80") +
  annotate("text", x = 956000 + 1100, y = (1942000 + 100), label = v_water_mark, na.rm = T,
           colour = "gray80",size = 7, alpha = 0.7,
           family = v_font_heavy) +
  coord_sf(xlim = c(956000, 956000 + 2100),
           ylim = c(1942000, 1942000 + 1600)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "gray20", colour = "gray20"),
        plot.margin = margin(0,0,0,0,"in"))

ggsave(filename = "./2024/20240321/v1-21.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 180, units = "in") 
ggsave(filename = "./2024/20240321/v1-21-120.png", 
       device = grDevices::png,
       width = 8, height = 6, dpi = 120, units = "in") 


R프로그래밍으로 만든 서초구 도로지도. 


#rstats #ggplot2 #dataviz #datascience #map #지도 #데이터시각화 #데이터분석