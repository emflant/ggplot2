source('./core.R')

# install.packages("sf")
# install.packages("terra")
# install.packages("spData")
# install.packages("spDataLarge", repos = "https://nowosad.r-universe.dev")
# install.packages("tmap", repos = c("https://r-tmap.r-universe.dev",
#                                    "https://cloud.r-project.org"))
# install.packages("rmapshaper")

library(sf)
# library(terra)
# library(spData)
# library(spDataLarge)
# library(tmap)
library(rmapshaper)

get_map_sido = function(vkeep = 0.001){
  read_sf('~/Documents/map/sido_20230729/ctprvn.shp') |> 
    mutate(CTP_KOR_NM = iconv(CTP_KOR_NM, from = "EUC-KR", to = 'UTF-8')) |> 
    ms_simplify(keep = vkeep, keep_shapes = T)  |> 
    st_set_crs(5179)
}

get_map_sig = function(vkeep = 0.001){
  read_sf('~/Documents/map/sig_20230729/sig.shp') |> 
    mutate(SIG_KOR_NM = iconv(SIG_KOR_NM, from = "EUC-KR", to = 'UTF-8')) |> 
    ms_simplify(keep = vkeep, keep_shapes = T)  |> 
    st_set_crs(5179)
}

get_dokdo = function(vkeep = 0.001){
  read_sf('~/Documents/map/li_20230729/li.shp') |> 
    mutate(LI_KOR_NM = iconv(LI_KOR_NM, from = "EUC-KR", to = 'UTF-8')) |> 
    st_set_crs(5179) |> 
    filter(str_detect(LI_KOR_NM, '독도리')) |> 
    ms_simplify(keep = vkeep, keep_shapes = T)
}

map_scale = function(vmap, scale = 1){
  geom_map = st_geometry(vmap)
  geom_centroid = st_centroid(geom_map)
  geom_scale = (geom_map - geom_centroid) * scale + geom_centroid
  st_set_geometry(vmap, geom_scale) |> 
    st_set_crs(5179)
}

get_center = function(vmap){
  bb = st_bbox(vmap)
  tibble(x = mean(c(bb[1], bb[3])), y = mean(c(bb[2], bb[4])))
}

sido_with_dokdo = function(vkeep = 0.001){
  
  dokdo = get_dokdo(vkeep) |> 
    map_scale(12)
  
  sido = get_map_sido(vkeep)
  
  sido47 = sido |> 
    filter(str_detect(CTP_KOR_NM, '경상북도'))
  
  geom_sido47 = sido47 |> 
    st_geometry()
  
  geom_dokdo = dokdo |> 
    st_geometry()
  
  
  geom_sido47_2 = st_union(geom_sido47, geom_dokdo)
  sido47_2 = st_set_geometry(sido47, geom_sido47_2)
  
  sido |> 
    filter(!str_detect(CTP_KOR_NM, '경상북도')) |> 
    union_all(sido47_2) |> 
    arrange(CTPRVN_CD)
}


sig_with_dokdo = function(vkeep = 0.001){
  dokdo = get_dokdo(vkeep) |> 
    map_scale(12)
  
  sig = get_map_sig(vkeep)
  
  ulleunggun = sig |> 
    filter(str_detect(SIG_KOR_NM, '울릉군'))
  
  geom_ulleunggun = ulleunggun |> 
    st_geometry()
  
  geom_dokdo = dokdo |> 
    st_geometry()
  
  
  geom_ulleunggun2 = st_union(geom_ulleunggun, geom_dokdo)
  ulleunggun2 = st_set_geometry(ulleunggun, geom_ulleunggun2)
  
  sig |> 
    filter(!str_detect(SIG_KOR_NM, '^울릉군$')) |> 
    union_all(ulleunggun2)
}

