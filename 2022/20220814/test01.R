library(sp)

tb_map1 = readRDS("~/data/map/gadm/gadm36_KOR_0_sp.rds")

sido_sf <- st_as_sf(tb_map1)
plot(sido_sf)
