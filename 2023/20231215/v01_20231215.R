Sys.setlocale("LC_ALL", "ko_KR.UTF-8")
source('./geo_core.R')
read_csv()

enframe(iconvlist()) |> 
  

try(utils::head(iconvlist(), n = 50))

# https://www.r-bloggers.com/2022/03/character-in-utf-8/
# iconv("\xb5\xbf\xb4\xeb\xb9\xae\xb1\xb8", from = "EUC-KR", to = 'UTF-8')

korea = read_sf('~/Documents/map/sig_20230729/sig.shp')
korea = korea |> 
  mutate(SIG_KOR_NM = iconv(SIG_KOR_NM, from = "EUC-KR", to = 'UTF-8'))
  

str(korea)

jongno_gu = korea |> 
  filter(SIG_CD == '11110')
jongno_gu
plot(jongno_gu)

# jongno_gu_sim = st_simplify(jongno_gu, dTolerance = 100)
# jongno_gu_sim
# plot(jongno_gu_sim)
# 
# jongno_gu_sim2 = ms_simplify(jongno_gu, keep = 0.01, keep_shapes = T)
# jongno_gu_sim2
# plot(jongno_gu_sim2)

korea
korea_sim2 = ms_simplify(korea, keep = 0.01, keep_shapes = T)

korea_sim2

# terra::crs(korea_sim2) = "EPSG:5179"
korea_sim2_crs = st_set_crs(korea_sim2, 5179)
korea_sim2_crs

# Simple feature collection with 250 features and 3 fields
# Geometry type: GEOMETRY
# Dimension:     XY
# Bounding box:  xmin: 746110.3 ymin: 1464333 xmax: 1302021 ymax: 2065895
# CRS:           NA
# 
# Simple feature collection with 250 features and 3 fields
# Geometry type: GEOMETRY
# Dimension:     XY
# Bounding box:  xmin: 746110.3 ymin: 1464333 xmax: 1302021 ymax: 2065895
# Projected CRS: Korea 2000 / Unified CS

st_crs("EPSG:5179")
ggplot(korea_sim2_crs) +
  geom_sf() 

ggplot(korea_sim2_crs) +
  geom_sf() +
  coord_sf(crs = sf::st_crs(3112))

ggplot(korea_sim2_crs) +
  geom_sf() +
  coord_sf(crs = sf::st_crs(4326))

ggplot(korea_sim2_crs) +
  geom_sf() +
  coord_sf(crs = sf::st_crs(5179))

st_bbox(korea_sim2_crs)

ggplot(korea_sim2_crs) +
  geom_sf() +
  coord_sf(crs = sf::st_crs(5179),
           xlim = c(746110.3, 1302020.6), ylim = c(1464332.9, 2065895.0))


# 경도 위도를 EPSG:5179 거리체계로 
# 위도 : latitude 가로줄, y축.
# 경도 : longitude 세로줄, x축.
lim_data = tibble(lon = c(126, 127), lat = c(36, 37)) |> 
  st_as_sf(coords = c("lon", "lat")) |> 
  st_set_crs("EPSG:4326") |> 
  st_transform("EPSG:5179")
lim_data
tibble(lon = c(126, 127), lat = c(36, 37)) |> 
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") 

tibble(lon = c(126, 127), lat = c(36, 37)) |> 
  st_as_sf(coords = c("lon", "lat")) |> 
  st_set_crs("EPSG:4326") 

# lim_data
  
korea_sim2_crs |> 
  mutate(SI_CD = str_sub(SIG_CD, 1,2), .before = SIG_CD) |> 
  distinct(SI_CD)

korea_sim2_crs = korea_sim2_crs |> 
  mutate(SI_CD = str_sub(SIG_CD, 1,2), .before = SIG_CD) 

#################################################
# 
# %in% c('11', '28', '41', '43', '51')
#################################################
korea_sim2_crs

korea_sim2_crs |> distinct(SI_CD)

ggplot(korea_sim2_crs) +
  geom_sf(aes(fill = SI_CD), colour = 'gray80') +
  scale_fill_gradient()

ggplot(korea_sim2_crs) +
  geom_sf(aes(fill = SI_CD), colour = 'gray80') +
  scale_fill_brewer(palette = "GnBu") +
  coord_sf(crs = sf::st_crs(4326),
           xlim = c(126, 128), ylim = c(37, 38)) 
           
ggplot(korea_sim2_crs) +
  geom_sf() +
  coord_sf(crs = sf::st_crs(4326),
           xlim = c(126, 127), ylim = c(36, 37))

ggplot(korea_sim2_crs) +
  geom_sf() +
  coord_sf(crs = sf::st_crs(5179),
           xlim = c(864803.8, 955511.8), ylim = c(1779174, 1889174))


london = data.frame(lon = -0.1, lat = 51.5) |> 
  st_as_sf(coords = c("lon", "lat"))

st_is_longlat(london)
london_geo = st_set_crs(london, "EPSG:4326")
london_geo = st_set_crs(london, "EPSG:5179")
london_geo
st_is_longlat(london_geo)


london_proj = data.frame(x = 530000, y = 180000) |> 
  st_as_sf(coords = c("x", "y"), crs = "EPSG:27700")


london_proj
st_is_longlat(london_proj)

london_geo
london2 = st_transform(london_geo, "EPSG:27700")
london2

st_crs(korea_sim2_crs)
world
st_crs(world)


world_dfr = st_read(system.file('shapes/world.shp', package = 'spData'))


world_dfr

plot(world["pop"])

india = world[world$name_long == "India", ]
world_asia = world[world$continent == "Asia", ]

plot(st_geometry(india), expandBB = c(0,0.2, 0.1, 1), col = "gray", lwd = 3)
plot(st_geometry(world_asia), add = T)


raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
my_rast = rast(raster_filepath)
class(my_rast)

plot(my_rast) 

rast



world_df = st_drop_geometry(world)  
  


nz_elev = rast(system.file("raster/nz_elev.tif", package = "spDataLarge"))
nz_elev

spData::world %>% 
  filter(str_detect(name_long, 'Korea'))

kr = spData::world %>% 
  filter(str_detect(iso_a2, 'KR'))

spData::nz


nz
# Add fill layer to nz shape
tm_shape(nz) +
  tm_fill() 
# Add border layer to nz shape
tm_shape(us_states) +
  tm_borders() 
# Add fill and border layers to nz shape
tm_shape(us_states) +
  tm_fill() +
  tm_borders() 



canterbury = nz |> filter(Name == "Canterbury")
canterbury
str(canterbury)
str(nz_height)
nz_height
nz_height[canterbury, ]
canterbury_height = nz_height[canterbury, ]