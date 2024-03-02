source('./geo_core.R')

sig = sig_with_dokdo() |> 
  clean_names()
sig


sido = sido_with_dokdo(vkeep = 0.01)
sido
seoul = sido |> filter(CTPRVN_CD == '11')
grid1 = expand.grid(x = seq(700000, 1500000, by = 7000), 
                    y = seq(1400000, 2100000, by = 7000))

grid1
sf_grid1 = tibble(grid1) |> 
  mutate(geometry = map2(x,y, polygon_square, gap = 7000)) |> 
  pull(geometry) |> 
  st_sfc(crs = st_crs(5179)) |> 
  st_sf(geometry = _)

sf_grid1

log_within = st_within(sf_grid1, sido) |> 
  lengths() > 0

# ggplot() +
#   geom_sf(data = sf_grid1) +
#   geom_sf(data = seoul)

ggplot() +
  geom_sf(data = sf_grid1[log_within,], fill = "#DB8582", colour = "#DB8582")

ggplot() +
  geom_sf(data = sf_grid1[log_within,])

ggplot() +
  geom_sf(data = seoul)

