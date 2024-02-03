source('./geo_core.R')

sido = sido_with_dokdo()


ggplot(sido) +
  geom_sf(fill = "#F8DA84", colour = "#0C2430") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#0C2430"))
