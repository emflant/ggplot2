source('./geo_core.R')



sig = sig_with_dokdo()
sig
ggplot(sig) +
  geom_sf(aes(fill=SIG_CD)) +
  scale_fill_viridis_d(option = "B") +
  theme(legend.position = "none")

