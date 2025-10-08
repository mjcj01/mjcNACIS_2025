library(tidyverse)
library(sf)
library(tigris)

scheme = c(CYN = "#3d6eb0",
           TRE = "#da8bb3",
           CHW = "#65b9bb",
           PAO = "#4f8c42",
           MED = "#56a2dc",
           WIL = "#91c05b",
           AIR = "#a762b3",
           NOR = "#b73633",
           LAN = "#e16c35",
           WAR = "#f1cf4a",
           WTR = "#76599f",
           CHE = "#8d663c",
           FOX = "#e7b846")

counties <- st_read("Data//tl_2025_us_county.shp") %>%
  filter(CBSAFP == 37980)

ggplot() +
  geom_sf(data = counties, linewidth = 1, color = "#CCCCCC", fill = "#FFFFFF") +
  geom_sf(data = map_df, aes(color = route_id, linewidth = EXP)) +
  scale_color_manual(values = scheme) +
  guides(color = "none",
         linewidth = guide_legend(title = str_wrap("# of Express Trains", width = 10))) +
  scale_linewidth(range = c(1, 3)) +
  theme_void()
