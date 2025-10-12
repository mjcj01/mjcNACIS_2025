library(tidyverse)
library(sf)

# Old SEPTA Regional Rail route colors.
scheme <- c(CYN = "#3d6eb0",
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

# Pulling in counties for some basemap content.
# Dealer's choice on what else to include.
counties <- st_read("Data//tl_2025_us_county.shp") %>%
  filter(CBSAFP == 37980)

ggplot() +
  # Loading counties layer. ggplot2 layers things bottom up based on what is loaded
  # first. So, because counties are a basemap item, this should be loaded in first.
  geom_sf(data = counties, linewidth = 1, color = "#CCCCCC", fill = "#FFFFFF") +
  # Loading in the map_df created in sf_data_creation.R. The color of lines is defined
  # by the route ID and the line width is defined by the number of express trains.
  geom_sf(data = map_df, aes(color = route_id, linewidth = EXP)) +
  labs(title = str_wrap("SEPTA Express Trains Around the Philadelphia Metropolitan Area",
                        width = 50)) +
  # This allows the scheme object to define the colors of route IDs.
  scale_color_manual(values = scheme) +
  # Removes the route ID colors from the legend, and sets the title for the line width
  # part.
  guides(color = "none",
         linewidth = guide_legend(title = str_wrap("# of Express Trains", width = 10))) +
  # Settting the range of line widths to 1pt to 3pt.
  scale_linewidth(range = c(1, 3)) +
  # Removing all default ggplot2 styling except legend and data (e.g., grid lines, axis
  # titles, etc).
  theme_void()
