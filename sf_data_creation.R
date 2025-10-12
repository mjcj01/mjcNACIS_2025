library(sf)
library(tidyverse)

# This data frame is the same as route_variant_stop_number until line 20
# in this file.
shapes <- septa_gtfs$stop_times %>%
  filter(trip_id %in% monday_trips$trip_id) %>%
  select(trip_id, stop_id, stop_sequence) %>%
  group_by(trip_id) %>%
  arrange(stop_sequence, .by_group = TRUE) %>%
  mutate(stop_id = ifelse(stop_id %in% c(90004, 90005, 90006, 90007, 90406, 90008, 90208, 90705),
                          "CC", stop_id),
         first_stop = first(stop_id),
         last_stop = last(stop_id)) %>%
  merge(., septa_gtfs$trips %>% select(trip_id, direction_id), by = "trip_id") %>%
  select(-stop_sequence) %>%
  unique() %>%
  group_by(trip_id, first_stop, last_stop, direction_id) %>%
  reframe(stop_count = n()) %>%
  mutate(trip_id_short = substr(trip_id, 1, 3)) %>%
  # Filtering so only the maximum length stop counts are used for each route. 
  # This way we can get the full length of every route.
  group_by(trip_id_short) %>%
  filter(stop_count == max(stop_count)) %>%
  group_by(trip_id_short) %>%
  # Taking the first value in every route group, as they will all have the same
  # shape_id value when that data frame is merged with this one.
  slice(1) %>%
  # Merging with the trips data frame to get the shape_id column.
  merge(., septa_gtfs$trips, by = "trip_id") %>%
  select(route_id, shape_id)

map_df <- septa_gtfs$shapes %>%
  # Converting the shapes data frame into an sf object with a geometry column.
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) %>%
  group_by(shape_id) %>%
  # Full disclosure: this line is necessary but I don't fully understand why.
  dplyr::summarize(do_union = FALSE) %>%
  # Turning the sf objects from points into a linestring.
  st_cast("LINESTRING") %>% 
  # Setting projection to UTM Zone 18N.
  st_transform(crs = 32618) %>%
  # Merging with the data frame created above.
  merge(., shapes, by = "shape_id") %>%
  # Merging with the table of express train counts per route.
  merge(., exp_count, by.x = "route_id", by.y = "trip_id") %>%
  # Arranging where the data frame descends 
  arrange(desc(EXP))