library(tidyverse)
library(tidytransit)
library(sf)

# Goal: Make a map showing number of express trains by SEPTA Route

# Load in the GTFS folder
septa_gtfs <- read_gtfs("SEPTA_Rail.zip")

# Pulling trips run on Mondays

# Getting service ID
monday_id <- septa_gtfs$calendar %>%
  filter(monday == 1) %>%
  pull(service_id)

# Filtering so only trips with a service ID equaling "monday_id" are kept
monday_trips <- septa_gtfs$trips %>%
  filter(service_id == monday_id)

# Get the number of stops per route
route_variant_stop_num <- septa_gtfs$stop_times %>%
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
  mutate(trip_id = substr(trip_id, 1, 3)) %>%
  group_by(trip_id, first_stop, last_stop, stop_count) %>%
  reframe(trip_count = n())

route_variant_max_stop_num <- route_variant_stop_num %>%
  group_by(trip_id, first_stop, last_stop) %>%
  select(-trip_count) %>%
  filter(stop_count == max(stop_count)) %>%
  rename(max_stop_count = "stop_count") %>%
  merge(.,
        route_variant_stop_num, 
        by = c("trip_id", "first_stop", "last_stop")) %>%
  mutate(exp_loc = ifelse(stop_count == max_stop_count, "LOC", "EXP"))

exp_count <- route_variant_max_stop_num %>%
  group_by(trip_id, exp_loc) %>%
  reframe(trip_count = sum(trip_count)) %>%
  pivot_wider(names_from = "exp_loc",
              values_from = "trip_count") %>%
  mutate(EXP = ifelse(is.na(EXP), 0, EXP)) %>%
  select(-LOC)

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
  group_by(trip_id_short) %>%
  filter(stop_count == max(stop_count)) %>%
  group_by(trip_id_short) %>%
  slice(1) %>%
  merge(., septa_gtfs$trips, by = "trip_id") %>%
  select(route_id, shape_id)

map_df <- septa_gtfs$shapes %>%
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) %>%
  group_by(shape_id) %>%
  dplyr::summarize(do_union=FALSE) %>%
  st_cast("LINESTRING") %>% 
  st_transform(crs = 32618) %>%
  merge(., shapes, by = "shape_id") %>%
  merge(., exp_count, by.x = "route_id", by.y = "trip_id") %>%
  arrange(desc(EXP))
  
