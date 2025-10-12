library(tidyverse)
library(tidytransit)

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
  # Filter so only trip IDs that run on Monday are kept
  filter(trip_id %in% monday_trips$trip_id) %>%
  select(trip_id, stop_id, stop_sequence) %>%
  group_by(trip_id) %>%
  # Making sure stop sequence is arranged in sequential order so first() and
  # last() on lines 32 and 33 work properly. They should be sequential, but
  # easier to just run this as a safety measure.
  arrange(stop_sequence, .by_group = TRUE) %>%
  # Removing stops that contextually make sense to remove (e.g., low ridership stops that
  # are frequently skipped on otherwise local trains) and the Center City stations.
  # Some trips have different Center City stations listed as a terminal stop even on
  # the same routes, so this makes it easier to collapse all CC stations as one.
  mutate(stop_id = ifelse(stop_id %in% c(90004, 90005, 90006, 90007, 90406, 90008, 90208, 90705),
                          "CC", stop_id),
         first_stop = first(stop_id),
         last_stop = last(stop_id)) %>%
  # Bringing in direction IDs (i.e., in- or out-bound from Philadelphia).
  merge(., septa_gtfs$trips %>% select(trip_id, direction_id), by = "trip_id") %>%
  # Removing stop_sequence so that unique() runs properly.
  select(-stop_sequence) %>%
  unique() %>%
  group_by(trip_id, first_stop, last_stop, direction_id) %>%
  # For each trip, the number of rows are counted as a way of counting the number
  # of stops a train makes.
  reframe(stop_count = n()) %>%
  # This keeps only the first three characters in the trip_id string, which are
  # route abbreviations.
  mutate(trip_id = substr(trip_id, 1, 3)) %>%
  group_by(trip_id, first_stop, last_stop, stop_count) %>%
  # For each route abbreviation, every combination of first stop, last stop,
  # and stop count are tallied.
  reframe(trip_count = n())

# The goal of this data frame is to find out, for each first and last stop
# pair, what is the maximum number of stops a train makes. Later, we will
# compare this maximum number of stops to all variants to see which ones are
# express trains and skip some stops.
route_variant_max_stop_num <- route_variant_stop_num %>%
  group_by(trip_id, first_stop, last_stop) %>%
  select(-trip_count) %>%
  # Filtering so for each route, first stop, and last stop combination only the
  # variants with the most stops are kept.
  filter(stop_count == max(stop_count)) %>%
  rename(max_stop_count = "stop_count") %>%
  # Merging with the previously created data frame.
  merge(.,
        route_variant_stop_num, 
        by = c("trip_id", "first_stop", "last_stop")) %>%
  # If/Else statement where if a variant has the same number of stops as the max 
  # amount, it is labeled a local. Otherwise, it is an express.
  mutate(exp_loc = ifelse(stop_count == max_stop_count, "LOC", "EXP"))

exp_count <- route_variant_max_stop_num %>%
  group_by(trip_id, exp_loc) %>%
  # Counting the number of local express trains for each route.
  reframe(trip_count = sum(trip_count)) %>%
  # Turning from long to wide data format (i.e., column for each group instead
  # of one column for group name and one column for values).
  pivot_wider(names_from = "exp_loc",
              values_from = "trip_count") %>%
  # Turning NAs in the express column into 0s, as not every route has express
  # trains.
  mutate(EXP = ifelse(is.na(EXP), 0, EXP)) %>%
  select(-LOC)