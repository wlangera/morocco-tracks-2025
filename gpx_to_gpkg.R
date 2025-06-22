library(sf)
library(tidyverse)

source("calculate_elevation_gain.R")

# Get gpx files
gpx_files <- list.files(pattern = "*.gpx")

# Proces gpx files
tracks_list <- lapply(gpx_files, function(path) {
    track_points_df_raw <- st_read(path, layer = "track_points", quiet = TRUE)
    tracks_df_raw <- st_read(path, layer = "tracks", quiet = TRUE)
    
    # Get variables
    track_date <- unique(lubridate::date(track_points_df_raw$time))
    elevation_start <- pull(head(track_points_df_raw, 1), "ele")
    elevation_end <- pull(tail(track_points_df_raw, 1), "ele")
    
    # Create output df
    track_points_df <- tracks_df_raw %>%
      select(geometry) %>%
      mutate(
        date = format(track_date, "%d/%m/%Y"),
        ele_start = elevation_start,
        ele_end = elevation_end,
        dist = st_length(tracks_df_raw),
        ascent = calculate_elevation_gain(track_points_df_raw),
        descent = calculate_elevation_gain(track_points_df_raw, ascent = FALSE),
        dist_label = paste0(round(as.numeric(dist) / 1000, 2), " km ",
                            "(", round(ascent, 1), " m ↑, ",
                            round(descent, 1), " m ↓", ")"),
      ) %>%
      select(everything(), geometry)
    
    return(track_points_df)
  }
)

# Read track names
track_names <- read_csv2("track_names.csv") %>%
  mutate(date = format(lubridate::dmy(date), "%d/%m/%Y"))

tracks <- bind_rows(tracks_list) %>%
  left_join(track_names, by = join_by(date)) %>%
  mutate(label = paste0(nr, ": ", start, " - ", end)) %>%
  select(nr, start, end, date, everything(), geometry)

st_write(tracks, "hiking_tracks.gpkg", append = FALSE)
