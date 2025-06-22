calculate_elevation_gain <- function(df, ascent = TRUE) {
  # Calculate the difference between consecutive elevation points
  elevation_diff <- diff(df$ele)
  
  if (ascent) {
    # Total ascent: sum of all positive elevation differences
    return(sum(elevation_diff[elevation_diff > 0], na.rm = TRUE))
  } else {
    return(sum(abs(elevation_diff[elevation_diff < 0]), na.rm = TRUE))
  }
}