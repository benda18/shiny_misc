
#https://www.phind.com/search?cache=l03sym9ouxpt1edour3u6wb2

# Define the function
calculate_distance <- function(lon1, lat1, lon2, lat2) {
  require(geosphere)
  distHaversine(c(lon1, lat1), c(lon2, lat2))
}

