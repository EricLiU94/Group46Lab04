library(nycflights13)
library(dplyr)
library(ggplot2)
library(maps)

#' Visualize airport delays
#'
#' @export visualize_airport_delays
#' @description Creates a plot that visualizes the mean (arrival) delay of flights for different airports by longitude and latitude.
#' @return A plot

visualize_airport_delays <- function() {
  requireNamespace("nycflights13")
  requireNamespace("dplyr")
  requireNamespace("ggplot2")
  requireNamespace("maps")
  
  # Select the destination and delay upon arrival for each flight
  my_flights <- dplyr::select(nycflights13::flights, c(dest, arr_delay))
    # Remove flights that haven't taken place
  my_flights <- dplyr::filter(my_flights, is.na(arr_delay) == FALSE)
    # If a flight was early, this means the delay was 0
  my_flights <- dplyr::mutate(my_flights, arr_delay = dplyr::if_else(arr_delay < 0, 0, arr_delay))
    # Group the flight per destination
  my_flights <- dplyr::group_by(my_flights, dest)
    # Summarize the data to obtain the mean delay
  my_flights <- dplyr::summarise(my_flights, avg_delay = mean(arr_delay))
    # Rename the faa code to dest for merging
  my_flights <- dplyr::rename(my_flights, faa = dest)
  
  # Select the airports
  my_airports <- dplyr::select(nycflights13::airports, faa:lon)
    # Join data frames. With a full join, NA's are kept, these airports should be added to airports
  my_airports <- dplyr::left_join(my_airports, my_flights, by = "faa")
    # Remove airports with no delay (no flights)
  my_airports <- dplyr::filter(my_airports, is.na(avg_delay) == FALSE)
    # Rename lon to long
  my_airports <- dplyr::rename(my_airports, long = lon)
    # If the longitude is less than -125 (Alaska and Hawaii) move these
  my_airports <- dplyr::mutate(my_airports, long = dplyr::if_else(long < -125, -125, long))
    # If the longitude is less than -125 (Alaska) move these
  my_airports <- dplyr::mutate(my_airports, lat = dplyr::if_else(lat > 50, 50, lat))
  
  # Plot the map
  ggplot2::ggplot(ggplot2::map_data("usa"), ggplot2::aes(x = long, y = lat, group = group)) +
    ggplot2::geom_polygon(fill = "grey", alpha = 0.3, colour = "white") +
    # Plot our data
    ggplot2::geom_point(data = my_airports, ggplot2::aes(x = long, y = lat, group = faa, size = avg_delay, color = avg_delay)) +
    # Add titles to the points we moved
    ggplot2::geom_text(data = my_airports, group = my_airports$name, 
              label = ifelse(my_airports$long == -125, my_airports$name, 
                             ifelse(my_airports$lat == 50, my_airports$name, "")), hjust = -0.1) +
    # Alter labels
    ggplot2::labs(title = "Average flight delay per destination \n2013 (minutes)", size = "Average \ndelay", color = "") +
    # Alter the theme
    ggplot2::theme_void() + 
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.title = ggplot2::element_text(hjust = 0.5))
}