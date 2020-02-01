#' Get distance data between two points based on all the travel mode options. Works for many origin points.
#'
#' @param x A vector of origins in address or postcode format
#' @param dest A single destinationin address or postocde format
#' @param arrival_time A POSIXct datetime that folks need to arrive by
#' @param key A google distance API key
#' @param ... Additional options to pass to `google_distance()`
#'
#' @return Data.frame containing (typically) 4 rows per input element
#'
#' @export

google_distance =  function(x, dest, arrival_time, key, ...){

  # simple hygeine stuff
  gd = purrr::possibly(
    memoise::memoise(
      googleway::google_distance)
    , "Fail"
  )

  # Prep dataset
  interested_in = expand.grid(from=x,
                              mode=c("driving", "walking", "bicycling", "transit"),
                              stringsAsFactors = FALSE)
  # Perform google_distance calls for all combos
  purrr::map2(interested_in$from,interested_in$mode,
              ~gd(.x, dest, mode=.y,
                  arrival_time = arrival_time,
                  key=key)
  ) %>%
    # Extract relevant section
    purrr::map("rows") %>%
    purrr::map("elements") %>%
    purrr::flatten() %>%
    # Simplify the data.frames
    purrr::map(unclass) %>%
    purrr::map_df(purrr::flatten) %>%
    # Add original lookup values
    cbind(interested_in)
}
