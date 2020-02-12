## ----commuting-time, include=FALSE--------------------------------------------------------------------------------------------------
chap <- 17
lc <- 0
rq <- 0
# **`r paste0("(LC", chap, ".", (lc <- lc + 1), ")")`**
# **`r paste0("(RQ", chap, ".", (rq <- rq + 1), ")")`**

knitr::opts_chunk$set(
  tidy = FALSE, 
  out.width = '\\textwidth', 
  fig.height = 4,
  warning = FALSE
  )

options(scipen = 99, digits = 3)

# Set random number generator see value for replicable pseudorandomness. Why 76?
# https://www.youtube.com/watch?v=xjJ7FheCkCU
set.seed(76)


## -----------------------------------------------------------------------------------------------------------------------------------
do_eval=file.exists("commute_times.csv")


## -----------------------------------------------------------------------------------------------------------------------------------
library(googleway)
library(tidyverse)
library(lubridate)

# Set up here your api key key=
key = 'AIzaSyCOly69PDrlPlM42I378p2lmvNs8I2w'

d <- dmy_hms("10/09/2018 08:30")

arrival <- as.numeric(d)


from <- c("SE3+8UQ,UK")
to <- c("E14+5EU,UK")

test <- google_distance(origins = from,
                        destinations = to,
                        mode = "walking",
                        arrival_time = as.POSIXct(arrival, origin = "1970-01-01", tz = "UTC"),
                        key=key)

test$rows$elements[[1]]$distance



## -----------------------------------------------------------------------------------------------------------------------------------
#' Get distance data between two points based on all the travel mode options. Works for many origin points.
#'
#' @param x A vector of origins in address or postcode format
#' @param dest A single destinationin address or postocde format
#' @param arrival_time A POSIXct datetime that folks need to arrive by
#' @param key A google distance API key
#' @param ... Additional options to pass to `google_distance()`
#'
#' @return Data.frame containing (typically) 4 rows per input element

google_distance_all =  function(x, dest, arrival_time, key, ...){
  
  # simple hygeine stuff
  gd = purrr::possibly(
    memoise::memoise(
      google_distance)
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


## -----------------------------------------------------------------------------------------------------------------------------------
if(!require(purrr)) install.packages("purrr")
if(!require(memoise)) install.packages("memoise")
if(!require(googleway)) install.packages("googleway")


## -----------------------------------------------------------------------------------------------------------------------------------
office = "E14 5EU"
monday_9am = as.POSIXct("2018-12-03 09:00")


## ----eval=FALSE---------------------------------------------------------------------------------------------------------------------
## the_200 <- read_csv("https://hranalytics.netlify.com/data/200_staff_members.csv")


## ----read_data_commuting, echo=FALSE, warning=FALSE, message=FALSE------------------------------------------------------------------
the_200 <- read_csv("data/200_staff_members.csv")


## ----eval=do_eval-------------------------------------------------------------------------------------------------------------------
## 
## results = google_distance_all(
##   the_200$Postcode,
##   office,
##   arrival_time = monday_9am,
##   key = key
## )
## 
## write_csv(results, file.path("data"), "commute_times.csv")


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------
results = read_csv("data/commute_times.csv")


## -----------------------------------------------------------------------------------------------------------------------------------
results


## -----------------------------------------------------------------------------------------------------------------------------------
results %>% 
  filter(mode == "driving") %>% 
  mutate(hours=value/60^2) %>% 
  top_n(10, hours) %>% 
  arrange(desc(hours))


## -----------------------------------------------------------------------------------------------------------------------------------
results %>% 
  mutate(hours=value/60^2) %>% 
  ggplot() +
  aes(x=hours) +
  geom_density() +
  scale_x_log10() +
  facet_wrap(~mode) +
  theme_minimal()

