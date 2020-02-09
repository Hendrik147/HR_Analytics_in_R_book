## ----ranking-medical1, include=FALSE------------------------------------------
chap <- 26
lc <- 0
rq <- 0
# **`r paste0("(LC", chap, ".", (lc <- lc + 1), ")")`**
# **`r paste0("(RQ", chap, ".", (rq <- rq + 1), ")")`**

knitr::opts_chunk$set(
  tidy = FALSE, 
  out.width = '\\textwidth', 
  fig.height = 4,
  warning = TRUE
  )

options(scipen = 99, digits = 3)

# Set random number generator see value for replicable pseudorandomness. Why 76?
# https://www.youtube.com/watch?v=xjJ7FheCkCU
set.seed(76)


## -----------------------------------------------------------------------------
library(tidyverse)
library(pdftools)
library(data.table)
library(bigstatsr)
library(plotly)
library(stringr)
#library(gsubfn)


## -----------------------------------------------------------------------------
Sys.setenv(TZ = "Europe/London")
Sys.setlocale(locale="fr_FR.UTF-8")


## ----eval=FALSE---------------------------------------------------------------
## #pdfdocument <- "https://goo.gl/wUXvjk" #Internet download
## #pdfdocument <- "https://hranalyticslive.netlify.com/data/liste_classement_ecn_20170628.pdf"


## ----read_data_medical_schools, echo=FALSE, warning=FALSE, message=FALSE------
pdfdocument <- "data/liste_classement_ecn_20170628.pdf"


## -----------------------------------------------------------------------------

txt <- pdftools::pdf_text(pdfdocument)
head(txt, n = 1) #Inspection of first page

data <- strsplit(txt, "\n")

head(data)

data_parsed <- matrix(NA_character_, length(data), 7)
data_words <- str_extract_all(data, boundary("word"))
data_parsed[, 1:4] <- t(sapply(data_words, head, n = 4))
data_parsed[, 5:7] <- t(sapply(data_words, tail, n = 3))
head(data_parsed)

data_parsed2 <- as_tibble(data_parsed) %>%
  transmute(
    ranking = as.integer(V1),
    is_male = (V2 == "M"),
    family_name = V3,
    first_name = V4,
    birth_date = pmap(list(V5, V6, V7), function(d, m, y) {
      paste(d, m, y, collapse = " ")
    }) %>% lubridate::dmy()
  ) 

data_parsed2


## ----eval=FALSE---------------------------------------------------------------
## # Proportion male/female
## mean(data_parsed2$is_male)
## # 43% of males.
## 
## 
## myggplot <- function(...) bigstatsr:::MY_THEME(ggplot(...))
## 
## myggplot(data_parsed2) +
##   geom_histogram(aes(x = birth_date), bins = 100)
## 


## ----eval=FALSE---------------------------------------------------------------
## myggplot(mutate(data_parsed2, prop_male = cummean(data_parsed2$is_male))) +
##   geom_hline(yintercept = mean(data_parsed2$is_male), col = "red") +
##   geom_line(aes(x = ranking, y = prop_male))
## 
## (myggplot(data_parsed2) +
##    geom_point(aes(ranking, birth_date, color = is_male)) +
##    aes(text = bigstatsr::asPlotlyText(data_parsed2))) %>%
##   plotly::ggplotly(tooltip = "text")


## ----eval=FALSE---------------------------------------------------------------
## myggplot(data_parsed2, aes(ranking, birth_date)) +
##   geom_point() +
##   geom_smooth(aes(color = is_male), lwd = 2)
## 

