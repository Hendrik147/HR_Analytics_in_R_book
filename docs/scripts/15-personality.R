## ----personality, include=FALSE-----------------------------------------------
chap <- 16
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


## ----include=FALSE, warning=FALSE, message=FALSE------------------------------
if(!require(httr)) install.packages("httr")
if(!require(janeaustenr)) install.packages("janeaustenr")
if (!require(ibmsunburst)) install.packages("ibmsunburst")


## ----eval=FALSE---------------------------------------------------------------
## key = "aOWMNztQ_VVlz9fINhc3v67rtnJqcN6JuubQorAvhq"
## url = "https://gateway.watsonplatform.net/personality-insights/api/v3/profile?version=2017-10-13"
## uname="a4a4ea65-e8e7-492c-a95e-128f10fc5f"
## pword="LuFm4BELs"


## ----eval=FALSE---------------------------------------------------------------
## library(httr)
## library(janeaustenr)
## cr=POST(url,
##     authenticate(uname, pword),
##     content_type("text/plain;charset=utf-8"),
##     accept_json(),
##     body=paste(janeaustenr::emma, collapse = " ")
## )
## status_code(cr)


## ----eval=FALSE---------------------------------------------------------------
## library(ibmsunburst)
## ibmsunburst(content(cr), version = "v3")

