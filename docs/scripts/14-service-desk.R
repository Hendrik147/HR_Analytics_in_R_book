## ----service-desk, include=FALSE----------------------------------------------------------------------------------------------------
chap <- 15
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


## ----include=FALSE------------------------------------------------------------------------------------------------------------------
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(DataExplorer)) install.packages("DataExplorer")
if(!require(RcmdrMisc)) install.packages("RcmdrMisc")
if(!require(qcc)) install.packages("qcc")


## -----------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)


## ----eval=FALSE---------------------------------------------------------------------------------------------------------------------
## service_requests <- read_csv("https:///hranalytics.netlify.com/data/ServiceRequestExtract2.csv")


## ----read_data_servicedesk, echo=FALSE, warning=FALSE, message=FALSE----------------------------------------------------------------
service_requests <- read_csv("data/ServiceRequestExtract2.csv")


## -----------------------------------------------------------------------------------------------------------------------------------
service_requests %>% 
  mutate(DateStarted = coalesce(DateStarted, DateSubmitted),
         DateCompleted=coalesce(DateCompleted, DateStarted + hours(2))) %>% 
  mutate(DateCompleted = 
           pmin(DateCompleted,
              DateStarted + hours(floor(rnorm(n(), mean = 71, sd=20))))) ->
  service_requests


## -----------------------------------------------------------------------------------------------------------------------------------
service_requests %>% 
  mutate(RequestID = as.character(RequestID)) %>% 
  mutate(
    WaitTime = difftime(DateStarted, 
                        DateSubmitted, 
                        units = "hours")
    ,TaskTime = difftime(DateCompleted, 
                        DateStarted, 
                        units = "hours")
    ,TotalTime = difftime(DateCompleted, 
                        DateSubmitted, 
                        units = "hours")) %>% 
  mutate_at(vars(ends_with("Time")), as.numeric)->
  service_requests

service_requests


## -----------------------------------------------------------------------------------------------------------------------------------
library(DataExplorer)
plot_density(service_requests, 
             title = "Distribution of task times",
             ggtheme = theme_minimal())


## -----------------------------------------------------------------------------------------------------------------------------------
plot_bar(service_requests,
         title="Distributions",
         ggtheme = theme_minimal())



## -----------------------------------------------------------------------------------------------------------------------------------
service_requests %>% 
  group_by(Category) %>% 
  summarise_at(vars(ends_with("Time")),
              .funs = c("mean","min","max")) %>% 
  arrange(WaitTime_mean)


## -----------------------------------------------------------------------------------------------------------------------------------
library(RcmdrMisc)
lm(WaitTime ~ Category, data=service_requests) %>% 
  Anova()


## -----------------------------------------------------------------------------------------------------------------------------------
lm(TaskTime ~ Category, data=service_requests) %>% 
  Anova()


## -----------------------------------------------------------------------------------------------------------------------------------
lm(TotalTime ~ Category, data=service_requests) %>% 
  Anova()


## -----------------------------------------------------------------------------------------------------------------------------------
library(qcc)

service_requests %>% 
  {qcc.groups(.$WaitTime, .$RequestID)} %>% 
  qcc(type="xbar.one") %>% 
  summary()


## -----------------------------------------------------------------------------------------------------------------------------------
service_requests %>% 
  {qcc.groups(.$TaskTime, .$RequestID)} %>% 
  qcc(type="xbar.one") %>% 
  summary()


## -----------------------------------------------------------------------------------------------------------------------------------
service_requests %>% 
  {qcc.groups(.$TotalTime, .$RequestID)} %>% 
  qcc(type="xbar.one") %>% 
  summary()


## -----------------------------------------------------------------------------------------------------------------------------------
# Need to get categories being added as titles
service_requests %>% 
  {split(., .$Category)} %>% 
  map(~qcc.groups(.$TotalTime, .$RequestID)) %>% 
  map(qcc, type ="xbar.one")


## ----eval=FALSE---------------------------------------------------------------------------------------------------------------------
## it_helpdesk <- read_csv("https://hranalytics.netlify.com/data/WA_Fn-UseC_-IT-Help-Desk.csv")


## ----read_data_ithelpdesk, echo=FALSE, warning=FALSE, message=FALSE-----------------------------------------------------------------
it_helpdesk <- read_csv("data/WA_Fn-UseC_-IT-Help-Desk.csv")


## -----------------------------------------------------------------------------------------------------------------------------------
it_helpdesk %>% 
  ggplot() +
  aes(x=ITOwner) +
  geom_bar() +
  labs(x="IT Owner", 
       y="Number of tickets", 
       title="Tickets by IT Owner") +
  theme_minimal()


## -----------------------------------------------------------------------------------------------------------------------------------
it_helpdesk %>% 
  ggplot() +
  aes(x=daysOpen) +
  geom_bar() +
  labs(x="Number of days ticket was open for", 
       y="Number of tickets", 
       title="Time to resolve/close tickets") +
  theme_minimal()


## -----------------------------------------------------------------------------------------------------------------------------------
it_helpdesk %>% 
  count(Requestor) %>% 
  ggplot() +
  aes(x=n) +
  geom_density() +
  labs(x="Number of tickets raised per person", 
       y="Density", 
       title="Distribution of tickets per person") +
  theme_minimal() 

