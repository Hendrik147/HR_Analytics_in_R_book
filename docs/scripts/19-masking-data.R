## ----masking-data, include=FALSE----------------------------------------------
chap <- 20
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


## ----include=FALSE------------------------------------------------------------
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(randNames)) install.packages("randNames")
if(!require(smoothmest)) install.packages("smoothmest")


## -----------------------------------------------------------------------------
library(tidyverse)
library(randNames)
library(smoothmest)


## ----eval=FALSE---------------------------------------------------------------
## whitehouse <- read_csv("https://hranalytics.netlify.com/data/2016-Report-White-House-Staff.csv")


## ----read_data7, echo=FALSE, warning=FALSE, message=FALSE---------------------
whitehouse <- read_csv("data/2016-Report-White-House-Staff.csv")


## -----------------------------------------------------------------------------
#Create a set of new names of equal length as the original dataset

fake <- nrow(whitehouse) %>%
  rand_names (nat="US")

#Function to capitalise the first letter
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#Create a dataframe without the original names columns
whitehouse_with_no_names <- dplyr::select(whitehouse, -Name)

#Create a column of fake last and first names
fake$newnames <- paste0(firstup(fake$name.last), ", ", firstup(fake$name.first))

#Create a column of fake last and first names of exactly the length of the data frame without the original names
result <- fake[1:nrow(whitehouse_with_no_names),]

#Bind the dataset without names with the new dataset containing fake names
whitehouse_masked <- cbind(result$newnames, whitehouse_with_no_names)

#Rename the column title to "Name"
colnames(whitehouse_masked)[1] <- "Name"



## -----------------------------------------------------------------------------
# Set seed for reproducibility of the random numbers
set.seed(42)

# Replace names with random numbers from 1 to 1000
whitehouse_no_names <- whitehouse_masked %>%
  mutate(Name = sample(1:1000, nrow(whitehouse_masked)))


## -----------------------------------------------------------------------------
# Rounding Salary to the nearest ten thousand
whitehouse_no_identifiers <- whitehouse_no_names %>%
  mutate(Salary = round(Salary, digits = -4))

# Top coding convert the salaries into three categories
whitehouse.gen <- whitehouse_masked %>%
  mutate(Salary = ifelse(Salary < 50000, 0, 
                         ifelse(Salary >= 50000 & Salary < 100000, 1, 2)))

# Bottom Coding
whitehouse.bottom <- whitehouse_masked %>%
  mutate(Salary = ifelse(Salary<=45000, 45000, Salary))



## ----eval=FALSE---------------------------------------------------------------
## fertility <- read_csv("https://https://hranalytics.netlify.com/data/fertility_Diagnosis.txt", col_names = FALSE)


## ----read_data10, echo=FALSE, warning=FALSE, message=FALSE--------------------
fertility <- read_csv("data/fertility_Diagnosis.txt", col_names = FALSE)


## -----------------------------------------------------------------------------
# Let us assign significant column titles
colnames(fertility) <- c("Season", "Age", "Child_Disease", "Accident_Trauma", "Surgical_Intervention","High_Fevers", "Alcohol_Consumption","Smoking_Habit","Hours_Sitting","Diagnosis")

# View fertility data
fertility


## -----------------------------------------------------------------------------
# Number of participants with Surgical_Intervention and Diagnosis
fertility %>%
  group_by(Diagnosis) %>%
  summarise_at(vars(Surgical_Intervention), sum)

# Number of participants with Surgical_Intervention and Diagnosis
fertility %>%
  summarise_at(vars(Age), funs(mean, sd))

# Counts of the Groups in High_Fevers
fertility %>%
  count(High_Fevers)

# Counts of the Groups in Child_Disease	and Accident_Trauma	
fertility %>%
  count(Child_Disease,Accident_Trauma)

# Calculate the average of Child_Disease
fertility %>%
  summarise_at(vars(Child_Disease), mean)


## -----------------------------------------------------------------------------

fert <- fertility %>%
    mutate(Hours_Sitting = log(Hours_Sitting))

fert %>%
    summarise_at(vars(Hours_Sitting), funs(mean, sd))

set.seed(42)

hours.sit <- rnorm(100, -1.01, 0.50)

hours.sit <- exp(hours.sit)

hours.sit[hours.sit < 0] <- 0
hours.sit[hours.sit > 1] <- 1

range(hours.sit)

fert$Hours_Sitting <- hours.sit


## -----------------------------------------------------------------------------
# Square root Transformation of Salary
whitehouse.salary <- whitehouse_masked %>%
  mutate(Salary = sqrt(Salary))

# Calculate the mean and standard deviation
stats <- whitehouse.salary %>% 
  summarise_at(vars(Salary), funs(mean, sd))

# Generate Synthetic data with the same mean and standard deviation
salary_transformed <- rnorm(nrow(whitehouse_masked), mean(whitehouse.salary$Salary), sd(whitehouse.salary$Salary))

# Power transformation
salary_original <- salary_transformed^2

# Hard bound
salary <- ifelse(salary_original < 0, 0, salary_original)


## -----------------------------------------------------------------------------
fertility %>%
    summarise_at(vars(Child_Disease), sum)

library(smoothmest)

#rdoublex(draws, mean, shaping) 

#rdoublex is a random number generator. It creates a vector of random numbers generated by the double exponential distribution.


## -----------------------------------------------------------------------------
set.seed(42)

rdoublex(1, 87, 1 / 10)
#[1] 87.01983

set.seed(42)    
rdoublex(1, 87, 1 / 0.1)
#[1] 88.98337


## -----------------------------------------------------------------------------

#Set Value of Epsilon
eps <- 0.1 / 2

# GS of Mean and Variance
gs.mean <- 0.01
gs.var <- 0.01

# Apply the Laplace mechanism
set.seed(42)
rdoublex(1, 0.41, gs.mean / eps)
#[1] 0.4496674
rdoublex(1, 0.19, gs.var / eps)
#[1] 0.2466982



## -----------------------------------------------------------------------------
#High_Fevers and Mean of Hours_Sitting

fertility %>%
  filter(High_Fevers >= 0) %>%
  summarise_at(vars(Hours_Sitting), mean)

#Hours_Sitting 0.3932967

# No High_Fevers and Mean of Hours_Sitting
fertility %>%
  filter(High_Fevers == -1) %>%
  summarise_at(vars(Hours_Sitting), mean)

#Hours_Sitting 0.5433333

#Set Value of Epsilon
eps <- 0.1

# GS of mean for Hours_Sitting
gs.mean <- 1 / 100

# Apply the Laplace mechanism
set.seed(42)
rdoublex(1, 0.39, gs.mean / eps)
#[1] 0.4098337
rdoublex(1, 0.54, gs.mean / eps)
#[1] 0.5683491


## -----------------------------------------------------------------------------
# Set Value of Epsilon
eps <- 0.01
# GS of counts
gs.count <- 1

fertility %>%
  count(Smoking_Habit)

#Smoking Count
#-1      56
# 0      23
# 1     21

#Apply the Laplace mechanism
set.seed(42)

smoking1 <- rdoublex(1, 56, gs.count / eps / 2) %>%
  round()

smoking2 <- rdoublex(1, 23, gs.count / eps / 2) %>%
  round()

# Post-process based on previous queries
smoking3 <- nrow(fertility) - smoking1 - smoking2

# Checking the noisy answers
smoking1
#[1] 60
smoking2
#[1] 29
smoking3
#[1] 11


## -----------------------------------------------------------------------------

# Set Value of Epsilon
eps <- 0.01
# GS of counts
gs.count <- 1

# Display Participants with Abnormal Diagnosis
Number_abnormal <- fertility %>% filter(Diagnosis=="O") %>% summarise(sum_x1 = sum(Diagnosis=="O"))

#Negative Counts: Applying the Laplace mechanism
# Apply the Laplace mechanism and set.seed(22)
set.seed(22)
rdoublex(1, 12, gs.count / eps) %>%
  round()
#[1] -79

# Apply the Laplace mechanism and set.seed(22)
set.seed(22)
rdoublex(1, 12, gs.count / eps) %>%
  round() %>%
  max(0)
#[1] 0

# Suppose we set a different seed
set.seed(12)
noisy_answer <- rdoublex(1, 12, gs.count / eps) %>%
  round() %>%
  max(0)

n <- nrow(fertility)

# ifelse example
ifelse(noisy_answer > n, n, noisy_answer)
#[1] 100

#Normalising
# Set Value of Epsilon
eps <- 0.01
# GS of Counts
gs.count <- 1
fertility %>%
  count(Smoking_Habit)

#Smoking Count
# -1     56
#  0     23
#  1     21

# Apply the Laplace mechanism and set.seed(42)
set.seed(42)
smoking1 <- rdoublex(1, 56, gs.count / eps / 2) %>%
  max(0)
smoking2 <- rdoublex(1, 23, gs.count / eps / 2) %>%
  max(0)
smoking3 <- rdoublex(1, 21, gs.count / eps / 2) %>%
  max(0)

# Checking the noisy answers
smoking <- c(smoking1, smoking2, smoking3)
smoking
#[1] 65.91684 37.17455 0.00000

# Normalize smoking
normalized <- (smoking/sum(smoking)) * (nrow(fertility))

# Round the values
round(normalized)
#[1] 64 36 0


