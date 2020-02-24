## ----absenteism-MFG, include=FALSE--------------------------------------------------------------------------------------------------
chap <- 21
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
if(!require(RcmdrMisc)) install.packages("RcmdrMisc")
if(!require(rattle)) install.packages("rattle")
if(!require(scatterplot3d)) install.packages("scatterplot3d")
if(!require(caret)) install.packages("caret")

library(tidyverse)
library(modelr)


## ----eval=FALSE---------------------------------------------------------------------------------------------------------------------
## MFGEmployees <- read_csv("https://hranalytics.netlify.com/data/MFGEmployees4.csv")


## ----read_data8, echo=FALSE, warning=FALSE, message=FALSE---------------------------------------------------------------------------
MFGEmployees <- read_csv("data/MFGEmployees4.csv")


## -----------------------------------------------------------------------------------------------------------------------------------
summary(MFGEmployees)


## -----------------------------------------------------------------------------------------------------------------------------------
MFGEmployees<-
  MFGEmployees %>% 
  filter(Age>=18 & Age<=65)


## -----------------------------------------------------------------------------------------------------------------------------------
summary(MFGEmployees)


## -----------------------------------------------------------------------------------------------------------------------------------
MFGEmployees <- 
  MFGEmployees %>%
  mutate(AbsenceRate = AbsentHours /2080*100)

str(MFGEmployees)


## -----------------------------------------------------------------------------------------------------------------------------------
MFGEmployees %>% 
  ggplot() + 
  aes(x=BusinessUnit) + 
  geom_bar() +
  labs(x="Business Units",
       y="Count",
       title="Employee Count By Business Units") +
  theme_minimal() + 
  coord_flip()

MFGEmployees %>% 
  ggplot() + 
  aes(x=Gender) + 
  geom_bar() +
  labs(x="Business Units",
       y="Count",
       title="Employee Count By Gender") +
  theme_minimal() + 
  coord_flip()

MFGEmployees %>% 
  ggplot() + 
  aes(x=Division) + 
  geom_bar() +
  labs(x="Division",
       y="Count",
       title="Employee Count By Division") +
  theme_minimal() + 
  coord_flip()


## -----------------------------------------------------------------------------------------------------------------------------------

MFGEmployees %>%
  summarise(median = median(AbsenceRate), mean = mean(AbsenceRate)) #Average absence rate and number of observations

means <- aggregate(AbsenceRate ~ 1, MFGEmployees, mean)

ggplot() + 
  geom_boxplot(aes(y = AbsenceRate, x =1), data = MFGEmployees) +
  coord_flip() +
  geom_text(data = round(means, digits = 3), aes(y = AbsenceRate, x =1, label = AbsenceRate),  check_overlap=TRUE) + #Add average absence rate as a text
  theme_minimal()



## -----------------------------------------------------------------------------------------------------------------------------------
ggplot() + 
  geom_boxplot(aes(y = AbsenceRate, x = Gender), data = MFGEmployees) + 
  coord_flip() +
  theme_minimal()

AnovaModel.1 <- lm(AbsenceRate ~ Gender, data=MFGEmployees) %>% 
  Anova()

AnovaModel.1 

#means
MFGEmployees %>% group_by(Gender) %>% summarise(avg=mean(AbsenceRate))


## -----------------------------------------------------------------------------------------------------------------------------------
ggplot() + 
  geom_boxplot(aes(y = AbsenceRate, x = Division), data = MFGEmployees) + 
  coord_flip() +
  theme_minimal()

AnovaModel.2 <- lm(AbsenceRate ~ Division, data=MFGEmployees) %>% 
  Anova()

AnovaModel.2

# means
MFGEmployees %>% group_by(Division) %>% summarise(avg=mean(AbsenceRate))


## -----------------------------------------------------------------------------------------------------------------------------------

AnovaModel.3 <- lm(AbsenceRate ~ Division*Gender, data=MFGEmployees) %>% 
  Anova()

AnovaModel.3

#means
MFGEmployees %>% group_by(Division, Gender) %>% summarise(avg=mean(AbsenceRate))


## -----------------------------------------------------------------------------------------------------------------------------------

# basic scatterplot
MFGEmployees %>%
ggplot() +
  aes(x=Age, y=AbsenceRate) + 
  geom_point(colour = "blue") +
  theme_minimal()

cor(MFGEmployees$Age, MFGEmployees$AbsenceRate)


## -----------------------------------------------------------------------------------------------------------------------------------
MFGEmployees %>%
ggplot() +
  aes(x=LengthService, y=AbsenceRate) + 
  geom_point(colour = "blue") +
  geom_smooth(method='lm', se = FALSE, color='red') +
  theme_minimal()

cor(MFGEmployees$LengthService, MFGEmployees$AbsenceRate)


## -----------------------------------------------------------------------------------------------------------------------------------
MFGEmployees %>%
ggplot() +
  aes(x=Age, y=LengthService) + 
  geom_point(colour = "blue") +
  geom_smooth(method='lm', se = FALSE, color='red') +
  theme_minimal()

cor(MFGEmployees$Age, MFGEmployees$LengthService)


## -----------------------------------------------------------------------------------------------------------------------------------
MYinput <- c("Gender", "DepartmentName", "StoreLocation", "Division",
             "Age", "LengthService", "BusinessUnit")
MYtarget  <- "AbsenceRate"

library(rpart, quietly=TRUE)

set.seed(crv$seed)
MYrpart <- rpart(AbsenceRate ~ .,
                 data=MFGEmployees[, c(MYinput, MYtarget)],
                 method="anova",
                 parms=list(split="information"),
                 control=rpart.control(minsplit=10,
                                       maxdepth=10,
                                       usesurrogate=0, 
                                       maxsurrogate=0))

fancyRpartPlot(MYrpart, main="Decision Tree MFGEmployees and AbsenceRate")


## -----------------------------------------------------------------------------------------------------------------------------------
#Linear Regression Model
RegressionCurrentData <- lm(AbsenceRate~Age+LengthService, data=MFGEmployees)

summary(RegressionCurrentData)



## -----------------------------------------------------------------------------------------------------------------------------------
#2D plot of Age and AbsenceRate

ggplot() + 
  geom_point(aes(x = Age,y = AbsenceRate),data=MFGEmployees) +
  geom_smooth(aes(x = Age,y = AbsenceRate),data=MFGEmployees,method = 'lm') +
  theme_minimal()

#3D Scatterplot  of Age and Length of Service with Absence Rate - with Coloring and Vertical Lines
# and Regression Plane 
library(scatterplot3d) 

s3d <-scatterplot3d(MFGEmployees$Age,MFGEmployees$LengthService,MFGEmployees$AbsenceRate, pch=16, highlight.3d=TRUE,
                    type="h", main="Absence Rate By Age And Length of Service")
fit <- lm(MFGEmployees$AbsenceRate ~ MFGEmployees$Age+MFGEmployees$LengthService) 

s3d$plane3d(fit)



## -----------------------------------------------------------------------------------------------------------------------------------
library(caret)

set.seed(998)

inTraining <- createDataPartition(MFGEmployees$BusinessUnit, p = .75, list = FALSE)

training <- MFGEmployees[inTraining,]
testing <- MFGEmployees[ - inTraining,]

fitControl <- trainControl(## 10-fold CV
method = "repeatedcv", number = 10, 
repeats = 10 ## repeated ten times
)

set.seed(825)

lmFit1 <- train(AbsenceRate ~ Age + LengthService, data = training,
                 method = "lm",
                 trControl = fitControl)
lmFit1

Testingdatasetandpredictions <- testing %>% add_predictions(lmFit1, type = "raw")

Testingdatasetandpredictions$pred[Testingdatasetandpredictions$pred<0] <- 0 #Put a zero to all negative predictions



## -----------------------------------------------------------------------------------------------------------------------------------

set.seed(825)

rpartFit1 <- train(AbsenceRate ~ Age + LengthService, data = training,
                 method = "rpart",
                 trControl = fitControl,
                 maxdepth = 5)
rpartFit1


## -----------------------------------------------------------------------------------------------------------------------------------

set.seed(825)

rpartFit2 <- train(AbsenceRate ~ Gender + DepartmentName + StoreLocation + Division + Age + LengthService + BusinessUnit, data = training,
                 method = "rpart",
                 trControl = fitControl,
                 maxdepth = 5)

rpartFit2



## -----------------------------------------------------------------------------------------------------------------------------------
#Apply model
#Generate 2016 data
Absence2016Data<-MFGEmployees
Absence2016Data$Age<-Absence2016Data$Age+1
Absence2016Data$LengthService<-Absence2016Data$LengthService+1

Absence2016Data <- Absence2016Data %>% add_predictions(lmFit1, type = "raw")
Absence2016Data$pred[Absence2016Data$pred<0] <- 0 #Put a zero to all negative predictions



## -----------------------------------------------------------------------------------------------------------------------------------
mean(Absence2016Data$pred)
mean(MFGEmployees$AbsenceRate)

