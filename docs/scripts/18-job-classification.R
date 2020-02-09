## ----job-classification, include=FALSE----------------------------------------
chap <- 19
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
list.of.packages <- c("plyr", "dplyr",  "ROCR", "caret", "randomForest",
"kernlab", "magrittr", "rpart", "ggplot2", "nnet", "car",
"rpart.plot", "pROC", "ada", "readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
	install.packages(new.packages)


## -----------------------------------------------------------------------------
library(tidyverse)
library(caret)
library(rattle)
library(rpart)
library(randomForest)
library(kernlab)
library(nnet)
library(car)
library(rpart.plot)
library(pROC)
library(ada)


## ----eval=FALSE---------------------------------------------------------------
## MYdataset <- read_csv("https://https://hranalytics.netlify.com/data/jobclassinfo2.csv")


## ----read_data6, echo=FALSE, warning=FALSE, message=FALSE---------------------
MYdataset <- read_csv("data/jobclassinfo2.csv")


## ----message=FALSE, warning=FALSE---------------------------------------------
str(MYdataset)
summary(MYdataset)


## -----------------------------------------------------------------------------
MYnobs <- nrow(MYdataset) # The data set is made of 66 observations

MYsample <- MYtrain <- sample(nrow(MYdataset), 0.7*MYnobs) # 70% of those 66 observations (i.e. 46 observations) will form our training dataset.

MYvalidate <- sample(setdiff(seq_len(nrow(MYdataset)), MYtrain), 0.14*MYnobs) # 14% of those 66 observations (i.e. 9 observations) will form our validation dataset.

MYtest <- setdiff(setdiff(seq_len(nrow(MYdataset)), MYtrain), MYvalidate) # # The remaining observations (i.e. 11 observations) will form our test dataset.


# The following variable selections have been noted.
MYinput <- c("EducationLevel", "Experience", "OrgImpact", "ProblemSolving",
     "Supervision", "ContactLevel", "FinancialBudget")

MYnumeric <- c("EducationLevel", "Experience", "OrgImpact", "ProblemSolving",
     "Supervision", "ContactLevel", "FinancialBudget")

MYcategoric <- NULL

MYtarget  <- "PG"
MYrisk    <- NULL
MYident   <- "ID"
MYignore  <- c("JobFamily", "JobFamilyDescription", "JobClass", "JobClassDescription", "PayGrade")
MYweights <- NULL



## -----------------------------------------------------------------------------

MYdataset %>%
  ggplot() +
  aes(x = factor(PG)) +
  geom_bar(stat = "count", width = 0.7, fill = "steelblue") +
  theme_minimal() + 
  coord_flip() +
  ggtitle("Number of job classifications per PG category") 

MYdataset %>%
  ggplot() +
  aes(x = factor(JobFamilyDescription)) +
  geom_bar(stat = "count", width = 0.7, fill = "steelblue") +
  theme_minimal() + 
  coord_flip() +
  ggtitle("Number of job classifications per job family") 

MYdataset %>%
  ggplot() +
  aes(EducationLevel) + 
  geom_bar(stat = "count", width = 0.7, fill = "steelblue")  +
  ggtitle("Number of job classifications per Education level") 

MYdataset %>%
  ggplot() +
  aes(Experience) + 
  geom_bar(stat = "count", width = 0.7, fill = "steelblue")  +
  ggtitle("Number of job classifications per experience") 

MYdataset %>%
  ggplot() +
  aes(OrgImpact) + 
    geom_bar(stat = "count", width = 0.7, fill = "steelblue")  +
  ggtitle("Number of job classifications per organisational impact") 

MYdataset %>%
  ggplot() +
  aes(ProblemSolving) + 
    geom_bar(stat = "count", width = 0.7, fill = "steelblue")  +
  ggtitle("Number of job classifications per problem solving") 

MYdataset %>%
  ggplot() +
  aes(Supervision) + 
    geom_bar(stat = "count", width = 0.7, fill = "steelblue")  +
  ggtitle("Number of job classifications per supervision") 

MYdataset %>%
  ggplot() +
  aes(ContactLevel) + 
  geom_bar(stat = "count", width = 0.7, fill = "steelblue")  +
  ggtitle("Number of job classifications per contact level") 



## -----------------------------------------------------------------------------
library(caret)

MYdataset$PG <- as.factor(MYdataset$PG)

featurePlot(x = MYdataset[,7:13], 
            y = MYdataset$PG, 
            plot = "density", 
            auto.key = list(columns = 2))

featurePlot(x = MYdataset[,7:13], 
            y = MYdataset$PG, 
            plot = "box", 
            auto.key = list(columns = 2))


## -----------------------------------------------------------------------------
# The 'rattle' package provides a graphical user interface to very many other packages that provide functionality for data mining.

library(rattle)

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.
crv$seed <- 42 
set.seed(crv$seed)

# Build the Decision Tree model.

MYrpart <- rpart(PG ~ .,
    data=MYdataset[, c(MYinput, MYtarget)],
    method="class",
    parms=list(split="information"),
      control=rpart.control(minsplit=10,
           minbucket=2,
           maxdepth=10,
        usesurrogate=0, 
        maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(MYrpart)
printcp(MYrpart)
cat("\n")



## -----------------------------------------------------------------------------

# The 'randomForest' package provides the 'randomForest' function.

library(randomForest, quietly=TRUE)

# Build the Random Forest model.

set.seed(crv$seed)

MYrf <- randomForest::randomForest(PG ~ ., # PG ~ .
      data=MYdataset[,c(MYinput, MYtarget)], 
      ntree=500,
      mtry=2,
      importance=TRUE,
      na.action=randomForest::na.roughfix,
      replace=FALSE)

# Generate textual output of 'Random Forest' model.

MYrf

# List the importance of the variables.

rn <- round(randomForest::importance(MYrf), 2)
rn[order(rn[,3], decreasing=TRUE),]



## -----------------------------------------------------------------------------

# The 'kernlab' package provides the 'ksvm' function.

library(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

#set.seed(crv$seed)
MYksvm <- ksvm(as.factor(PG) ~ .,
      data=MYdataset[,c(MYinput, MYtarget)],
      kernel="rbfdot",
      prob.model=TRUE)

# Generate a textual view of the SVM model.

MYksvm



## -----------------------------------------------------------------------------

# Build a multinomial model using the nnet package.

library(nnet, quietly=TRUE)

# Summarise multinomial model using Anova from the car package.

library(car, quietly=TRUE)

# Build a Regression model.

MYglm <- multinom(PG ~ ., data=MYdataset[,c(MYinput, MYtarget)], trace=FALSE, maxit=1000)

# Generate a textual view of the Linear model.

rattle.print.summary.multinom(summary(MYglm,
                              Wald.ratios=TRUE))
cat(sprintf("Log likelihood: %.3f (%d df)
", logLik(MYglm)[1], attr(logLik(MYglm), "df")))
if (is.null(MYglm$na.action)) omitted <- TRUE else omitted <- -MYglm$na.action
cat(sprintf("Pseudo R-Square: %.8f

",cor(apply(MYglm$fitted.values, 1, function(x) which(x == max(x))),
as.integer(MYdataset[omitted,]$PG))))

cat('==== ANOVA ====')
print(Anova(MYglm))



## -----------------------------------------------------------------------------

# Plot the resulting Decision Tree. 

# We use the rpart.plot package.

fancyRpartPlot(MYrpart, main="Decision Tree MYdataset $ PG")



## -----------------------------------------------------------------------------

# Predict new job classsifications utilising the Decision Tree model.

MYpr <- predict(MYrpart, newdata=MYdataset[,c(MYinput, MYtarget)], type="class")

# Generate the confusion matrix showing counts.

table(MYdataset[,c(MYinput, MYtarget)]$PG, MYpr,
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions and misclassification error in the last column. Misclassification error, represents how often is the prediction wrong,

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x)
  tbl <- cbind(x/length(actual),
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(MYdataset[,c(MYinput, MYtarget)]$PG, MYpr)
round(per, 2)


# First we calculate the overall miscalculation rate (also known as error rate or percentage error).
#Please note that diag(per) extracts the diagonal of confusion matrix.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2)) # 23%

# Calculate the averaged miscalculation rate for each job classification. 
# per[,"Error"] extracts the last column, which represents the miscalculation rate per.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))  # 28%



## -----------------------------------------------------------------------------
# Generate the Confusion Matrix for the Random Forest model.

# Obtain the response from the Random Forest model.

MYpr <- predict(MYrf, newdata=na.omit(MYdataset[,c(MYinput, MYtarget)]))

# Generate the confusion matrix showing counts.

table(na.omit(MYdataset[,c(MYinput, MYtarget)])$PG, MYpr,
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x)
  tbl <- cbind(x/length(actual),
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(na.omit(MYdataset[,c(MYinput, MYtarget)])$PG, MYpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))



## -----------------------------------------------------------------------------
# Generate the Confusion Matrix for the SVM model.

# Obtain the response from the SVM model.

MYpr <- kernlab::predict(MYksvm, newdata=na.omit(MYdataset[,c(MYinput, MYtarget)]))

# Generate the confusion matrix showing counts.

table(na.omit(MYdataset[,c(MYinput, MYtarget)])$PG, MYpr,
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x)
  tbl <- cbind(x/length(actual),
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(na.omit(MYdataset[,c(MYinput, MYtarget)])$PG, MYpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))



## -----------------------------------------------------------------------------
# Generate the confusion matrix for the linear regression model.

# Obtain the response from the Linear model.

MYpr <- predict(MYglm, newdata=MYdataset[,c(MYinput, MYtarget)])

# Generate the confusion matrix showing counts.

table(MYdataset[,c(MYinput, MYtarget)]$PG, MYpr,
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x)
  tbl <- cbind(x/length(actual),
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(MYdataset[,c(MYinput, MYtarget)]$PG, MYpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))



## ----eval=FALSE---------------------------------------------------------------
## DeployDataset <- read_csv("https://hranalytics.netlify.com/data/Deploydata.csv")


## ----read_data11, echo=FALSE, warning=FALSE, message=FALSE--------------------
DeployDataset <- read_csv("data/Deploydata.csv")


## -----------------------------------------------------------------------------
DeployDataset

PredictedJobGrade <- predict(MYrf, newdata=DeployDataset)
PredictedJobGrade

