## ----interview-attendance, include=FALSE--------------------------------------------------------------------------------------------
chap <- 25
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
library(tidyverse)


## ----eval=FALSE---------------------------------------------------------------------------------------------------------------------
## interview_attendance <- read_csv("https://hranalyticslive.netlify.com/data/interview.csv")


## ----read_interview_data, echo=FALSE, warning=FALSE, message=FALSE------------------------------------------------------------------
interview_attendance <- read_csv("data/interview.csv")


## -----------------------------------------------------------------------------------------------------------------------------------
head(interview_attendance, 5)


## -----------------------------------------------------------------------------------------------------------------------------------

interview_attendance <- interview_attendance[-1234,] #remove the last row that contains only missing values

interview_attendance$X24   <- NULL #get rid of unnecesary columns on the right side
interview_attendance$X25 <- NULL #get rid of unnecesary columns on the right side
interview_attendance$X26 <- NULL #get rid of unnecesary columns on the right side
interview_attendance$X27 <- NULL #get rid of unnecesary columns on the right side
interview_attendance$X28 <- NULL #get rid of unnecesary columns on the right side

# Here we create a vector with column titles
mycolnames <-c('date_of_interview','client_name','industry','location','position',
               'skillset','interview_type','name', 'gender','current_location',
               'cjob_location','interview_venue','cnative_location',
               'permission_obtained','unscheduled_meetings','call_three_hours_before',
               'alternative_number','printout_resume_jd','clear_with_venue',
               'letter_been_shared','expected_attendance','observed_attendance',
               'marital_status')  

#Here we assign the previously defined column titles
colnames(interview_attendance) <- mycolnames 

rm(mycolnames) # Here we remove the mycolnames vector, as it is not required anymore

interview_attendance<- mutate_all(interview_attendance, funs(tolower)) # Sets all words to lower case

# Cancels all empty spaces in the observed attendance column
interview_attendance$observed_attendance <- gsub(" ", "", interview_attendance$observed_attendance)

# Cancels all empty spaces in the location column
interview_attendance$location <- gsub(" ", "", interview_attendance$location) 

# Cancels all empty spaces in the interview type column
interview_attendance$interview_type <- gsub(" ", "", interview_attendance$interview_type)

# Corrects a typo in the interview type column
interview_attendance$interview_type <- gsub("sceduledwalkin", "scheduledwalkin", interview_attendance$interview_type)

# Cancels all empty spaces in the candidate current location column
interview_attendance$current_location <- gsub(" ", "", interview_attendance$current_location)


#Converts values from character to numbers for Yes/no answers, just to keep things simple.
  colstoyesno <- c(14:22) # Here we define which column numbers to look at.
for (i in 1:length(colstoyesno)){   # Here we tell R to examine all variables in the previously defined columns
  j <- colstoyesno[i]
  interview_attendance[,j][interview_attendance[,j] !="yes"] <- "no"   
  interview_attendance[,j][is.na(interview_attendance[,j]) == TRUE] <- "no"
  #With the previous two lines all values different to yes, become a no, i.e. "uncertain" and "NA" are set to a "no".
}
rm(colstoyesno, i, j) #Here we remove the three just created vectors as a claen up.


## -----------------------------------------------------------------------------------------------------------------------------------
dir.create("codefiles_interview_attendance", showWarnings = FALSE)
#detach("package:plyr", unload = TRUE)
for(i in 1:length(colnames(interview_attendance))){
  vvar <- colnames(interview_attendance)[i]
  outdata <- interview_attendance %>% dplyr::group_by(.dots = vvar) %>% dplyr::count(.dots = vvar)
  outdata$idt <- LETTERS[seq(from=1, to=nrow(outdata))]
  outdata$id  <- row.names(outdata)
  outfile <- paste0("codefiles_interview_attendance/", vvar, ".csv")
  write.csv(outdata, file=outfile, row.names = FALSE)
}
rm(outdata, i, outfile, vvar)


## ----eval = FALSE-------------------------------------------------------------------------------------------------------------------
## colstomap <- c(2:7, 9:21, 23)
## library(plyr)
## for(i in 1:length(colstomap)){
##     j <- colstomap[i]
##     vfilename <- paste0("codefiles_interview_attendance/", colnames(interview_attendance)[j], ".csv")
##     dfcodes <- read.csv(vfilename, stringsAsFactors=FALSE)
##     vfrom <- as.vector(dfcodes[,1])
##     vto <- as.vector(dfcodes[,4])
##     interview_attendance[,j] <- mapvalues(interview_attendance[,j], from=vfrom, to=vto)
##     interview_attendance[,j] <- as.integer(interview_attendance[,j])
## }
## rm(colstomap, i, j, vfilename, vfrom, vto, dfcodes)


## ----eval = FALSE-------------------------------------------------------------------------------------------------------------------
## interview_attendanceml <- interview_attendance %>% dplyr::select(-date_of_interview, -name)
## interview_attendanceml <- interview_attendanceml %>% dplyr::select(client_name:expected_attendance,
##                                 observed_attendance)
## head(interview_attendanceml, 5)


## ----eval = FALSE-------------------------------------------------------------------------------------------------------------------
## 
## library(caret) # Calls the caret library
## 
## set.seed(144) # Sets a seed for reproducability
## 
## index <- createDataPartition(interview_attendanceml$observed_attendance, p=0.75, list=FALSE) #Creates an index vector of the length of all the observations
## 
## interview_attendanceml_train <- interview_attendanceml[index,] # Creates a subset of 75% of data for training dataset
## interview_attendanceml_test  <- interview_attendanceml[-index,] # Creates a subset of the remaining 25% of data for test dataset
## 
## rm(index,interview_attendanceml)


## ----eval = FALSE-------------------------------------------------------------------------------------------------------------------
## myml_model <- train(interview_attendanceml_train[,1:19], interview_attendanceml_train[,20], method='gbm')
## 
## summary(myml_model)
## 
## predictions <- predict(object = myml_model, interview_attendanceml_test,
##                        type = 'raw')
## 
## head(predictions)
## 
## print(postResample(pred=predictions, obs=as.factor(interview_attendanceml_test[,20])))

