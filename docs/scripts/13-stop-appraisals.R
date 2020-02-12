## ----stop-appraisals, include=FALSE-------------------------------------------------------------------------------------------------
chap <- 14
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
## Time.spent.on.appraisals <- read_csv("https://hranalytics.netlify.com/data/Time.spent.on.appraisals.csv")


## ----read_appraisals_data, echo=FALSE, warning=FALSE, message=FALSE-----------------------------------------------------------------
Time.spent.on.appraisals <- read_csv("data/Time.spent.on.appraisals.csv")


## -----------------------------------------------------------------------------------------------------------------------------------
plot(Hours ~ Count.of.Appraisee,        # plot the variables y ~ x
     xlab="Number of appraisee",        # x−axis label 
     ylab="Hours",                      # y−axis label
     data=Time.spent.on.appraisals)     # data set

model <- lm(Hours ~ Count.of.Appraisee, data=Time.spent.on.appraisals)
abline(model, col = "red")

#The following produces the formula of the linear regression
paste('y =', round(coef(model)[[2]], digits = 3), '* x', '+', round(coef(model)[[1]], digits = 3)) 



## -----------------------------------------------------------------------------------------------------------------------------------

# First let us identify the outliers and plot the initial plot with the names of the appraiser

# Calculate Mahalanobis Distance with height and weight distributions
m_dist <- mahalanobis(Time.spent.on.appraisals[,2:3], colMeans(Time.spent.on.appraisals[,2:3], na.rm = TRUE), cov(Time.spent.on.appraisals[,2:3], use="complete.obs"))
Time.spent.on.appraisals$m_dist <- round(m_dist, 2)

# A quick check by sorting the value will indicate the threshold you may want to use:
sort(Time.spent.on.appraisals$m_dist)


## -----------------------------------------------------------------------------------------------------------------------------------

# We chose to set the threshold at 4, as this would result in five outliers. There is no "rule of thumb" on where to set the threshold and the decision is up to the analyst.

# Mahalanobis Outliers - Threshold set to 4
Time.spent.on.appraisals$outlier_maha <- "No"
Time.spent.on.appraisals$outlier_maha[Time.spent.on.appraisals$m_dist > 4] <- "Yes"

# Scatterplot with Mahalanobis' Outliers
ggplot(Time.spent.on.appraisals, aes(x = Count.of.Appraisee, y = Hours, color = outlier_maha)) +
      geom_point(size = 5, alpha = 0.6, na.rm=TRUE) +    # na.rm=TRUE removes missing values error message silently
      xlim(0, 40) + ylim(0, 600) +
      labs(title = "Number of appraisees vs Hours",
           subtitle = "Outlier Detection in time spent on appraisals data - Using Mahalanobis Distances") +
      ylab("Hours") + xlab("Number of appraisees")

model_without_mahaoutliers <- lm(Hours ~ Count.of.Appraisee, data=Time.spent.on.appraisals[which(Time.spent.on.appraisals$outlier_maha== "No"),])


#The following produces the formula of the new linear regression without the Mahalanobis outliers

paste('y =', round(coef(model_without_mahaoutliers)[[2]], digits = 3), '* x', '+', round(coef(model_without_mahaoutliers)[[1]], digits = 3)) 




## ----eval=FALSE---------------------------------------------------------------------------------------------------------------------
## Time.spent.on.appraisals <- read_csv("https://https://hranalytics.netlify.com/data/Time.spent.on.appraisals.csv")


## ----read_appraisals_data2, echo=FALSE, warning=FALSE, message=FALSE----------------------------------------------------------------
Time.spent.on.appraisals <- read_csv("data/Time.spent.on.appraisals.csv")


## -----------------------------------------------------------------------------------------------------------------------------------
plot(Hours ~ Count.of.Appraisee,        # plot the variables y ~ x
     xlab="Number of appraisee",        # x−axis label 
     ylab="Hours",                      # y−axis label
     data=Time.spent.on.appraisals)     # data set

model <- lm(Hours ~ Count.of.Appraisee, data=Time.spent.on.appraisals)
abline(model, col = "red")

cooksd <- cooks.distance(model)

# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(Time.spent.on.appraisals)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")         # plot cook's distance
abline(h = 4/sample_size, col="red")         # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels

# Removing Outliers

# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])

Time.spent.on.appraisals_screen <- Time.spent.on.appraisals[-influential, ]

plot3 <- ggplot(data = Time.spent.on.appraisals, aes(x = Count.of.Appraisee, y = Hours)) +
        geom_point(size = 2, alpha = 0.6, na.rm=TRUE) + 
        xlim(0, 40) + ylim(0, 600) +
        geom_smooth(method = lm, na.rm=TRUE) +             # geom_smooth 
        ggtitle("Before")

plot4 <- ggplot(data = Time.spent.on.appraisals_screen, aes(x = Count.of.Appraisee, y = Hours)) +
        geom_point(size = 2, alpha = 0.6, na.rm=TRUE) + 
        xlim(0, 40) + ylim(0, 600) +
        geom_smooth(method = lm, na.rm=TRUE) +
        ggtitle("After")

gridExtra::grid.arrange(plot3, plot4, ncol=2)


model_screen <- lm(Hours ~ Count.of.Appraisee, data=Time.spent.on.appraisals_screen)

#The following produces the formula of the new linear regression without the outliers

paste('y =', round(coef(model_screen)[[2]], digits = 3), '* x', '+', round(coef(model_screen)[[1]], digits = 3)) 


