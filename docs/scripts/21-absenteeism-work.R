## ----absenteism-work, include=FALSE-------------------------------------------
chap <- 22
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
if(!require(DataExplorer)) install.packages("DataExplorer")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(grid)) install.packages("grid")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(factoextra)) install.packages("factoextra")
if(!require(FactoMineR)) install.packages("FactoMineR")


## -----------------------------------------------------------------------------
library(tidyverse)
library(DataExplorer)
library(ggthemes)
library(grid)
library(gridExtra)
library(factoextra)
library(FactoMineR)


## ----eval=FALSE---------------------------------------------------------------
## absenteeism_at_work <- read_delim(na = "NA", delim = ";", col_types = cols(col = col_character()), "https://hranalytics.netlify.com/data/Absenteeism_at_work.csv")


## ----read_data9, echo=FALSE, warning=FALSE, message=FALSE---------------------
absenteeism_at_work <- read_delim(na = "NA", delim = ";", col_types = cols(col = col_character()), "data/Absenteeism_at_work.csv")


## -----------------------------------------------------------------------------
str(absenteeism_at_work)
summary(absenteeism_at_work)

# converting variables to factors

col <- c(2:5,12:17)
absenteeism_at_work_factored <- absenteeism_at_work
absenteeism_at_work_factored[col] <- lapply(absenteeism_at_work_factored[col], factor)

# converting codes to meaningful information

absenteeism_at_work_factored <- absenteeism_at_work_factored %>%
  mutate(`Reason for absence` = fct_recode(`Reason for absence`,`Infectious, parasitic diseases`="0", `Neoplasms`="1",`Diseases of the blood`="2",`Endocrine and metabolic diseases`="3",`Mental and behavioural disorders`="4",`Diseases of the nervous system`="5",`Diseases of the eye and adnexa`="6",`Diseases of the ear and mastoid process`="7",`Diseases of the circulatory system`="8",`Diseases of the respiratory system`="9",`Diseases of the digestive system`="10", `Diseases of the skin and subcutaneous tissue`="11",`Diseases of the musculoskeletal system and connective tissue`="12", `Diseases of the genitourinary system`="13",`Pregnancy, childbirth and the puerperium`="14",`Certain conditions originating in the perinatal`="15",  `Congenital malformations, deformations and chromosomal abnormalities`= "16",`Symptoms, signs and abnormal clinical  findings`="17", `Injury, poisoning and certain other consequences of external causes`= "18",`causes of morbidity and mortality`="19", `Factors influencing health status and contact with health services`="21",`patient follow-up`="22",`medical consultation`="23",`blood donation`="24", `laboratory examination`="25", `unjustified absence`="26", `physiotherapy`="27", `dental consultation`="28"))

absenteeism_at_work_factored <- absenteeism_at_work_factored %>%
  mutate(`Month of absence`= fct_recode(`Month of absence`,None="0",Jan="1",Feb="2",Mar="3",Apr="4",May="5", Jun="6",Jul="7",Aug="8",Sep="9",Oct="10",Nov="11",Dec="12") )

absenteeism_at_work_factored <- absenteeism_at_work_factored %>%
  mutate(Seasons= fct_recode(Seasons,summer="1",autumn="2",winter="3",spring="4"))

absenteeism_at_work_factored <- absenteeism_at_work_factored %>%
  mutate(Education = fct_recode(Education,highschool="1",graduate="2",postgraduate="3",`master& doctrate`="4"))

absenteeism_at_work_factored <- absenteeism_at_work_factored %>%
  mutate(`Disciplinary failure`= fct_recode(`Disciplinary failure`,No="0",Yes="1"))

absenteeism_at_work_factored <- absenteeism_at_work_factored %>%
  mutate(`Social drinker`= fct_recode(`Social drinker`,No="0",Yes="1"))

absenteeism_at_work_factored <- absenteeism_at_work_factored %>%
  mutate(`Social smoker`= fct_recode(`Social smoker`,No="0",Yes="1"))

absenteeism_at_work_factored <- absenteeism_at_work_factored %>%
  mutate(`Day of the week` = fct_recode(`Day of the week`, Monday="2", Tuesday="3", Wednesday="4", Thursday="5", Friday="6"))



## -----------------------------------------------------------------------------

p <- absenteeism_at_work_factored %>% 
  ggplot() +
  aes(x = Pet, fill = Pet) + 
  geom_bar() 

s <- absenteeism_at_work_factored %>% 
  ggplot() + 
  aes(x = Son, fill = Son) + 
  geom_bar()

SS <- absenteeism_at_work_factored %>% 
  ggplot() + 
  aes(x =`Social smoker`, fill =`Social drinker`) + 
  geom_bar() 

S <- absenteeism_at_work_factored %>% 
  ggplot() + 
  aes(x =   Seasons,fill = Seasons) + 
  geom_bar()

Day <- absenteeism_at_work_factored %>% 
  ggplot() + 
  aes(x =`Day of the week`, fill =`Day of the week`) + 
  geom_bar() 

grid.arrange(p,s, nrow = 1)
grid.arrange(SS,S, nrow = 1)
grid.arrange(Day, nrow = 1)


## -----------------------------------------------------------------------------
absent <- as.data.frame(absenteeism_at_work_factored %>% dplyr::select(everything()) %>% dplyr::filter(`Absenteeism time in hours` > 0))

season1 <- as.data.frame(absent %>% dplyr::group_by(Seasons) %>% dplyr::summarise(count= n(), percent = round(count*100/nrow(absent),1)) %>% arrange(desc(count)))

season1 %>%
ggplot() + 
  aes(x= reorder(Seasons,percent), y= percent, fill = Seasons) +
  geom_bar(stat='identity') + 
  coord_flip() +
  geom_text(aes(label = percent), vjust = 1.1, hjust = 1.2) + 
  xlab('Seasons')


## -----------------------------------------------------------------------------
disciplinary <- as.data.frame(absent %>% dplyr::group_by(`Disciplinary failure`) %>% dplyr::summarise(count= n(), percent = round(count*100/nrow(absent),1))%>% arrange(desc(count)))

disciplinary %>%
  ggplot() + 
  aes(x= reorder(`Disciplinary failure`,percent), 
      y= percent, fill = `Disciplinary failure`) + 
  geom_bar(stat='identity') + 
  coord_flip() +
  geom_text(aes(label = percent), vjust = 1.1, hjust = 1.2) + 
  xlab('Disciplinary failure')


## ---- fig.width=8.6, fig.height= 8.5------------------------------------------
                                      

Reason <-  as.data.frame(absent %>% group_by(`Reason for absence`) %>% dplyr::summarise(count= n(), percent = round(count*100/nrow(absent),1))%>% arrange(desc(count)))

Reason %>%
ggplot() + 
  aes(x = reorder(`Reason for absence`,percent), 
      y= percent, fill= `Reason for absence`) + 
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  theme(legend.position='none') +  
  geom_text(aes(label = percent), vjust = 0.5, hjust = 1.1) + 
  xlab('Reason for absence')


## -----------------------------------------------------------------------------
absent %>%
ggplot() + 
  aes(x= Age,y= `Absenteeism time in hours`,fill= `Social drinker`)+ 
  geom_bar(stat='identity',position= position_dodge()) + 
  scale_x_continuous(breaks =c(seq(20,60,5)),limits=c(20,60))



## -----------------------------------------------------------------------------

absent %>%
ggplot() + 
  aes(x= `Service time`,
      y= `Hit target`) + 
  geom_point() + 
  geom_smooth(method = 'loess') + 
  ggtitle('Analysis of Hit target across Service time') + 
  xlab('Service time(years)') + 
  ylab('Hit target(%)')



## -----------------------------------------------------------------------------

absent %>%
ggplot() + 
  aes(x= Age,y= `Hit target`) + 
  geom_point() + 
  geom_smooth(method = 'loess') + 
  labs(title='Analysis of Hit target across Age',
       x='Age',
       y='Hit target(%)')



## -----------------------------------------------------------------------------

absent %>%
ggplot() + 
  aes(x= Age,y= `Service time`) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  labs(title='Analysis of Service time across Age',
       x='Age',
       y='Service time')


## -----------------------------------------------------------------------------
absenteeism_at_work$`Work load Average/day ` <- as.numeric(as.character(absenteeism_at_work$`Work load Average/day ` ))

d1 <- absenteeism_at_work %>%
  dplyr::select(-ID) %>%
  dplyr::select(-`Absenteeism time in hours`)

d1 <- scale(d1)  


## -----------------------------------------------------------------------------
pcaex <- PCA(d1,graph = F) 

#The output of the function PCA() is a list including :
print(pcaex)

#The proportion of variation retained by the principal components (PCs) can be extracted as follow :

egv1 <- get_eigenvalue(pcaex)
head(egv1[, 1:2])

#Here, 60% of the information (variances) contained in the data are retained by the first six principal components.

#The amount of variation retained by each PC is called eigenvalues. The first PC corresponds to the direction with the maximum amount of variation in the data set.
#The importance of PCs can be visualized using a scree plot :

#Plot the eigenvalues/variances against the number of dimensions

# eigen values -
egv1 <- get_eigenvalue(pcaex)
fviz_eig(pcaex,addlabels=T)


## -----------------------------------------------------------------------------
# correlation of variables with PCA components-
fviz_pca_var(pcaex,col.var='red')

pcaex$var$contrib

# quality of presentation of variables in correlogram-
fviz_cos2(pcaex,choice='var',axes=1:2)

# contribution of variables to the respective principal components-
fviz_contrib(pcaex,choice='var',axes=1)
fviz_contrib(pcaex,choice='var',axes=2)
fviz_contrib(pcaex,choice='var',axes=3)
fviz_contrib(pcaex,choice='var',axes=4)
fviz_contrib(pcaex,choice='var',axes=5)

