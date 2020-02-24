## -----------------------------------------------------------------------------------------------------------------------------------
library(readr)


## ----eval=FALSE---------------------------------------------------------------------------------------------------------------------
## LTI_Analysis <- read_delim(na = "NA", delim = ";", col_types = cols(col = col_character()), "https://hranalytics.netlify.com/data/LTI Analysis Data.csv")


## ----read_data30, echo=FALSE, warning=FALSE, message=FALSE--------------------------------------------------------------------------
LTI_Analysis <- read_delim(na = "NA", delim = ";", col_types = cols(col = col_character()), "data/LTI Analysis Data.csv")

