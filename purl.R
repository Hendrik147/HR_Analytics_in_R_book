if(!dir.exists("docs/scripts")){
  dir.create("docs")
  dir.create("docs/scripts")
}

# For Chapter 5
solutions_shown <- c('')
show_solutions <- function(section){
  return(solutions_shown == "ALL" | section %in% solutions_shown)
}

# Note order and title matters here:
chapter_titles <- c("getting-started",
                    "visualization",
                    "wrangling",
                    "tidy",
                    "regression",
                    "multiple-regression",
                    "sampling",
                    "confidence-intervals",
                    "hypothesis-testing",
                    "inference-for-regression",
                    "tell-your-story-with-data",
                    "pay-gap",
                    "stop-appraisals",
                    "service-desk",
                    "personality",
                    "commuting-time",
                    "organisational-network",
                    "job-classification",
                    "masking-data",
                    "absenteeism-MFG",
                    "absenteeism-work",
                    "accidents-work",
                    "attrition",
                    "interview-attendance",
                    "ranking-medical-schools",
                    "webscraping-linkedin",
                    "flexdashboards",
                    "data-science-product")
chapter_numbers <- stringr::str_pad(
  string = 1:(length(chapter_titles) + 1),
  width = 2,
  side = "left",
  pad = "0"
)
for(i in seq_len(length(chapter_numbers))){
  Rmd_file <- stringr::str_c(chapter_numbers[i], "-",
                             chapter_titles[i], ".Rmd")
  R_file <- stringr::str_c("docs/scripts/", chapter_numbers[i],
                           "-", chapter_titles[i], ".R")
  knitr::purl(Rmd_file, R_file)
}

