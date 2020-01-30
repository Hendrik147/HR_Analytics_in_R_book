library(googledrive)

drive_upload(media = "docs/hendrikfeddersen.pdf",
             path = "HR_Analytics_live_book_in_pdf/",
             name = paste0("HR_Analytics_live_book_", Sys.Date(), ".pdf"),
             type = NULL,
             verbose = TRUE)

