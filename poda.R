#!/usr/bin/env Rscript

source("~/CMWT/common.R")
source("topic.R")

input_dates <- Sys.Date()-1

if(lubridate::wday(input_dates)==1) {
  input_dates <- as.Date((input_dates-2):input_dates, "1970-01-01")
}

cat(paste("Start download file list:", Sys.time()))

dat <- context_data(input_dates)

cat(paste("Start target:", Sys.time()))

masiv <- get_masiv(dat)

cat(paste("Start topic modeling:", Sys.time()))

osr <- invisible(capture.output({
  masiv <- masiv %>% bind_cols(train_lda(masiv$Текст, masiv$Заголовок, nrow(masiv) / 4)[3:4])
}))

print(paste("Writing xlsx",Sys.time()))

masiv$Текст <- substr(masiv$Текст, 1, 32000)

fileXls <- paste0(getwd(), "/workfiles/tv_daily/tv_", input_dates[length(input_dates)], ".xlsx")

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "tv")

openxlsx::writeDataTable(wb,"tv",masiv,withFilter = F)
openxlsx::setColWidths(wb, "tv", c(1:137), widths = 8.43, ignoreMergedCells = FALSE)
openxlsx::saveWorkbook(wb,file = fileXls,overwrite = T)

suppressPackageStartupMessages(library(mailR))

send.mail(from = "Roman Kyrychenko<roman.kyrychenko@corestone.expert>",
         to = c("kirichenko17roman@gmail.com", "victoriya.poda@corestone.expert"),
         html = F,encoding = "utf-8", 
         subject = paste("Context",input_dates[length(input_dates)]),
         body = paste("Context",input_dates[length(input_dates)]),
         attach.files = c(fileXls),
         smtp = list(host.name = hostname, port = port, user.name = username, passwd = mailpass),
         authenticate = TRUE,
         send = TRUE)

q(save = "no")