#!/usr/bin/env Rscript

source("~/CMWT/common.R")
source("topic.R")
#suppressPackageStartupMessages(library(gmailr))

input_dates <- Sys.Date()-1

if(lubridate::wday(input_dates)==1) {
  input_dates <- as.Date((input_dates-2):input_dates, "1970-01-01")
}

#Sys.sleep(3600*2)

cat(paste("Start download file list:", Sys.time()))

dat <- context_data(input_dates)

cat(paste("Start target:", Sys.time()))

masiv <- get_masiv(dat)

cat(paste("Start topic modeling:", Sys.time()))

osr <- invisible(capture.output({
  masiv <- masiv %>% bind_cols(train_lda(masiv$Текст,masiv$Заголовок,nrow(masiv)/4)[3:4])
}))

print(paste("Writing xlsx",Sys.time()))

masiv$Текст <- substr(masiv$Текст, 1, 32000)

#writexl::write_xlsx(masiv,paste0("~/context/tv_",input_dates,".xlsx"))
fileXls <- paste0(getwd(), "/workfiles/tv_daily/tv_", input_dates[length(input_dates)], ".xlsx")

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "tv")

openxlsx::writeDataTable(wb,"tv",masiv,withFilter = F)
openxlsx::setColWidths(wb, "tv", c(1:137), widths = 8.43, ignoreMergedCells = FALSE)
openxlsx::saveWorkbook(wb,file = fileXls,overwrite = T)
#readr::write_excel_csv(masiv,"~/context/tv_08_10-12-2017.csv")

#use_secret_file("~/context/client_secret_780645875644-m1kk5tro7vs3mhuum8m4ulcfo1vfvl5d.apps.googleusercontent.com (1).json")
#gmail_auth(scope = 'full', secret_file = "~/context/client_secret_780645875644-m1kk5tro7vs3mhuum8m4ulcfo1vfvl5d.apps.googleusercontent.com (1).json")
#
#test_email <- mime(
#  To = "victoriya.poda@corestone.expert",
#  From = "kirichenko17roman@gmail.com",
#  Subject = paste("Context",input_dates[length(input_dates)]),
#  body = paste("Context",input_dates[length(input_dates)])) %>% 
#  attach_file(paste0("~/context/workfile/tv_daily/tv_",input_dates[length(input_dates)],".xlsx")) %>% 
#  attach_file(paste0("~/context/workfile/tv_daily/tv_",input_dates[length(input_dates)],".xlsx"))
#send_message(test_email)

suppressPackageStartupMessages(library(mailR))

send.mail(from = "Roman Kyrychenko<roman.kyrychenko@corestone.expert>",
         to = c("kirichenko17roman@gmail.com", "victoriya.poda@corestone.expert"),
         #replyTo = c("Reply to someone else <someone.else@gmail.com>"),
         html = F,encoding = "utf-8", #inline = T,
         subject = paste("Context",input_dates[length(input_dates)]),
         body = paste("Context",input_dates[length(input_dates)]),
         attach.files = c(fileXls),
         smtp = list(host.name = "smtp.openxchange.eu", port = 587, user.name = "roman.kyrychenko@corestone.expert", passwd = "21](,r:==P"),
         authenticate = TRUE,
         send = TRUE)

