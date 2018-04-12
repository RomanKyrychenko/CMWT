#!/usr/bin/env Rscript

source("~/CMWT/common.R")
source("topic.R")
suppressPackageStartupMessages(library(gmailr))

"Usage:\n  zaporozhets.R [-s <start> -e <end> -evt <evt>]\n\nOptions:\n  -s Start time [default: Sys.Date()-1]\n  -e End time [default: Sys.Date()-1]\n  -evt Make evt file [default: 1]\n\n]" -> doc

opts <- docopt(doc)

input_dates <- eval(parse(text = opts$s))

if (lubridate::wday(input_dates) == 1) {
  input_dates <- as.Date((input_dates - 2):input_dates, "1970-01-01")
}

cat(paste0("[",Sys.time(),"]"," Start download file list\n"))

dat <- context_data(input_dates:(Sys.Date()))

cat(paste0("[",Sys.time(),"]"," Start target\n"))

masiv <- get_masiv(dat)

programs <- masiv %>% group_by(Дата, Источник, `Час початку програми`) %>% summarise(`Start time` = min(`Start time`), `End time` = max(`End time`))
readr::write_rds(programs, paste0("workfiles/programs/programs_", input_dates[1], ".rds"))

#cat(paste0("[",Sys.time(),"]"," Start topic modeling\n"))

#masiv <- masiv %>% bind_cols(train_lda(masiv$Текст, masiv$Заголовок, nrow(masiv) / 4)[3:4])

if (as.logical(as.numeric(opts$`-evt`))) {
  cat(paste0("[",Sys.time(),"]"," Writing evt\n"))
  evt(masiv, input_dates[length(input_dates)])
}

cat("Press return, if IA is ready\n")
dali <- readLines("stdin", n = 1)


pt <- fs::dir_info("~/Downloads/") %>% arrange(desc(modification_time)) %>% filter(str_detect(path, "Individual Analysis") & !str_detect(path, "\\$")) %>% slice(1) %>% pull(path)

masiv$Охоплення <- readxl::read_excel(pt, sheet = "Reach & Frequency")[-c(1:3), -1] %>% mutate(
  Rch = Rch * 100
) %>% pull(Rch)

cat(paste0("[",Sys.time(),"]", " Writing xlsx\n"))

masiv$Текст <- substr(masiv$Текст, 1, 32000)

fileXls <- paste0("~/CMWT/workfile/tv_daily/tv_", input_dates[length(input_dates)], ".xlsx")

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "tv")

openxlsx::writeDataTable(wb, "tv", masiv, withFilter = F)
openxlsx::setColWidths(wb, "tv", c(1:139), widths = 8.43, hidden = c(rep(F, 6), rep(T, 129), F, F, F, F), ignoreMergedCells = FALSE)
openxlsx::saveWorkbook(wb, file = fileXls, overwrite = T)

gmail_auth(scope = "full", secret_file = "~/context/client_secret_780645875644-m1kk5tro7vs3mhuum8m4ulcfo1vfvl5d.apps.googleusercontent.com (1).json")

ml <- c("victoriya.poda@corestone.expert", "dovhoshyia.t@gmail.com", "dmytro.tkalich@corestone.expert", "bogdan.khytryk@corestone.expert", "yaroslawbozhkoo@gmail.com",    "bogdanfeschenko@gmail.com",    "baskakov.j@gmail.com",    "zybbygame@gmail.com")

test_email <- map(ml, function(x) mime(
  To = x,
  # To = "iryna.zaporozhets@corestone.expert",
  From = "kirichenko17roman@gmail.com",
  Subject = paste("Context & Markdata", input_dates[length(input_dates)]),
  body = paste("Дані за", input_dates[length(input_dates)])
) %>%
  attach_file(fileXls) %>%
  attach_file(fileXls))

a <- capture.output({  
  map(test_email, send_message)
})

cat(paste0("[",Sys.time(),"]", " Ready!\n"))