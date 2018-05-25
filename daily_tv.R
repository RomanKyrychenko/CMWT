#!/usr/bin/env Rscript

source("~/CMWT/common.R")
source("topic.R")
suppressPackageStartupMessages(require(mailR))

input_dates <- Sys.Date() - 1

if (lubridate::wday(input_dates) == 1) {
  input_dates <- as.Date((input_dates - 2):input_dates, "1970-01-01")
}

cat(paste0("[", Sys.time(), "]", " Start download file list\n"))

dat <- context_data(input_dates)

cat(paste0("[", Sys.time(), "]", " Start target\n"))

masiv <- get_masiv(dat)

programs <- masiv %>% group_by(Дата, Источник, `Час початку програми`) %>% summarise(`Start time` = min(`Start time`), `End time` = max(`End time`))

readr::write_rds(programs, paste0("workfiles/programs/programs_", input_dates[1], ".rds"))

cat(paste0("[", Sys.time(), "]", " Writing evt\n"))

nss <- dat %>% group_by(
  news_source, program_datetime
) %>% summarise(
  ns = max(number)
) %>% filter(!is.na(ns))

su <- dat %>% group_by(
  news_source, program_datetime
) %>% do(
  d = sort(.$number)
) %>% filter(length(d)!=0)

nss$prop <- map(map2(map(su$d, function(x) 1:max(x)), su$d, function(x, y) x %in% y), function(x) which(x == F))

nss <- nss[!map_lgl(nss$prop, is_empty), ]

txt <- paste(paste0("На каналі ",nss$news_source," у випуску о ",nss$program_datetime," пропущено сюжет номер ",nss$prop), collapse = "\n")

if(nrow(nss) == 0) txt = "" 
  
evt(masiv, input_dates[length(input_dates)])

cat("Press return, if IA is ready\n")
dali <- readLines("stdin", n = 1)

pt <- fs::dir_info("~/Downloads/") %>% arrange(desc(modification_time)) %>% filter(str_detect(path, "Individual Analysis") & !str_detect(path, "\\$")) %>% slice(1) %>% pull(path)

masiv$Охоплення <- readxl::read_excel(pt, sheet = "Reach & Frequency")[-c(1:3), -1] %>% mutate(
  Rch = Rch * 100
) %>% pull(Rch)

cat(paste0("[", Sys.time(), "]", " Writing xlsx\n"))

masiv$Текст <- substr(masiv$Текст, 1, 32000)

fileXls <- paste0("~/CMWT/workfiles/tv_daily/tv_", input_dates[length(input_dates)], ".xlsx")

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "tv")

openxlsx::writeDataTable(wb, "tv", masiv, withFilter = F)
openxlsx::setColWidths(wb, "tv", c(1:139), widths = 8.43, hidden = c(rep(F, 6), rep(T, 129), F, F, F, F), ignoreMergedCells = FALSE)
openxlsx::saveWorkbook(wb, file = fileXls, overwrite = T)

ml <- c("victoriya.poda@corestone.expert", "dovhoshyia.t@gmail.com", "dmytro.tkalich@corestone.expert", "bogdan.khytryk@corestone.expert", "yaroslawbozhkoo@gmail.com", "bogdanfeschenko@gmail.com", "baskakov.j@gmail.com", "zybbygame@gmail.com")

send.mail(
  from = "Roman Kyrychenko<roman.kyrychenko@corestone.expert>",
  to = c("kirichenko17roman@gmail.com", ml),
  html = F, encoding = "utf-8",
  subject = paste("Context", input_dates[length(input_dates)]),
  body = paste("Дані за", input_dates[length(input_dates)], "\n", txt),
  attach.files = c(fileXls),
  smtp = list(host.name = "smtp.openxchange.eu", port = 587, user.name = "roman.kyrychenko@corestone.expert", passwd = "21](,r:==P"),
  authenticate = TRUE,
  send = TRUE
)

cat(paste0("[", Sys.time(), "]", " Ready!\n"))