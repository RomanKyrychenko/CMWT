#!/usr/bin/env Rscript

source("~/context/common.R")

"Usage:\nevt.R [-f <file> -s <sheet>]\n\nOptions:\n  -f Start time\n  -s End time [default: 1]\n\n]" -> doc

opts <- docopt(doc)

input_dates <- Sys.Date() - 1

if (lubridate::wday(input_dates) == 1) {
  input_dates <- as.Date((input_dates - 2):input_dates, "1970-01-01")
}

# dat <- readxl::read_excel("~/Downloads/APU_TV.xlsx", sheet = 1)

dat <- readxl::read_excel(opts$f, sheet = eval(parse(text = opts$s)))
dat <- dat %>% left_join(kods, by = c("Источник" = "news_source"))
if (!is.POSIXct(dat$`Start time`)) {
  dat$`Start time` <- as.POSIXct(lubridate::hms(dat$`Start time`), origin = "1969-12-31 21:00:00", tz = "Europe/Kiev")
  dat$`End time` <- as.POSIXct(lubridate::hms(dat$`End time`), origin = "1969-12-31 21:00:00", tz = "Europe/Kiev")
}

if ("Канал для свода" %in% names(dat)) {
  programs <- readr::read_rds(paste0("~/CMWT/workfiles/programs/programs_", input_dates[1], ".rds")) %>% left_join(kods, by = c("Источник" = "news_source"))
  dat %<>% mutate(`Час початку програми` = ifelse(grepl("*.mp4$", Текст), substr(Текст, nchar(Текст) - 20, nchar(Текст) - 8),
                                                  substr(Текст, nchar(Текст) - 17, nchar(Текст) - 5)
  ))
  dat$`Час початку програми` <- ymd_hm(dat$`Час початку програми`, tz = "Europe/Kiev")
  dat %<>% select(Дата, Источник, `Час початку програми`, `Start time`, `End time`, ID)
  dat$Дата <- as.Date(dat$Дата)
  programs %<>% semi_join(dat, by = c("Дата", "ID", "Час початку програми"))
  dat %<>% bind_rows(programs)
}

evt(dat, basename(opts$f))