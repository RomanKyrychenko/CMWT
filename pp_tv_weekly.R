#!/usr/bin/env Rscript

source("~/CMWT/common.R")

w <- list.files("workfiles/pp_tv/final/", pattern = paste0("w", week(Sys.Date()) - 1), full.names = T)

w <- w[!str_detect(w, "\\$")]

files <- purrr::map(w, function(x) readxl::read_excel(x, sheet = "Данные"))
for (i in which(sapply(files, function(x) is.POSIXct(x$`Start time`)))) {
  files[[i]] <- files[[i]] %>% mutate(
    `Start time` = strftime(`Start time` - 7200, format = "%H:%M:%S"),
    `End time` = strftime(`End time` - 7200, format = "%H:%M:%S")
  )
}

files <- bind_rows(files)

files <- files %>% mutate(
  Тема = ifelse(wday(files$Дата) %in% c(1, 6, 7), substr(Тема, 9, 1000), Тема)
)

files <- files %>% mutate(program_datetime = ifelse(grepl("*.mp4$", Текст), substr(Текст, nchar(Текст) - 11, nchar(Текст) - 8),
  substr(Текст, nchar(Текст) - 8, nchar(Текст) - 5)
))
files <- files %>% filter(
  (`Канал для свода` == "ICTV" & program_datetime == "1845") |
    (`Канал для свода` == "1+1" & program_datetime == "1930") |
    (`Канал для свода` == "Інтер" & program_datetime == "2000") |
    (`Канал для свода` == "СТБ" & program_datetime == "2200") |
    (`Канал для свода` == "Україна" & program_datetime == "1900")
)

tems <- files %>%
  group_by(Дата, Тема) %>%
  summarise(text = paste(ifelse(!duplicated(`Канал для свода`), sapply(`Канал для свода`, as.character), NA), collapse = ", ")) %>%
  mutate(text = gsub("NA, ", "", text)) %>%
  mutate(text = gsub(", NA", "", text))

files <- files %>% left_join(tems, by = c("Дата", "Тема"))

files <- files %>% mutate(Тема = paste0(Тема, " (", text, ")"))

files$Дата <- as.Date(files$Дата)

fileXls <- paste0("workfiles/pp_tv/weekly/pp_tv_w", median(week(files$Дата)), ".xlsx")

options("openxlsx.dateFormat" = "dd.mm.yyyy")
options("openxlsx.datetimeFormat" = "hh:mm:ss")

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "tv")
openxlsx::writeDataTable(wb, "tv", files[1:10], withFilter = T)
openxlsx::setColWidths(wb, "tv", c(1:6, 8:10), widths = 10, ignoreMergedCells = FALSE)
openxlsx::setColWidths(wb, "tv", c(7), widths = 120, ignoreMergedCells = FALSE)
openxlsx::saveWorkbook(wb, file = fileXls, overwrite = T)