#!/usr/bin/env Rscript

source("~/CMWT/common.R")

ptu <- fs::dir_info("~/Downloads/") %>% arrange(desc(modification_time)) %>% filter(str_detect(path, "APU") & !str_detect(path, "\\$") & str_detect(path, "\\.xls")) %>% slice(1) %>% pull(path)

pp <- pp2 <- readxl::read_excel(ptu, sheet = "Данные") %>% filter(!is.na(Дата))

input_dates <- Sys.Date() - 1

if (lubridate::wday(input_dates) == 1) {
  input_dates <- as.Date((input_dates - 2):input_dates, "1970-01-01")
}

dat <- pp %>% left_join(kods, by = c("Источник" = "news_source"))
if (!is.POSIXct(dat$`Start time`)) {
  dat$`Start time` <- if_else(str_detect(dat$`Start time`,":"),
                              as.POSIXct(lubridate::hms(dat$`Start time`), origin = paste(dat$Дата, "00:00:00"), tz = "Europe/Kiev"), 
                              as.POSIXct(as.numeric(dat$`Start time`) * 24* 3600, origin = paste(dat$Дата, "00:00:00"), tz = "Europe/Kiev")) - 3600*3
  dat$`End time` <- if_else(str_detect(dat$`End time`,":"),
                            as.POSIXct(lubridate::hms(dat$`End time`), origin = paste(dat$Дата, "00:00:00"), tz = "Europe/Kiev"), 
                            as.POSIXct(as.numeric(dat$`End time`) * 24* 3600, origin = paste(dat$Дата, "00:00:00"), tz = "Europe/Kiev")) - 3600*3
}

programs <- readr::read_rds(paste0("workfiles/programs/programs_", input_dates[1], ".rds")) %>% left_join(kods, by = c("Источник" = "news_source"))

dat$`Час початку програми` <- as.POSIXct(sapply(1:nrow(dat), function(y) programs$`Час початку програми`[sapply(1:nrow(programs), function(x) between(dat$`Start time`[y], programs$`Start time`[x], programs$`End time`[x]) & dat$ID[y] == programs$ID[x])][1]), tz = "Europe/Kiev", origin = "1970-01-01")

dat %<>% select(Дата, Источник, `Час початку програми`, `Start time`, `End time`, ID)

dat$Дата <- as.Date(dat$Дата)
programs %<>% semi_join(dat, by = c("Дата", "ID", "Час початку програми"))
dat %<>% bind_rows(programs)

cat(paste0("[",Sys.time(),"]"," Writing evt\n"))
evt(dat, "pp")

cat("Press return, if IA is ready\n")
dali <- readLines("stdin", n = 1)

pt <- fs::dir_info("~/Downloads/") %>% arrange(desc(modification_time)) %>% filter(str_detect(path, "Individual Analysis") & !str_detect(path, "\\$")) %>% slice(1) %>% pull(path)

ia <- readxl::read_excel(pt, sheet = "Reach & Frequency")[-c(1:3), -1] %>% mutate(
  Rch = Rch * 100
)

pp$program_datetime <- dat$`Час початку програми`[1:nrow(pp2)]

pp2$Охват <- pp$Охват <- ia$Rch[1:nrow(pp)]

ia <- ia[-c(1:nrow(pp)), ]

ia %<>% mutate(
  `Канал для свода` = case_when(
    Channel == "1+1" ~ "1+1",
    Channel == "INTER" ~ "Інтер",
    Channel == "ICTV" ~ "ICTV",
    Channel == "CHANNEL UKRAINE" ~ "Україна",
    Channel == "STB" ~ "СТБ",
    Channel == "UA:PERSHIY" ~ "Перший"
  )
) %>% mutate(
  program_datetime = dat$`Час початку програми`[(nrow(pp)+1):nrow(dat)]
) %>% rename(`Охоплення випуску в цілому (контактів)` = Rch)

#pp$program_datetime <- ymd_hm(pp$program_datetime, tz = "Europe/Kiev")
if (!is.character(pp$`Start time`)) {
  pp %<>% mutate(
    `Start time` = strftime(`Start time`, format = "%H:%M:%S"),
    `End time` = strftime(`End time`, format = "%H:%M:%S")
  )
}

pp %<>% left_join(ia, by = c("Канал для свода", "program_datetime"))

pp$`Канал для свода` <- factor(pp$`Канал для свода`, levels = c("1+1", "Інтер", "Україна", "СТБ", "ICTV", "Перший"))

pp %<>% arrange(Тема, `Канал для свода`, program_datetime) %>%
  group_by(Тема) %>%
  mutate(`Час виходу` = paste(`Канал для свода`, strftime(program_datetime, format = "%H:%M")))

tms <- pp %>% group_by(Тема) %>% summarise(`Сумарно по темі` = sum(Охват))

pp %<>% left_join(tms, by = "Тема")

chan <- pp %>% group_by(Тема, `Канал для свода`) %>% summarise(Охоплення = sum(Охват)) %>% tidyr::spread(key = "Канал для свода", "Охоплення")

chan <- data_frame(
  Тема = chan$Тема,
  `1+1` = if (!is.null(chan$`1+1`)) chan$`1+1` else NA,
  Інтер = if (!is.null(chan$Інтер)) chan$Інтер else NA,
  Україна = if (!is.null(chan$Україна)) chan$Україна else NA,
  СТБ = if (!is.null(chan$СТБ)) chan$СТБ else NA,
  ICTV = if (!is.null(chan$ICTV)) chan$ICTV else NA,
  Перший = if (!is.null(chan$Перший)) chan$Перший else NA
)

pp %<>% left_join(chan, by = "Тема") %>% arrange(desc(`Сумарно по темі`), `Канал для свода`, `Час виходу`)
neg <- which(pp$Тональность == 1) + 2
pp %<>% select(Тема, `Сумарно по темі`, `1+1`, Інтер, Україна, СТБ, ICTV, Перший, `Час виходу`, `Охоплення випуску в цілому (контактів)`) %>% ungroup()

rows <- (pp %>% left_join(pp %>% group_by(Тема) %>% dplyr::count(), by = "Тема")) %>% filter(!duplicated(Тема)) %>% pull(n)

pp %<>% mutate(
  Тема = ifelse(!duplicated(Тема), Тема, NA),
  `Сумарно по темі` = ifelse(!duplicated(`Сумарно по темі`), `Сумарно по темі`, NA),
  `1+1` = ifelse(!duplicated(`1+1`), `1+1`, NA),
  ICTV = ifelse(!duplicated(ICTV), ICTV, NA),
  Інтер = ifelse(!duplicated(Інтер), Інтер, NA),
  Україна = ifelse(!duplicated(Україна), Україна, NA),
  СТБ = ifelse(!duplicated(СТБ), СТБ, NA),
  Перший = ifelse(!duplicated(Перший), Перший, NA)
)

int <- data_frame(from = cumsum(rows) - rows + 3, to = cumsum(rows) + 2)

leg <- data_frame(`Теми сюжетів` = NA, `Кількість контактів з сюжетами` = NA)[-1, ]

fileXls <- paste0("~/CMWT/workfiles/pp_tv/draft/final_", Sys.Date(), ".xlsx")

style <- openxlsx::createStyle(
  fontSize = 12, fontName = "PT Sans", valign = "center",
  textDecoration = "bold", halign = "center", borderColour = "white",
  fgFill = "#C8D9EF", border = "TopBottomLeftRight", wrapText = T, fontColour = "black"
)

style2 <- openxlsx::createStyle(
  fontSize = 12, fontName = "PT Sans", valign = "center",
  textDecoration = "bold", halign = "center", borderColour = "white",
  fgFill = "#D9D9D9", border = "TopBottomLeftRight", wrapText = T, fontColour = "black"
)

stl <- openxlsx::createStyle(fontSize = 11, fontName = "PT Sans", wrapText = T)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Таблица")
openxlsx::addWorksheet(wb, "Данні")

openxlsx::addStyle(wb, sheet = "Таблица", style, cols = c(1:8), rows = rep(1, 8))
openxlsx::addStyle(wb, sheet = "Таблица", style, cols = c(1:8), rows = rep(2, 8))

openxlsx::addStyle(wb, sheet = "Таблица", style2, cols = c(9:10), rows = rep(1, 2))
openxlsx::addStyle(wb, sheet = "Таблица", style2, cols = c(9:10), rows = rep(2, 2))

for (j in 1:50) {
  for (i in 3:100) {
    openxlsx::addStyle(
      wb, sheet = "Таблица", stl,
      cols = j, rows = i
    )
  }
}

openxlsx::addStyle(
  wb, sheet = "Таблица", openxlsx::createStyle(fontSize = 11, fontName = "PT Sans", wrapText = F, valign = "center", halign = "left"),
  cols = rep(1, 60), rows = 3:62
)
openxlsx::addStyle(
  wb, sheet = "Таблица", openxlsx::createStyle(fontSize = 11, fontName = "PT Sans", wrapText = T, valign = "center", halign = "center", textDecoration = "bold", numFmt = "### ### ###"),
  cols = rep(2, 60), rows = 3:62
)
for (j in c(3:8, 10)) {
  for (i in 3:100) {
    openxlsx::addStyle(
      wb, sheet = "Таблица", openxlsx::createStyle(fontSize = 11, fontName = "PT Sans", wrapText = T, valign = "center", halign = "center", numFmt = "### ### ###"),
      cols = j, rows = i
    )
  }
}

openxlsx::writeDataTable(wb, "Таблица", pp, withFilter = F, startCol = 1, startRow = 2)
openxlsx::writeDataTable(wb, "Данні", pp2, withFilter = F, startCol = 1, startRow = 1)
for (j in 1:8) {
  for (i in 1:nrow(int)) {
    openxlsx::mergeCells(wb, "Таблица", rows = int$from[i]:int$to[i], cols = j)
  }
}

if (!is_empty(neg)) {
  for (i in neg) {
    openxlsx::addStyle(wb, "Таблица", openxlsx::createStyle(fontSize = 11, fontName = "PT Sans", wrapText = T, textDecoration = "bold", valign = "center", halign = "center", numFmt = "### ### ###", fontColour = "red"), rows = i, cols = 2)
  }
  for (j in 3:8) {
    for (i in neg) {
      openxlsx::addStyle(wb, "Таблица", openxlsx::createStyle(fontSize = 11, fontName = "PT Sans", wrapText = T, valign = "center", halign = "center", numFmt = "### ### ###", fontColour = "red"), rows = i, cols = j)
    }
  }
}

openxlsx::mergeCells(wb, "Таблица", rows = 1:2, cols = 1)
openxlsx::mergeCells(wb, "Таблица", rows = 1, cols = 2:8)
openxlsx::mergeCells(wb, "Таблица", rows = 1, cols = 9:10)
openxlsx::conditionalFormatting(wb, "Таблица", cols = 2, rows = 3:(nrow(pp) + 2), type = "databar", style = "#DBDBDB", gradient = FALSE)
openxlsx::setColWidths(wb, "Таблица", c(2:8), widths = 10, ignoreMergedCells = FALSE)
openxlsx::setColWidths(wb, "Таблица", c(1), widths = 58.43, ignoreMergedCells = FALSE)
openxlsx::setColWidths(wb, "Таблица", c(9), widths = 14.43, ignoreMergedCells = FALSE)
openxlsx::setColWidths(wb, "Таблица", c(10), widths = 20.43, ignoreMergedCells = FALSE)
openxlsx::saveWorkbook(wb, file = fileXls, overwrite = T)

openxlsx::openXL(fileXls)
