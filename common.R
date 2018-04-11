invisible(Sys.setlocale(locale = "UK_ua"))

setwd("~/CMWT")

options(warn = -1)

dyn.load("/Applications/IBM/SPSS/Statistics/23/jre/lib/server/libjvm.dylib")

suppressPackageStartupMessages({
  require(RCurl)
  require(dplyr)
  require(tidyverse)
  require(xml2)
  require(XML)
  require(XLConnect)
  require(docopt)
  require(lubridate)
  require(magrittr)
  require(cli)
  require(clisymbols)
})

url <- "ftp:/91.218.214.110"
userpwd <- "corestone:GTYJgkfcn2190"

cleanFun <- function(htmlString) str_remove_all(htmlString, "<.*?>")

options("openxlsx.dateFormat" = "dd.mm.yyyy")
options("openxlsx.datetimeFormat" = "hh:mm:ss")

kods <- readr::read_rds("~/CMWT/kods.rds")

info <- function(filenames) {
  files <- filenames %>% str_remove_all("\\r") %>% str_replace_all("\\n", " ") %>% strsplit(" ", fixed = TRUE) %>% unlist()
  data_frame(
    files = files,
    date = as.Date.POSIXct(parse_number(files), origin = "1970-01-01")
  )
}

safe_ftp <- function(x) {
  s <- "try-error"
  while (s == "try-error") {
    res <- try(getURL(paste(url, x, sep = "/"), userpwd = userpwd, .encoding = "UTF-8"))
    s <- class(res)
  }
  res
}

context_data <- function(input_dates) {
  dat <- info(getURL(url, userpwd = userpwd, ftp.use.epsv = FALSE, dirlistonly = TRUE)) %>%
    filter(date %in% input_dates:(Sys.Date())) %>%
    pull(files) %>%
    purrr::map_dfr(function(x) {
      safe_ftp(x) %>%
        purrr::compact() %>%
        xml2::as_xml_document() %>%
        as_list() %>%
        unlist(recursive = F) %>%
        map(flatten_df) %>%
        bind_rows()
    }) %>%
    left_join(kods, by = "news_source") %>%
    filter(!is.na(ID)) %>%
    arrange(program_datetime, news_source) %>%
    mutate(
      news_datetime = as.POSIXct(as.numeric(news_datetime), origin = "1970-01-01", tz = "Europe/Kiev"),
      program_datetime = as.POSIXct(as.numeric(program_datetime), origin = "1970-01-01", tz = "Europe/Kiev"),
      news_text = cleanFun(news_text),
      number = as.numeric(substr(url, nchar(url) - 5, nchar(url) - 4)),
      chek = news_datetime - program_datetime,
      text = news_text,
      news_text = tolower(news_text)
    ) %>%
    distinct() %>%
    filter(mass_media_type == 1 & (as.Date(news_datetime) %in% input_dates)) %>%
    group_by(news_source, program_datetime) %>%
    mutate(
      number = number,
      cum_dur_s = as.POSIXct(ifelse(min(news_datetime) > program_datetime, min(news_datetime), program_datetime), origin = "1970-01-01", tz = "Europe/Kiev") + c(0, cumsum(duration)[-n()]),
      cum_dur = min(news_datetime) + cumsum(duration),
      s = as.numeric(median(difftime(news_datetime, cum_dur_s, units = "secs"))),
      v = as.numeric(difftime(news_datetime, cum_dur_s, units = "secs")),
      tru_time = as.POSIXct(ifelse(s * 1.25 < v | s * 0.75 > v, cum_dur_s, news_datetime), origin = "1970-01-01", tz = "Europe/Kiev")
    ) %>%
    ungroup()
}

get_masiv <- function(dat) {
  data_frame(
    Дата = as.Date(dat$tru_time),
    Источник = dat$news_source,
    `Start time` = dat$tru_time,
    `End time` = dat$tru_time + as.numeric(dat$duration),
    Заголовок = dat$caption,
    Текст = dat$text,
    Президент = str_detect(dat$news_text, "порошенк|президент(а|у|ом|ові|е) укра|президент укра|підпи(ше|с) президент"),
    Гройсман = str_detect(dat$news_text, "гройсман| прем'єр-міністр(|а|ом|у) укра| премьер-министр(|а|у|ом) укра| прем'єр(|a|ом|ський|у)[^-] укра| премьер(|a|ом|ский|у)[^-] укра"), # & !str_detect(dat$news_text,"прем'єр-міністр(|а|ом|у) (канад|із|поль|рос|пів|тере|вел|мед|бри|іта|ісп|ката|авс|гру|л|країн|коро)| премьер-министр(|а|у|ом) (канад|из|поль|рос|тере|исп|кат|ит|авс|гру|мед)| прем'єр(|а|и|ою|ами) сер| премьер (тере|стран)"),
    Грановский = str_detect(dat$news_text, "грановс(|ь)к(ий|ого|ому|им)"),
    Данилюк = str_detect(dat$news_text, "данилюк"),
    Саенко = str_detect(dat$news_text, "са(е|є)нк(о|а)"),
    А.Филатов = str_detect(dat$news_text, "(о|а)лекс(і|е)й ф(і|и)латов"),
    Гриценко = str_detect(dat$news_text, "гриценк(о|а|у)"),
    Коболев = str_detect(dat$news_text, "кобол(е|є)в"),
    Рева = str_detect(dat$news_text, "андр(е|і)(й|я|ю) рев(а|і|у|и)"),
    Садовой = str_detect(dat$news_text, "андр(і|е)(й|я|ю|єм|ем) садов(и|о)(й|м|му|го)"),
    Аваков = str_detect(dat$news_text, "аваков"),
    Ложкин = str_detect(dat$news_text, "ложк(и|і)н"),
    Новинский = str_detect(dat$news_text, "новинс(|ь)к(ий|ого|им|ому)"),
    Черныш = str_detect(dat$news_text, "вадим(а|ом|у) черн(ы|и)ш(а|у|ом)"),
    Наливайченко = str_detect(dat$news_text, "наливайченк(о|а|у|ом)"),
    Гонтарева = str_detect(dat$news_text, "гонтар(е|є)в(а|ої|ій|у)"),
    Муженко = str_detect(dat$news_text, "муженк(о|а|у|ом)"),
    Пинчук = str_detect(dat$news_text, "п(і|и)нчук"),
    Ковальчук = str_detect(dat$news_text, "в(и|і)тал(и|і)(й|ю|єм|я) ковальчук(|а|у|ом)"),
    Кистион = str_detect(dat$news_text, "к(і|и)ст(і|и)он"),
    Омелян = str_detect(dat$news_text, "омелян"),
    Кириленко = str_detect(dat$news_text, "міністр(|а|ом|у) кириленк(о|а|у|ом) в('|ь)ячеслав(|а|о|му) кириленк(о|а|у|ом)"),
    Зубко = str_detect(dat$news_text, "зубк(о|а|у|ом), zubko"),
    Добродомов = str_detect(dat$news_text, "добродомов"),
    Левченко = str_detect(dat$news_text, "дмитр(ий|о|ом|у|а|ием) левченк(о|а|у|ом|ові)"),
    Фирташ = str_detect(dat$news_text, "ф(и|і)рташ,firtash"),
    Шимкив = str_detect(dat$news_text, "шимк(і|и)в"),
    Нищук = str_detect(dat$news_text, "нищук"),
    Климпуш = str_detect(dat$news_text, "цинцадзе"),
    Насалик = str_detect(dat$news_text, "насал(и|і)к"),
    Райнин = str_detect(dat$news_text, "райн(і|и)н"),
    Геращенко = str_detect(dat$news_text, "(и|і)рин(а|і|у) геращенко"),
    Елисеев = str_detect(dat$news_text, "ко(|н)ст(я|а)нтин(|а|ом|у|ові) (є|е)л(и|і)с(єє|ее)в(|у|а|им|ым)"),
    Гриневич = str_detect(dat$news_text, "гриневич"),
    М.Тымченко = str_detect(dat$news_text, "максим(|а|ом|у) т(і|и)мченк(о|у|ом|ові)"),
    М.Порошенко = str_detect(dat$news_text, "марин(а|и|і|е|ою|ой) порошенко|пер(ш|в)(а|ая|ої|у|ій|ой) лед(і|и)|дружин(а|і|ою) порошенка|дружин(а|і|ою) президента україни|жен(а|е|ой) порошенк(а|о)"),
    Саакашвили = str_detect(dat$news_text, "саакашв(и|і)л(|л)(и|і)| м(і|и)хо "),
    Савченко = str_detect(dat$news_text, "над(ежда|ія|я|ії|ежды|ією|еждою) савченко"),
    Семерак = str_detect(dat$news_text, "семерак"),
    Климкин = str_detect(dat$news_text, "кл(і|и)мк(і|и)н"),
    Петренко = str_detect(dat$news_text, "пав(|е)л(|о|а|ом|у) петренк(о|у|а|ом)"),
    Розенко = str_detect(dat$news_text, "розенк(о|а|у)"),
    Насиров = str_detect(dat$news_text, "нас(і|и)ров"),
    Лещенко = str_detect(dat$news_text, "серг(е|і)(й|я|ю|єм) лещенк(a|о|у|ом|ові)"),
    Новак = str_detect(dat$news_text, "славом(і|и)р(|у|а|ом) новак(|а|у|ом)"),
    Кубив = str_detect(dat$news_text, "степан(|а|ом|у) куб(і|и)в"),
    Полторак = str_detect(dat$news_text, "полторак"),
    Кутовой = str_detect(dat$news_text, "тарас(|у|а|ом) кутов(о|и)(й|го|му|м)"),
    Хромаев = str_detect(dat$news_text, "тимур(|а|ом|у) хрома(е|є)в(|а|у|им)"),
    Супрун = str_detect(dat$news_text, "супрун"),
    Тимошенко = str_detect(dat$news_text, "тимошенко"),
    Луценко = str_detect(dat$news_text, "луценк| генпрокурор| генеральн(ий|ого|им|ому) прокурор(|у|а|ом)"),
    Стець = str_detect(dat$news_text, "стец(ь|я|ем|ю) "),
    Терентьев = str_detect(dat$news_text, "юр(і|и)(й|я|ю|єм) теренть(є|е)в(|а|им|у)"),
    Матиос = str_detect(dat$news_text, "мат(і|и)ос"),
    Артеменко = str_detect(dat$news_text, "артеменк(о|а|у|ом)"),
    Белецкий = str_detect(dat$news_text, "б(і|и|е)лец(|ь)к(ий|ого|им|ому)"),
    Деркач = str_detect(dat$news_text, "андр(і|е)(й|я|ю|єм|ем) деркач"),
    Журжий = str_detect(dat$news_text, "журж(і|и)(й|я|єм|ю)"),
    Лозовой = str_detect(dat$news_text, "андр(і|е)(й|я|ю|єм|ем) лозов(ой|ий|им|ого|ому)"),
    Парубий = str_detect(dat$news_text, "паруб(і|и)(й|я|ю|єм|ем)"),
    Тетерук = str_detect(dat$news_text, "андр(і|е)(й|я|ю|єм) тетерук(|а|ом|у)"),
    А.Геращенко = str_detect(dat$news_text, "антон(|а|у|ом) геращенк(о|у|а|ом)"),
    Яценюк = str_detect(dat$news_text, "яценюк"),
    Сытник = str_detect(dat$news_text, "с(и|ы)тник"),
    Герасимов = str_detect(dat$news_text, "артур(|у|ом|а) герасимов(|у|ом|а)"),
    Б.Филатов = str_detect(dat$news_text, "борис(|а|ом|у) ф(и|і)латов(|а|им|у)"),
    Б.Береза = str_detect(dat$news_text, "борислав(|у|ом|а) берез(а|і|ою)"),
    Рабинович = str_detect(dat$news_text, "раб(и|і)нович(|а|ем|у)"),
    Резниченко = str_detect(dat$news_text, "резниченк(о|ом|у|а)"),
    Грицак = str_detect(dat$news_text, "грицак"),
    Балога = str_detect(dat$news_text, "бало(г|з)(і|а|у|и)"),
    Бондарь = str_detect(dat$news_text, "в(и|і)ктор(|у|а|ом) бондар(|ем|у|ю|єм)"),
    Медведчук = str_detect(dat$news_text, "медведчук"),
    Чумак = str_detect(dat$news_text, "в(и|і)ктор(|а|у|ом) чумак(|у|ом|а)"),
    Янукович = str_detect(dat$news_text, "янукович"),
    Войцицька = str_detect(dat$news_text, "войц(і|и)ц(|ь)к(а|ая|ої|ій|ой)"),
    Сюмар = str_detect(dat$news_text, "сюмар"),
    Кличко = str_detect(dat$news_text, "кличк(о|а|ом|у)"),
    Куприй = str_detect(dat$news_text, "купр(и|і)(й|ю|я|єм)"),
    Хомутынник = str_detect(dat$news_text, "хомутинн(і|и)к"),
    Бальчун = str_detect(dat$news_text, "бальчун"),
    Парасюк = str_detect(dat$news_text, "парасюк"),
    Гопко = str_detect(dat$news_text, "гопко"),
    Кернес = str_detect(dat$news_text, "кернес"),
    Москаль = str_detect(dat$news_text, "геннад(и|і)(й|я|ю|єм) москал(ь|ем|ю|я)"),
    Труханов = str_detect(dat$news_text, "геннад(и|і)(й|я|єм|ем|ю) труханов(|им|у|а|ым)"),
    Сакварелидзе = str_detect(dat$news_text, "cакварел(и|і)дзе"),
    Тымчук = str_detect(dat$news_text, "т(и|ы)мчук"),
    Мураев = str_detect(dat$news_text, "мура(е|є)в"),
    Соболев = str_detect(dat$news_text, "(є|е)гор(|а|у) собол(є|е)в(|а|у)"),
    Коломойский = str_detect(dat$news_text, "коломойськ(ий|ого|ому)"),
    Кононенко = str_detect(dat$news_text, "(и|і)гор(|ь|я|ю|ем) кононенк(о|ом|у|а)"),
    ИгорьЛуценко = str_detect(dat$news_text, "(и|і)гор(|ь|я|а|ем|ю) луценк(о|ом|у|а)"),
    Мосийчук = str_detect(dat$news_text, "мос(і|и)йчук") & !str_detect(dat$news_text, "натал(ка|ія|ья) мос(і|и)йчук"),
    ИринаЛуценко = str_detect(dat$news_text, "(и|і)рин(а|і|у|ою) луценко"),
    Кучма = str_detect(dat$news_text, "кучм"),
    Бурбак = str_detect(dat$news_text, "бурбак"),
    Степанов = str_detect(dat$news_text, "максим(|а|у|ом) степанов(|у|а|им)"),
    Княжицкий = str_detect(dat$news_text, "княжиц(|ь)к(ий|ого|ому)"),
    Добкин = str_detect(dat$news_text, "добк(і|и)н"),
    Найем = str_detect(dat$news_text, "най(є|е)м"),
    Холодницкий = str_detect(dat$news_text, "холодниц(|ь)к(ий|ого|ому|им)"),
    Сыроид = str_detect(dat$news_text, "с(и|ы)ро(ї|е)д"),
    Березюк = str_detect(dat$news_text, "олег(|ом|ові|у|а) березюк(|а|у|ом)"),
    Ляшко = str_detect(dat$news_text, "олег(|a|e) ляшк(о|у|а), ^ляшк"),
    Тягнибок = str_detect(dat$news_text, "тягн(ы|и)бок"),
    Брыгынець = str_detect(dat$news_text, "бригин(ец|ця|цем|цю)"),
    Вилкул = str_detect(dat$news_text, "в(і|и)лкул"),
    Домбровский = str_detect(dat$news_text, "домбровськ(ий|ого|им|ому)"),
    Онищенко = str_detect(dat$news_text, "онищенк(о|у|ом|а)"),
    Фельдман = str_detect(dat$news_text, "фельдман"),
    А.Гончаренко = str_detect(dat$news_text, "(а|о)лекс(е|і)(й|єм|ю|я) гончаренк(о|а|у|ом)"),
    Жебривский = str_detect(dat$news_text, "жебр(і|и|о)вс(|ь)к"),
    Ахметов = str_detect(dat$news_text, "ахметов"),
    Залищук = str_detect(dat$news_text, "зал(і|и)щук"),
    Семенченко = str_detect(dat$news_text, "cемен(|ом|у|а) cеменченк(о|а|у|ом)"),
    Березенко = str_detect(dat$news_text, "cерг(е|і)(й|ю|я|єм|ем) березенк(о|ом|у|а|ом)"),
    Власенко = str_detect(dat$news_text, "cерг(е|і)(й|я|ю|єм) власенк(о|ом|у|а)"),
    Каплин = str_detect(dat$news_text, "капл(і|и)н"),
    Князев = str_detect(dat$news_text, "серг(е|і)(й|я|ю|єм) княз(е|є)в"),
    Левочкин = str_detect(dat$news_text, "л(ьо|е|ё)вочк(і|и)н"),
    Рыбалка = str_detect(dat$news_text, "cерг(е|і)(й|ю|єм|ем|я) р(и|ы)бал(ка|кою|ку|ці)"),
    С.Соболев = str_detect(dat$news_text, "cерг(е|і)(й|ю|єм|ем|я) собол(є|е)в(|а|им|у)"),
    Тарута = str_detect(dat$news_text, "тарут"),
    Т.Чорновол = str_detect(dat$news_text, "т(а|е)т(|ь)ян(а|и|у|ою|і) чорновол"),
    Свитлычна = str_detect(dat$news_text, "юл(и|і)(я|ї|ю) св(і|и|е)тличн(а|ая|ої|ою|ій)"),
    Ю.Береза = str_detect(dat$news_text, "юр(и|і)(й|єм|ю|я) берез(а|у|ою|і)"),
    Бойко = str_detect(dat$news_text, "юр(и|і)(й|я|ю|єм|ем) бойк(о|ом|у|а)"),
    Охоплення = NA,
    Програма = dat$title_prog,
    `Ведуч(ий/а)` = dat$leading,
    `Час початку програми` = dat$program_datetime,
    `Номер сюжету` = dat$number,
    ID = dat$ID
  ) %>% mutate_at(.vars = 7:135, function(x) ifelse(as.numeric(x) == 1, 1, NA)) %>% arrange(Дата, Источник, `Start time`)
}

evt <- function(dat, name) {
  dats <- substr(gsub("-", "", as.character(dat$Дата)), 1, 8)

  hours <- substr(as.character(dat$`Start time`), 12, 13)
  hours <- ifelse(as.numeric(hours) < 3, as.character(as.numeric(hours) + 24), hours)
  dats <- ifelse(as.numeric(hours) < 3, substr(gsub("-", "", as.character(dat$`Start time` - 3600 * 24)), 1, 8), dats)
  minuts <- substr(as.character(dat$`Start time`), 15, 16)

  seconds <- substr(as.character(dat$`Start time`), 18, 19)

  tims <- paste0(hours, minuts, seconds)
  ts <- paste(hours, minuts, seconds, sep = ":")
  dif <- as.numeric(difftime(dat$`End time`, dat$`Start time`, units = "secs"))
  sink(paste0("~/context/workfile/evt/markdata_", gsub(".xlsx", "", name), ".evt"))
  cat(paste0("% (evt-2.10|module=indanl_v3.dll|agent=2.00-r2013.37|time=1412241926) 
             { ", nrow(dat), ":\r\n"))
  cat(paste(paste0("(", dats, ": ", dat$ID, ": ", tims, ": ", dif, ' ["', dat$Источник, " ", ts, '", 256, 4] ), ', dif, ";"), collapse = ",\r\n"))
  cat("\n}")
  sink()
}