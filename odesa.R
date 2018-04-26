#!/usr/bin/env Rscript
source("~/CMWT/common.R")
source("~/CMWT/topic.R")

suppressPackageStartupMessages(library(mailR))

'Usage:
odesa.R [-s <start> -e <end> -evt <evt>]

Options:
-s Start time [default: Sys.Date()-6]
-e End time [default: Sys.Date()-1]
-evt Make evt file [default: 1]

]' -> doc

opts <- docopt(doc)

input_dates <- as.Date(eval(parse(text = opts$s)):eval(parse(text = opts$e)), origin = "1970-01-01")

cat(paste0("[",Sys.time(),"]", " Start download file list\n"))

dat <- context_data(input_dates)

cat(paste0("[",Sys.time(),"]", " Start target\n"))

dat$news <- dat$news_text

dat$news_text <- tolower(dat$news_text)

masiv <- data_frame(
  Дата = as.Date(dat$news_datetime),
  Источник = dat$news_source,	
  `Start time`=	dat$news_datetime,
  `End time`= dat$news_datetime+as.numeric(dat$duration),
  Заголовок	= dat$caption,
  Текст = dat$news,
  Труханов = grepl("труханов",dat$news_text),
  Урбанський = grepl("урбанс(|ь)к",dat$news_text),
  Ківалов = grepl("к(і|и)валов",dat$news_text),
  Гончаренко = grepl("гончаренк",dat$news_text),
  Скорик = grepl("скорик",dat$news_text),
  Кіссе = grepl("к(і|и)ссе",dat$news_text),
  Родковський = grepl("родковс(|ь)к",dat$news_text),
  Дімчогло = grepl("д(і|и)мчогло",dat$news_text),
  Пресман = grepl("пресман",dat$news_text),
  Барвіненко = grepl("барв(і|и)ненк",dat$news_text),
  Гриневецький = grepl("гриневец(|ь)к",dat$news_text),
  Матвійчук = grepl("(э|е)дуард(|а|у|ом|ові) матв(і|и)йчук",dat$news_text),
  Москаль = grepl("москал",dat$news_text),
  Жебрівський = grepl("жебр(і|и|о)вс(|ь)к",dat$news_text),
  Синютка = grepl("cинютк",dat$news_text),
  Гундич = grepl("гундич",dat$news_text),
  Світлична = grepl("св(і|и|е)тличн",dat$news_text),
  Степанов = grepl("cтепанов",dat$news_text),
  Резніченко = grepl("резн(і|и)ченк",dat$news_text),
  Бриль = grepl("брил",dat$news_text),
  Гарбуз = grepl("юр(і|и)(й|я|ю|ем|єм) гарбуз",dat$news_text),
  Барна = grepl("барн",dat$news_text),
  `Савченко Олександр` = grepl("(а|о)лександр(|а|ом|у|ові) савченк",dat$news_text),
  Гордєєв = grepl("(а|о)лекс(ій|ей|ія|ея|еем|ієм) горд(єє|ее)в",dat$news_text),
  Ткаченко = grepl("юр(і|и)(й|я|ю|ем|єм) ткаченк",dat$news_text),
  Муляренко = grepl("муляренк",dat$news_text),
  Гончарук = grepl("гончарук",dat$news_text),
  Савченко = grepl("(а|о)лекс(і|е)(й|я|ю|єм|ем) савченк",dat$news_text),
  Фіщук = grepl("ф(и|і)щук",dat$news_text),
  Горган = grepl("горган",dat$news_text),
  Головко = grepl("головко",dat$news_text),
  Клочко = grepl("клочк",dat$news_text),
  Коровій = grepl("коров(і|и)",dat$news_text),
  Корнійчук = grepl("корн(і|и)йчук",dat$news_text)
)

masiv <- masiv[apply(masiv[7:40], 1, sum) > 0, ]

masiv <- masiv %>% mutate_at(.vars = 7:40, function(x) ifelse(as.numeric(x) == 1, 1, NA))

masiv <- masiv %>% left_join(kods, by = c("Источник" = "news_source"))

cat(paste0("[",Sys.time(),"]"," Writing evt\n"))
evt(masiv, paste0("odesa_", input_dates[length(input_dates)]))

cat("Press return, if IA is ready\n")
dali <- readLines("stdin", n = 1)

pt <- fs::dir_info("~/Downloads/") %>% arrange(desc(modification_time)) %>% filter(str_detect(path, "Individual Analysis") & !str_detect(path, "\\$")) %>% slice(1) %>% pull(path)

masiv$Охоплення <- readxl::read_excel(pt, sheet = "Reach & Frequency")[-c(1:3), -1] %>% mutate(
  Rch = Rch * 100
) %>% pull(Rch)

cat(paste0("[",Sys.time(),"]", " Writing xlsx\n"))

masiv$Текст <- substr(masiv$Текст,1,32766)

#masiv <- masiv %>% filter(Дата >= input_dates)

fileXls <- paste0(getwd(), "/workfiles/odesa/odesa_tv_", input_dates[1], " - ", input_dates[length(input_dates)], ".xlsx")
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "tv")

openxlsx::writeDataTable(wb, "tv", masiv, withFilter = F)
openxlsx::saveWorkbook(wb, file = fileXls, overwrite = T)

#gmail_auth(scope = "full", secret_file = "~/context/client_secret_780645875644-m1kk5tro7vs3mhuum8m4ulcfo1vfvl5d.apps.googleusercontent.com (1).json")

send.mail(from = "Roman Kyrychenko<roman.kyrychenko@corestone.expert>",
          to = c("kirichenko17roman@gmail.com", "dovhoshyia.t@gmail.com", "baskakov.j@gmail.com"),
          #replyTo = c("Reply to someone else <someone.else@gmail.com>"),
          html = F,encoding = "utf-8", #inline = T,
          subject = paste0("Odesa: Context ", input_dates[1], " - ", input_dates[length(input_dates)]),
          body = paste0("Дані за ", input_dates[1], " - ", input_dates[length(input_dates)]),
          attach.files = c(fileXls),
          smtp = list(host.name = "smtp.openxchange.eu", port = 587, user.name = "roman.kyrychenko@corestone.expert", passwd = "21](,r:==P"),
          authenticate = TRUE,
          send = TRUE)

#a <- capture.output({
#  test_email <- mime(
#    To = "dovhoshyia.t@gmail.com", 
#    Bcc = "baskakov.j@gmail.com",
#    # To = "iryna.zaporozhets@corestone.expert",
#    From = "kirichenko17roman@gmail.com",
#    Subject = paste0("Odesa: Context ", input_dates[1], " - ", input_dates[length(input_dates)]),
#    body = paste0("Дані за ", input_dates[1], " - ", input_dates[length(input_dates)])
#  ) %>%
#    attach_file(fileXls) %>%
#    attach_file(fileXls)
#  
#  send_message(test_email)
#})

cat(paste0("[",Sys.time(),"]", " Ready!\n"))
