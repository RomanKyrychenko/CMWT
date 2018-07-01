#!/usr/bin/env Rscript

require(elastic)
source("~/CMWT/common.R")

connect(es_host = es_host, es_port = es_port)

q <- '{
"query": {
"query_string" : {
"default_field" : "domain",
"query" : "1"
}
}
}'

#input_dates <- (Sys.Date() - 1) %>% format("%Y%m%d")

#if (lubridate::wday(Sys.Date() - 1) == 1) {
  input_dates <- seq(Sys.Date()-5, Sys.Date()-1, "days") %>% format("%Y%m%d")
#}

dl_el <- function(result_date) {
  res <- Search(
    index = paste0("urls_", result_date), body = q,
    type = "news", source = paste("dtpost", "dtend", "descr", sep = ","), 
    scroll = "1m"
  )
  out <- res$hits$hits
  hits <- 1
  while (hits != 0) {
    res <- scroll(res$`_scroll_id`)
    hits <- length(res$hits$hits)
    if (hits > 0) {
      out <- c(out, res$hits$hits)
    }
  }
  
  map_dfr(out, flatten_df) %>% mutate(
    Дата = as.Date(dtpost),
    `Start time` = ymd_hms(dtpost, tz = "MET"),
    `End time` = ymd_hms(dtend, tz = "MET"),
    Источник = descr
  ) %>% left_join(
    kods, by = c("Источник" = "news_source")
  ) %>% filter(
    !is.na(ID)
  )
}

chans <- dl_el(input_dates)

evt(chans, "test")

cat("Press return, if IA is ready\n")
dali <- readLines("stdin", n = 1)

pt <- fs::dir_info("~/Downloads/") %>% arrange(desc(modification_time)) %>% filter(str_detect(path, "Individual Analysis")) %>% slice(1) %>% pull(path)

chans$Охоплення <- readxl::read_excel(pt, sheet = "Reach & Frequency")[-c(1:3), -1] %>% mutate(
  Rch = Rch * 100
) %>% pull(Rch)

con <- file(paste0("/Users/romankyrychenko/CMWT/", input_dates[1], ".json"), open = "a", encoding = "UTF-8")
for (i in 1:nrow(chans)) {
  write(
    print(paste0(
      '{"update": {"_id": "', chans[i, "_id"], '", "_type" : "news", "_index" : "',
      chans[i, "_index"], '"}}\n{"doc": {"total_bigmir": ', 0, ', "detailed_bigmir": ',chans[i, "Охоплення"], ',"detailed_liveinternet": 0, "total_liveinternet": 0}}'
    )), file = con,
    append = TRUE
  )
}

res <- tryCatch(docs_bulk(paste0("/Users/romankyrychenko/CMWT/", input_dates[1], ".json")))
