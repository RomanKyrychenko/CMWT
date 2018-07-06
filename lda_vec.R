#!/usr/bin/env Rscript

source("~/CMWT/common.R")
source("~/CMWT/topic.R")

suppressPackageStartupMessages({
  require(elastic)
  require(RCurl)
  require(gtools)
  require(clisymbols)
  require(purrr)
  require(R.utils)
})

csv_to_json <- function(dat, pretty = F, na = "null", raw = "mongo", digits = 3, force = "unclass") {
  dat_to_json <- jsonlite::toJSON(dat, pretty = pretty, na = "null", raw = raw, digits = digits, force = force)
  substr(dat_to_json, start = 2, nchar(dat_to_json) - 1)
}

result_date <- gsub("-", "", as.character(Sys.Date() - 1))

train_lda <- function(tem, k) {
  it_train <- itoken(
    text.clean(tem$fullhtml), preprocessor = str_to_lower,
    ids = tem$`_id`, progressbar = T
  )
  vocab <- create_vocabulary(it_train, stopwords = stopwords)
  
  pruned_vocab <- prune_vocabulary(vocab, term_count_min = 5, doc_proportion_max = 0.5, doc_proportion_min = 0.001)
  
  vectorizer <- vocab_vectorizer(pruned_vocab)

  dtm_train <- create_dtm(it_train, vectorizer)
  
  lda_model <- text2vec::LDA$new(n_topics = k)
  
  doc_topic_distr <- lda_model$fit_transform(
    x = dtm_train, n_iter = 1500,
    convergence_tol = 0.0001, n_check_convergence = 25,
    progressbar = F
  )
  
  gammaDF <- as_tibble(doc_topic_distr)
  names(gammaDF) <- c(1:k)
  
  toptopics <- tibble(
    `_id` = attr(doc_topic_distr, "dimnames")[[1]],
    theme = as.numeric(apply(gammaDF, 1, function(x) names(gammaDF)[which(x == max(x))][1])),
    theme_quality = apply(gammaDF, 1, max)
  )
  
  tem <- left_join(tem, toptopics, by = "_id")
  slov <- left_join((tem %>%
                       group_by(theme) %>%
                       summarise(theme_quality = max(theme_quality)[1])), tem %>% select(title, theme, theme_quality), by = c("theme_quality", "theme")) %>%
    rename("theme2" = "title")
  left_join(tem, (slov[!duplicated(slov[, "theme"]), ] %>% select(theme, theme2)), by = c("theme")) %>%
    select(-theme) %>%
    rename("theme" = "theme2")
}

cat(paste(symbol$tick, "Start connection:", Sys.time()))

invisible(connect(es_host = es_host, es_port = es_port, es_user = es_user, es_pwd = es_pwd))

cat(paste(symbol$tick, "Start download:", Sys.time()))

res <- Search(
  index = paste0("urls_", result_date), config = httr::add_headers("Content-Type" = "application/json"), #body = q,
  type = "news", source = paste("title", "fullhtml", "dtpost", "domain", sep = ","), time_scroll = "1m"
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

out_df <- map_dfr(out, flatten_df) %>% mutate(
  title = title %>% str_replace_all("[:punct:]", " ") %>% str_squish()
) %>% filter(
  !(str_detect(fullhtml, "(ë|ž|İ|š|η|μ|o|ö)") | (fullhtml == "") | (fullhtml == "No fullhtml") | nchar(fullhtml) < 30)
) 

cat(paste(symbol$tick, "Downloaded", Sys.time()))

cat(paste(symbol$tick, "Start training", Sys.time()))

startdate <- paste0(substr(as.character(Sys.time()), 1, 10), "T", substr(as.character(Sys.time()), 12, 20), "+0300")

out_df2 <- train_lda(out_df, nrow(out_df) / 18)
gc()

out_df2 <- out_df2 %>% filter(theme_quality > median(theme_quality))

out_df2 <- bind_rows(out_df2, train_lda(out_df %>% anti_join(out_df2, by = "_id"), nrow(out_df2) / 10))

cat(paste(symbol$tick, "Trained", Sys.time()))

names(out_df2) <- c("_index", "_type", "_id", "_score", "fullhtml", "domain", "dtpost", "title", "theme_quality", "theme")
saveRDS(out_df2, "out_df2.rds")

cat(paste(symbol$tick, "Start writing", Sys.time()))

fl <- paste0(getwd(), "/elastic/OU", result_date, ".json")
sink(file = paste0(getwd(), "/elastic/backup", result_date, ".json"))
con <- file(fl, open = "a", encoding = "UTF-8")
for (i in 1:nrow(out_df2)) {
  write(
    print(paste0(
      '{"update": {"_id": "', out_df2[i, 3], '", "_type" : "news", "_index" : "',
      out_df2[i, 1], '"}}\n{"doc": {"theme": "', str_replace_all(gsub(
        "|", "",
        str_replace_all(out_df2[i, "theme"], "[[:punct:]]", "")
      ), "[\r\n]", ""), '", "theme_quality": ', as.numeric(out_df2[i, "theme_quality"]), "}}"
    )), file = con,
    append = TRUE
  )
}
sink()
cat(paste(symbol$tick, "Start bulking:", Sys.time()))

withTimeout <- safely(withTimeout)

gc()

interruptor = function(time.limit){
  results <- withTimeout({docs_bulk(fl)}, timeout = time.limit, onTimeout = "silent")
  while(is.null(results$result$result)){
    results <- withTimeout({docs_bulk(fl)}, timeout = time.limit, onTimeout = "silent")
  }
  results$result$result
}   

res <- interruptor(time.limit = 400)

httpPUT(
  paste(es, paste0("urls_", result_date), "news", "theme", sep = "/"),
  csv_to_json(data.frame(dt = startdate, dtpost = paste0(substr(as.character(Sys.time()), 1, 10), "T", substr(as.character(Sys.time()), 12, 20), "+0300")))
)
cat(paste(symbol$tick, "Bulked:", Sys.time()))

sink(paste0(getwd(), "/elastic/log/", result_date, ".log"))
cat(paste0("Datetime: ", startdate, "\nNews: ", nrow(out_df2), "\nThemes: ", length(unique(out_df2$theme))))
sink()

q(save = "no")