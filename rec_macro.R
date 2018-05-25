start = Sys.time()

suppressPackageStartupMessages({
  require(parallel)
  require(magrittr)
  require(magick)
  require(tesseract)
  require(EBImage)
  require(keras)
  require(foreach)
  require(doParallel)
})

#source("~/CMWT/common.R")
#
#input_dates <- Sys.Date() - 1 
#
#if (lubridate::wday(input_dates) == 1) {
#  input_dates <- as.Date((input_dates - 2):input_dates, "1970-01-01")
#}
#
#dat <- context_data(input_dates:(Sys.Date() - 1))

dname <- gsub("-", "", input_dates[1])

dir.create(dname)

cores = detectCores()
cl <- makeCluster(cores[1]-1) 
registerDoParallel(cl)

#paste(dat$news_source, dat$program_datetime, dat$number) %>% unique()

foreach(i = dat$url) %dopar% {
  system(paste0("ffmpeg -i ", i, " -vf 'select=eq(n\\,0)' -q:v 3 ~/CMWT/", dname, "/", gsub(".mp4", "", gsub('http://server002.medisum.com.ua/mp4.php\\?id=', "", i)), ".png"))
}

stopCluster(cl)

hg <- 4:25

vec <- list(4:15, 16:30, 31:45, 47:63, 79:95, 96:110, 127:143, 144:160, 178:192, 192:207, 221:239, 240:255, 272:287, 288:303)[-c(1:8)]
vec2 <- list(4:15, 16:30, 31:45, 47:63, 79:95, 96:110, 127:143, 144:160, 240:255, 256:271, 288:303, 304:319, 334:349, 351:366)[-c(1:8)]
vec3 <- list(15:24, 24:33, 36:44, 45:51, 58:65, 66:73, 74:80, 82:88, 95:101, 103:109, 115:122, 123:130, 135:143, 144:151)[-c(1:8)]
vec4 <- list(5:15, 16:27, 28:38, 39:48, 58:67, 68:77, 86:95, 96:105, 120:129, 130:139, 148:157, 159:168, 176:185, 187:196)[-c(1:8)]

fl <- list.files(dname, full.names = T)
idx <- readr::parse_number(substr(fl, 12, 15))

train_photo <- data_frame(
  photo = fl,
  channel = dat$news_source[idx]
) %>% arrange(readr::parse_number(substr(fl, 12, 15)))

ip_keras <- function(img, nam) {
  ln <- readImage(img)
  if (nam == "ТК Київ") {
    vec <- vec3
    hg <- 4:15
  } else if (nam == "СТБ") {
    vec <- vec4
    hg <- 5:19
  } else if (nam %in% c("ТК Перший діловий", "ТК Интер", "ТК 24")) {
    vec <- vec2
  }
  mclapply(vec, function(x) {
    ln <- ln[x, hg, 1:3] %>% resize(w = 256, h = 256) %>% channel("gray")
    ln <- ln > mean(ln@.Data)
    ln %>% EBImage::imageData()
  }, mc.cores = 8)
}

ip_keras <- compiler::cmpfun(ip_keras)

keras_mas <- map2(train_photo %>% pull(photo), train_photo %>% pull(channel), ip_keras) %>% 
  unlist(recursive = FALSE) %>% 
  lapply(function(x) array(x, dim = c(1, 256, 256))) %>% 
  abind(along = 1)

model <- load_model_hdf5("model.hdf5")

preds_new <- model %>% predict_classes(keras_mas / 255)

probs <- apply(round(model %>% predict_proba(keras_mas / 255, verbose = 1), 2), 1, max)

chunk2 <- function(x, n) split(x, cut(seq_along(x), n, labels = FALSE))

preds_new <- paste0(dname, preds_new %>%
     chunk2(length(preds_new) / 6) %>%
     map_chr(function(x) paste(x, collapse = ""))) %>% lubridate::ymd_hms(tz = "Europe/Kiev")

probs <- probs %>% chunk2(length(probs) / 6) %>% map_dbl(mean)

dat$preds_new <- preds_new
dat$probs <- probs

#reslt <- data_frame(
#  preds_new,
#  probs,
#  tru = dat$news_datetime[idx],
#  dif = preds_new - dat$news_datetime[idx],
#  program = dat$program_datetime[idx],
#  ulr = dat$url[idx],
#  media = dat$news_source[idx],
#  num = dat$number[idx]
#)

dat$time <- if_else(dat$probs >= 0.99 & !is.na(dat$preds_new), dat$preds_new, dat$news_datetime)

dat$time <- if_else(dat$news_source == "ТК 24" & dat$program_datetime >= lubridate::ymd_hms(paste(Sys.Date()-1,"12:00:00"), tz = "Europe/Kiev"), dat$time+3600*12, dat$time)

dat$tru_time <- dat$time
#table(reslt$probs >= 0.99) %>% prop.table()

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

end = Sys.time()