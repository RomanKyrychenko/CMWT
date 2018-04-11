#!/usr/bin/env Rscript

source("~/context/youtube_common.R")

result_date <- Sys.Date()

vds <- purrr::map_dfr(cnl, tuber::list_channel_videos) %>% mutate(
  contentDetails.videoPublishedAt = lubridate::ymd_hms(contentDetails.videoPublishedAt)
) %>% filter(contentDetails.videoPublishedAt >= as.POSIXct(result_date) & contentDetails.videoPublishedAt < as.POSIXct(result_date + 1)) %>% mutate(
  url = paste0("www.youtube.com/watch?v=", contentDetails.videoId)
) %>% select(url, contentDetails.videoPublishedAt, contentDetails.videoId)

vds <- bind_cols(
  vds, purrr::map_dfr(vds$contentDetails.videoId, function(x) get_video_details(x)$items[[1]]$snippet[c(3, 6)] %>% as_data_frame()),
  purrr::map_dfr(vds$contentDetails.videoId, function(x) get_stats(x) %>% as_data_frame()) %>% select(viewCount, likeCount, dislikeCount, favoriteCount, commentCount)
) %>% select(url, channelTitle, contentDetails.videoPublishedAt, title, viewCount, likeCount, dislikeCount, favoriteCount)

dir.create(as.character(result_date))

for (i in vds$url) {
  system(paste0("youtube-dl ", i, " -o ", "~/", as.character(result_date), "/", i))
}

videos <- list.files(paste0("~/", result_date, "/www.youtube.com"), full.names = T, recursive = T, pattern = ".mp4")

video_proc(videos)

test <- list.files(as.character(result_date), full.names = T, recursive = T, pattern = ".png")
rs <- purrr::map_dfr(test, this_is) %>% mutate(
  time = as.numeric(hms(gsub("400", "00", gsub("300", "00", gsub("200", "00", gsub("100", "00", gsub(".png", "", gsub("*._", "", gsub("cropped_face", "", basename(new_face)))))))))),
  dir = dirname(new_face)
) %>% filter(is == T) %>% group_by(dir) %>% summarise(
  start = strftime(as_datetime(min(time, na.rm = T)) - 3600 * 3) %>% stringr::str_replace("1970-01-01 ", ""),
  end = strftime(as_datetime(max(time, na.rm = T)) - 3600 * 3) %>% stringr::str_replace("1970-01-01 ", ""),
  quality = mean(quality)
) %>% filter(quality > 0.45) %>% mutate(
  url = paste0("www.youtube.com/watch?v=", substr(gsub(".*v=", "", dir), 1, 11))
) %>% left_join(vds, by = "url") %>% select(url, channelTitle, start, end, contentDetails.videoPublishedAt, title, viewCount, likeCount, dislikeCount, favoriteCount, quality)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "youtube")
openxlsx::writeDataTable(wb, "youtube", rs, withFilter = F)
openxlsx::setColWidths(wb, "youtube", c(1, 2, 5, 6), widths = 35, ignoreMergedCells = FALSE)
openxlsx::saveWorkbook(wb, file = paste0("~/stepanov_youtube/Степанов_ютуб_", result_date, ".xlsx"), overwrite = T)

suppressPackageStartupMessages(library(gmailr))

gmail_auth(scope = "full", secret_file = "~/context/client_secret_780645875644-m1kk5tro7vs3mhuum8m4ulcfo1vfvl5d.apps.googleusercontent.com (1).json")

a <- capture.output({
  test_email <- mime(
    To = "baskakov.j@gmail.com",
    From = "kirichenko17roman@gmail.com",
    Subject = paste0("Stepanov youtube: ", result_date),
    body = paste0("Дані за ", result_date)
  ) %>%
    attach_file(paste0("~/stepanov_youtube/Степанов_ютуб_", result_date, ".xlsx")) %>%
    attach_file(paste0("~/stepanov_youtube/Степанов_ютуб_", result_date, ".xlsx"))
  
  send_message(test_email)
})

cat(paste0("[",Sys.time(),"]", " Ready!\n"))