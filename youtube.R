setwd("/home")

suppressPackageStartupMessages({
  require(reticulate)
  require(tuber)
  require(dplyr)
  require(lubridate)
  require(stringr)
  require(openxlsx)
})

options("openxlsx.dateFormat" = "dd.mm.yyyy")
options("openxlsx.datetimeFormat" = "dd.mm.yyyy")

yt_oauth("780645875644-m1kk5tro7vs3mhuum8m4ulcfo1vfvl5d.apps.googleusercontent.com", "XJhIz5zcNTnY7iaBLOmX6JiM")

cnl <- c("UCwH-Z6mFREUAE5hdAGXYsZw", "UCMZmUbNzLkAeFDFOCEbY8ng", "UCOCwYne_F0SeOaIuX6nmT1g", "UCwB_ZIlOTdoFg2tu5fSuv_Q", "UCS7g49TiDIa5RGdf_-u5oKQ", "UCWL8vLJenNk5u2T6fv1qDww")

findDates <- function(strings) {
  readr::parse_datetime(str_extract(strings, "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]"))
}

cv2 <- import("cv2")
numpy <- import("numpy")
face_recognition <- import("face_recognition")

faceCascade = cv2$CascadeClassifier("/usr/local/lib/python3.5/dist-packages/cv2/data/haarcascade_frontalface_default.xml")

video_proc <- function(videos, fsp = 2) {
  
  invisible(purrr::map(videos, function(video) {
    dir.create(paste0("/home/", result_date, "/www.youtube.com/", gsub(".webm", "", gsub(".mp4", "", basename(video)))))
    
    system(paste0("ffmpeg -analyzeduration 2147483647 -probesize 2147483647 -i ", video, " -vf fps=", 1/fsp, " ", "/home/", result_date, "/www.youtube.com/", gsub(".mp4", "", gsub(".mp4", "", basename(video))), "/out%d.png"))
    
    files <- list.files(paste0("/home/", result_date, "/www.youtube.com/", gsub(".mp4", "", basename(video))), ".png", full.names = T, recursive = T)
    
    pb <- dplyr::progress_estimated(length(files), 0)
    
    invisible(purrr::map(files, function(file) {
      
      if (pb$i < pb$n) pb$tick()$print()
      newpath = paste0(dirname(file))
      if (!dir.exists(newpath)) dir.create(newpath)
      
      image = np_array(cv2$imread(file), dtype = "uint8")
      
      gray = np_array(cv2$cvtColor(image, cv2$COLOR_BGR2GRAY), dtype = "uint8")
      
      faces = faceCascade$detectMultiScale(gray, scaleFactor = 1.1, minNeighbors = 10L, minSize = tuple(30L, 30L), flags = cv2$CASCADE_SCALE_IMAGE)
      
      left = 10
      right = 10
      top = 10
      bottom = 10
      if (length(faces) != 0) {
        purrr::map(1:nrow(faces), function(i) {
          tryCatch({
            x = faces[i, 1]
            y = faces[i, 2]
            w = faces[i, 3]
            h = faces[i, 4]
            st1 = y + h + bottom
            en1 = y - top
            st2 = x + w + right
            en2 = x - left
            dim1 = rev(st1:en1)
            dim2 = rev(st2:en2)
            image = r_to_py(py_to_r(image)[dim1, dim2, 1:3])
            
            cv2$imwrite(paste0(newpath, "/cropped_face", i, "_", substr(strftime(as_datetime(readr::parse_number(basename(file)) * fsp - fsp)-3*60*60), 12, 19), ".png"), image)
          }, error = function(e) NULL)
        })
      }
      file.remove(file)
    }))
    gc()
  }))
}

video_proc <- compiler::cmpfun(video_proc)

this_is <- function(new_face, face = "https://raw.githubusercontent.com/RomanKyrychenko/CMWT/master/Stepanov.jpg") {
  tryCatch({
    picture_of_me = np_array(face_recognition$load_image_file(face), dtype = "uint8")
    my_face_encoding = np_array(face_recognition$face_encodings(picture_of_me)[[1]])
    
    unknown_picture = np_array(face_recognition$load_image_file(new_face), dtype = "uint8")
    unknown_face_encoding = r_to_py(face_recognition$face_encodings(unknown_picture))
    
    data_frame(new_face, is = face_recognition$compare_faces(my_face_encoding, unknown_face_encoding)[[1]], 
               quality = 1-face_recognition$face_distance(my_face_encoding, unknown_face_encoding)[[1]])
  }, error = function(e) NULL)
}

this_is <- compiler::cmpfun(this_is)

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
  system(paste0("youtube-dl -f 'bestvideo[ext=mp4]+bestaudio[ext=m4a]/mp4' ", i, " -o ", "/home/", as.character(result_date), "/", i))
}

videos <- list.files(paste0("/home/", result_date, "/www.youtube.com"), full.names = T, recursive = T, pattern = ".mp4")

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
fn <- paste0("/home/stepanov_youtube/Stepanov_youtube_", result_date, ".xlsx")
openxlsx::saveWorkbook(wb, file = fn, overwrite = T)

py_run_file("https://raw.githubusercontent.com/RomanKyrychenko/CMWT/master/mail.py")

cat(paste0("[",Sys.time(),"]", " Ready!\n"))

