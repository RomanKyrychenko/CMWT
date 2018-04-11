setwd("~/CMWT/")

invisible(Sys.setlocale(locale = "UK_ua"))

options("openxlsx.dateFormat" = "dd.mm.yyyy")
options("openxlsx.datetimeFormat" = "dd.mm.yyyy")

suppressPackageStartupMessages({
  require(reticulate)
  require(tuber)
  require(dplyr)
  require(lubridate)
  require(stringr)
})

yt_oauth("780645875644-m1kk5tro7vs3mhuum8m4ulcfo1vfvl5d.apps.googleusercontent.com", "XJhIz5zcNTnY7iaBLOmX6JiM")

cnl <- c("UCwH-Z6mFREUAE5hdAGXYsZw", "UCMZmUbNzLkAeFDFOCEbY8ng", "UCOCwYne_F0SeOaIuX6nmT1g", "UCwB_ZIlOTdoFg2tu5fSuv_Q", "UCS7g49TiDIa5RGdf_-u5oKQ", "UCWL8vLJenNk5u2T6fv1qDww")

use_python("/Users/romankyrychenko/anaconda3/bin/python3.6")
py_config()

findDates <- function(strings) {
  pattern <- "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]"
  readr::parse_datetime(str_extract(strings, pattern))
}

cv2 <- import("cv2")
numpy <- import("numpy")
face_recognition <- import("face_recognition")

faceCascade = cv2$CascadeClassifier("/Users/romankyrychenko/anaconda3/share/OpenCV/haarcascades/haarcascade_frontalface_default.xml")

video_proc <- function(videos, fsp = 4) {
  
  invisible(purrr::map(videos, function(video) {
    dir.create(paste0("~/", result_date, "/www.youtube.com/", gsub(".webm", "", gsub(".mp4", "", basename(video)))))
    
    system(paste0("ffmpeg -analyzeduration 2147483647 -probesize 2147483647 -i ", video, " -vf fps=", 1/fsp, " ", "~/", result_date, "/www.youtube.com/", gsub(".mp4", "", gsub(".mp4", "", basename(video))), "/out%d.png"))
    
    files <- list.files(paste0("~/", result_date, "/www.youtube.com/", gsub(".mp4", "", basename(video))), ".png", full.names = T, recursive = T)
    
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
    #system(paste0('./recognize.sh ./',gsub(".mp4", "", basename(video))))
  }))
}

video_proc <- compiler::cmpfun(video_proc)

this_is <- function(new_face, face = "know/Stepanov.jpg") {
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