
source("common.R")
require(ggplot2)
require(stringr)

nc <- c("CHANNEL 112", "NEWS ONE", "CHANNEL Z(ZIK)", "PRIAMYI", "5 CHANNEL", "24 CHANNEL", "ESPRESO TV")

ptt <- fs::dir_info("~/Downloads/") %>% arrange(desc(modification_time)) %>% filter(str_detect(path, "Каналы_day_ср") & !str_detect(path, "\\$")) %>% pull(path) %>% first()
cdt <- xlsx::read.xlsx(ptt, sheetIndex = 1) %>% as_tibble() %>% mutate_all(as.character)

pt <- fs::dir_info("~/Downloads/") %>% arrange(desc(modification_time)) %>% filter(str_detect(path, "Каналы  day") & !str_detect(path, "\\$")) %>% pull(path) %>% first()

cd <- map(1:3, function(x) xlsx::read.xlsx(pt, sheetIndex = x) %>% as_tibble() %>% mutate_all(as.character))

day <- map_chr(1:3, function(x) cd[[x]][[1]][2]) %>% dmy

#names(cd) <- cd %>% slice(1) %>% flatten_chr()

kyiv_all <- function(mas) {
  mas <- mas[4:nrow(mas),] %>% mutate_at(2:7, as.numeric) %>% mutate_at(c(2,5), function(x) x*100)
  r <- which(mas[,1] == "киев 18+")
  
  all <- mas %>% slice(1:(r-1))
  kyiv <- mas %>% slice((r+1):nrow(mas))
  
  cdt <- cdt[3:nrow(cdt), 2:4] %>% mutate_at(2:3, as.numeric) %>% mutate_at(2:3, function(x) x*100)
  
  all <- all %>% left_join(cdt[,1:2], by = c("X." = "Channel"))
  kyiv <- kyiv %>% left_join(cdt[,c(1, 3)], by = c("X." = "Channel"))
  
  names(kyiv) <- names(all)
  list(all, kyiv)
}

mas <- map(1:3, function(x) kyiv_all(cd[[x]]))

create_plot <- function(all, name = "all") {
  l = -1:nrow(all)+1
  r = c(0, 1, 2.6, 3.8, 5, 6,7,8.2,9.2,10.2)
  
  rat <- all[all$X. %in% nc,] %>% arrange(desc(`NA.`)) %>% slice(1) %>% pull(1) 
  
  rch <- all[all$X. %in% nc,] %>% arrange(desc(`X03.00.00...27.00.00.x`)) %>% slice(1) %>% pull(1) 
  rch_k <- round((all[all$X. %in% nc,] %>% arrange(desc(`X03.00.00...27.00.00.x`)) %>% slice(1) %>% pull(2)) / 1000000, ifelse(str_detect(name,"all"), 2, 3)) %>% format(decimal.mark = ",")
  
  rat <- case_when(rat == "CHANNEL 112" ~ "112 канал",
                   rat == "NEWS ONE" ~ "News One",
                   rat == "CHANNEL Z(ZIK)" ~ "ZIK",
                   rat == "PRIAMYI" ~ "Прямий",
                   rat == "5 CHANNEL" ~ "5 канал",
                   rat == "24 CHANNEL" ~ "24 канал",
                   rat == "ESPRESO TV" ~ "Еспресо ТВ")
  
  rch <- case_when(rch == "CHANNEL 112" ~ "112 канал",
                   rch == "NEWS ONE" ~ "News One",
                   rch == "CHANNEL Z(ZIK)" ~ "ZIK",
                   rch == "PRIAMYI" ~ "Прямий",
                   rch == "5 CHANNEL" ~ "5 канал",
                   rch == "24 CHANNEL" ~ "24 канал",
                   rch == "ESPRESO TV" ~ "Еспресо ТВ")
  
  cairo_pdf(paste0("workfiles/tv_channels/plots/plot_", name, ".pdf"), width = 11, height = 11)
  print(ggplot() + 
          geom_rect(aes(xmin = 0, xmax = 10.2, ymin = max(l)-1, ymax = max(l)+2), fill = "#DCE6F1", color = NA) +
          geom_text(aes(x=5.1,y=max(l)+1.5, label = "Аудиторія 18+, вся Україна"), fontface = "bold", family = "PT Sans") +
          geom_text(aes(x=0.5,y=max(l), label = "Date"), family = "PT Sans", size = 3) +
          geom_text(aes(x=1.65,y=max(l), label = "Channel"), family = "PT Sans", size = 3) +
          geom_text(aes(x = 3.2, y = max(l), label = "Середнє охоплення\nза минулий тиждень"), family = "PT Sans", size = 3) +
          geom_text(aes(x = 4.4, y = max(l) - .5, label = "Охоплення, чол."), family = "PT Sans", size = 3) +
          geom_text(aes(x = 5.5, y = max(l) - .5, label = "Частка, %"), family = "PT Sans", size = 3) +
          geom_text(aes(x = 6.5, y = max(l) - .5, label = "Рейтинг, %"), family = "PT Sans", size = 3) +
          geom_text(aes(x = 7.6, y = max(l) - .5, label = "Охоплення, чол."), family = "PT Sans", size = 3) +
          geom_text(aes(x = 8.7, y = max(l) - .5, label = "Частка, %"), family = "PT Sans", size = 3) +
          geom_text(aes(x = 9.7, y = max(l) - .5, label = "Рейтинг, %"), family = "PT Sans", size = 3) +
          geom_text(aes(x = 5.5, y = max(l)  + .5, label = "Day"), family = "PT Sans", size = 3) +
          geom_text(aes(x = 8.7, y = max(l)  + .5, label = "Prime"), family = "PT Sans", size = 3) +
          geom_linerange(aes(x = r, r, ymin = rep(0, 10), ymax = rep(max(l), 10)), linetype = "dotted", size = 0.1) +
          geom_segment(aes(x = rep(0,length(l)-1),xend = rep(10.2, length(l)-1),y = l[-length(l)], yend = l[-length(l)]), linetype = "dotted", size = 0.1) + 
          geom_segment(aes(x = r[4], xend = r[10], y = max(l), yend = max(l)), linetype = "dotted", size = 0.1) +
          geom_segment(aes(x = r[1], xend = r[10], y = max(l)+1, yend = max(l)+1), linetype = "dotted", size = 0.1) +
          geom_linerange(aes(x = r[c(1:4, 7, 10)], r[c(1:4, 7, 10)], ymin = rep(max(l), 6), ymax = rep(max(l)+1, 6)), linetype = "dotted", size = 0.1) +
          geom_linerange(aes(x = r[c(1, 10)], r[c(1, 10)], ymin = rep(max(l)+1, 2), ymax = rep(max(l)+2, 2)), linetype = "dotted", size = 0.1) +
          geom_segment(aes(x = r[1], xend = r[10], y = max(l)+2, yend = max(l)+2), linetype = "dotted", size = 0.1) +
          geom_text(aes(x = 0.1, y = (l-0.5)[-c(1,length(l))], label = day[parse_number(name)] %>% format("%d.%m.%Y")), hjust = 0, family = "PT Sans", family = "PT Sans") +
          geom_text(aes(x = 1.1, y = (l-0.5)[-c(1,length(l))], label = all %>% pull(1) %>% rev), hjust = 0, family = "PT Sans", family = "PT Sans") +
          geom_rect(aes(xmin = rep(2.65, length(l) - 2), xmax = 2.65 + rev((all %>% pull(8))/max(all %>% pull(8))), ymin = (l-0.05)[-c(1,length(l))], ymax = (l-0.95)[-c(1,length(l))]), fill = "#B7DEE8") +
          geom_text(aes(x = rep(3.75, length(l) - 2), y = (l-0.8)[-c(1,length(l))], label =  rev((all %>% pull(8))) %>% format(big.mark = " ")),hjust = 1, vjust = 0, fontface = "bold", family = "PT Sans") +
          geom_rect(aes(xmin = rep(3.85, length(l) - 2), xmax = 3.85 + rev((all %>% pull(2))/max(all %>% pull(2))), ymin = (l-0.05)[-c(1,length(l))], ymax = (l-0.95)[-c(1,length(l))]), fill = "#CCC0DA") +
          geom_text(aes(x = rep(4.95, length(l) - 2), y = (l-0.8)[-c(1,length(l))], label =  rev((all %>% pull(2))) %>% format(big.mark = " ")),hjust = 1, vjust = 0, family = "PT Sans") +
          geom_rect(aes(xmin = rep(5.05, length(l) - 2), xmax = 5.05 + rev((all %>% pull(3))/max(all %>% pull(3)))*0.9, ymin = (l-0.05)[-c(1,length(l))], ymax = (l-0.95)[-c(1,length(l))]), fill = "#B8CCE4") +
          geom_text(aes(x = rep(5.95, length(l) - 2), y = (l-0.8)[-c(1,length(l))], label =  rev((all %>% pull(3)))%>% format(decimal.mark = ",")),hjust = 1, vjust = 0, family = "PT Sans") +
          geom_rect(aes(xmin = rep(6.05, length(l) - 2), xmax = 6.05 + rev((all %>% pull(4))/max(all %>% pull(4)))*0.9, ymin = (l-0.05)[-c(1,length(l))], ymax = (l-0.95)[-c(1,length(l))]), fill = "#E6B8B7") +
          geom_text(aes(x = rep(6.95, length(l) - 2), y = (l-0.8)[-c(1,length(l))], label =  rev((all %>% pull(4)))%>% format(decimal.mark = ",")),hjust = 1, vjust = 0, family = "PT Sans") +
          geom_rect(aes(xmin = rep(7.05, length(l) - 2), xmax = 7.05 + rev((all %>% pull(5))/max(all %>% pull(5))), ymin = (l-0.05)[-c(1,length(l))], ymax = (l-0.95)[-c(1,length(l))]), fill = "#CCC0DA") +
          geom_text(aes(x = rep(8.15, length(l) - 2), y = (l-0.8)[-c(1,length(l))], label =  rev((all %>% pull(5))) %>% format(big.mark = " ")),hjust = 1, vjust = 0, family = "PT Sans") +
          geom_rect(aes(xmin = rep(8.25, length(l) - 2), xmax = 8.25 + rev((all %>% pull(6))/max(all %>% pull(6)))*0.9, ymin = (l-0.05)[-c(1,length(l))], ymax = (l-0.95)[-c(1,length(l))]), fill = "#B8CCE4") +
          geom_text(aes(x = rep(9.15, length(l) - 2), y = (l-0.8)[-c(1,length(l))], label =  rev((all %>% pull(6)))%>% format(decimal.mark = ",")),hjust = 1, vjust = 0, family = "PT Sans") +
          geom_rect(aes(xmin = rep(9.25, length(l) - 2), xmax = 9.25 + rev((all %>% pull(7))/max(all %>% pull(7)))*0.9, ymin = (l-0.05)[-c(1,length(l))], ymax = (l-0.95)[-c(1,length(l))]), fill = "#E6B8B7") +
          geom_text(aes(x = rep(10.1, length(l) - 2), y = (l-0.8)[-c(1,length(l))], label =  rev((all %>% pull(7)))%>% format(decimal.mark = ",")),hjust = 1, vjust = 0, family = "PT Sans") +
          theme_void(base_family = "PT Sans", base_size = 9) +
          theme(plot.margin=unit(c(-1.37,-1.37,-1.37,-1.37,-1.37), "cm")))
  dev.off()
  return(c(rat, rch, rch_k))
}

all1 <- create_plot(mas[[1]][[1]], "all1")
kyiv1 <- create_plot(mas[[1]][[2]], "kyiv1")

all2 <- create_plot(mas[[2]][[1]], "all2")
kyiv2 <- create_plot(mas[[2]][[2]], "kyiv2")

all3 <- create_plot(mas[[3]][[1]], "all3")
kyiv3 <- create_plot(mas[[3]][[2]], "kyiv3")

#all4 <- create_plot(mas[[4]][[1]], "all4")
#kyiv4 <- create_plot(mas[[4]][[2]], "kyiv4")