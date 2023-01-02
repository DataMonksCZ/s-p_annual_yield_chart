rm(list = ls())
options(scipen = 999)

#install.packages("pacman")
pacman::p_load(ggplot2, ggthemes, gganimate, plotly, ggshadow, ggrepel, directlabels,
               tidyr, dplyr, tidyverse, tibble, data.table,
               quantmod, rugarch, readxl, lubridate,  scales,
               av)



######################################### Functions #############################################
#cumprod with NA's, suitable also for cumsum with NA's
cumpfun <- function(x){
  x[!is.na(x)] <- cumprod(x[!is.na(x)]+1)-1
  x
}

######################################## Nastavení ################################################
#vybrané roky k vizualizaci labelů na konci lines
year_labels <- c(1970, 1973, 1974, 2002, 1993, 1995, 2000, 2002, 2008, 2019, 2020, 2021, 2022)

######################################## Načtení dat ################################################

data <- getSymbols("^GSPC", from = "1960-01-01", to = Sys.Date()-2, auto.assign = F)

################################### Úprava a filtrace dat #######################################
data <- tibble::rownames_to_column(data.frame(as.matrix(cbind(data, Year = year(data)))), "Date")
data$Date <- as.Date(data$Date)
data <- as.data.table(data)

#oindexuje jednotlivé roky
data[, index := seq_len(.N), by = list(Year)]

##výpočet return + cumproduct

data <- data %>% 
  mutate(return = c(NA,GSPC.Close[2:length(GSPC.Close)]/GSPC.Close[1:(length(GSPC.Close)-1)]-1)) %>%
  group_by(Year) %>% 
  mutate(csum = cumpfun(return))

#ungroupne, aby šel udělat jiný group_by na mean
data <- ungroup(data)

#průměr
mmean <- data %>%
  group_by(index) %>%
  summarise_at(vars(csum), list(meanv = mean)) %>%
  #filtrace kvůli tomu, že 253 a 254 dní má jen malinko indexů a zkresluje to průměr
  filter(index %in% 0:252) %>%
  #natvrdo přidá rok, aby se lehce plotovalo
  mutate(Year = 1900)

#doplní nulté indexy, aby grafy začínaly ve stejném bodě a label roku na konci
data$label = NA
index_one <- which(data$index == 1)
ii <- 0
for (i in index_one){
  #doplní nultý index
  data <- add_row(data, Date = data$Date[i+ii]-1, Year = data$Year[i+ii], index = 0, csum = 0, .before = i+ii)
  #doplní label roku
  if ((data$Year[i+ii]-1) %in% year_labels) data$label[i+ii-1] <- data$Year[i+ii-1]
  
  ii<-ii+1
}
#
#data$label[index_one[2:length(index_one)]-1] <- data$Year[index_one[2:length(index_one)]-1]

#aktuální rok pro zvýraznění
curr_year <- max(data$Year)

#graf
p <- ggplot(data %>% filter(Year %in% c(1966, 1973, 1974, 2002, 2008, 2022)) , aes(y = csum, x = index, group = Year))+
  geom_line(aes(col = Year), size = 0.2)+
  #glowing current year
  geom_line(data = subset(data,  Year == curr_year), size = 3, colour = "red", alpha = 0.1)+
  geom_line(data = subset(data,  Year == curr_year), size = 2, colour = "red", alpha = 0.2)+
  geom_line(data = subset(data,  Year == curr_year), size = 1, colour = "red", alpha = 0.5)+
  #glowing mean
  # geom_line(data = mmean, aes(x = index, y = meanv), size = 3, colour = "black", alpha = 0.1)+
  # geom_line(data = mmean, aes(x = index, y = meanv), size = 2, colour = "black", alpha = 0.2)+
  # geom_line(data = mmean, aes(x = index, y = meanv), size = 1, colour = "black", alpha = 0.5)+
  
  ggtitle("The six worst years of S&P 500 during 1960-2022")+
  # geom_label_repel(aes(label = label),
  #                  nudge_x = 1,
  #                  na.rm = TRUE)+
  
  theme_clean()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1,1, 1, 1, "cm"),
        legend.position= "none"
  )+
  #část animace
  geom_point(aes(col = Year))+
  #geom_segment(aes(xend = 255, yend = csum), linetype = 2, colour = 'grey') +
  geom_text(aes(x = index + 5, label = Year), hjust = 0, size = 7) + transition_reveal(along = index)

animation <- animate(p, width = 1080, height = 566, 
        end_pause = 40,
        fps = 20, rewind = FALSE, duration = 20)

########################## Ukládání ################################
#jako gif
anim_save("s&p_anim.gif", animation)
p
dev.off()

#jako video
anim_save("s&p_anim.mp4", animation)

