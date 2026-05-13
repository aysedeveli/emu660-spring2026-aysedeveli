install.packages("dplyr")
install.packages("readr")
install.packages("tidyverse")
install.packages("janitor")
library(readr)
library(dplyr)
library(janitor)

tum_kitaplar <- read_csv("~/GitHub/emu660-spring2026-aysedeveli/tum_kitaplar.csv")
View(tum_kitaplar)



kitaplar <- read_csv("tum_kitaplar.csv")
save(kitaplar, file = "kitaplar.RData")

kitaplar <- clearNames(kitaplar)
names(kitaplar)
Yayinevleri <- unique(kitaplar$Yayınevi)







