install.packages("dplyr")
install.packages("readr")
library(readr)
library(dplyr)

tum_kitaplar <- read_csv("~/GitHub/emu660-spring2026-aysedeveli/tum_kitaplar.csv")
View(tum_kitaplar)



kitaplar <- read_csv("tum_kitaplar.csv")
save(kitaplar, file = "kitaplar.RData")

kitaplar <- kitaplar %>%
  mutate(
    Fiyat = as.numeric(gsub(",", ".", Fiyat)),
    'Yorum Sayısı' = as.numeric('Yorum Sayısı')
  )









