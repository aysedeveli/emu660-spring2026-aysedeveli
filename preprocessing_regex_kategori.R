
# EMU660 - Preprocessing: Purchase Count & Category Extraction


library(tidyverse)

df <- read_csv("kitapyurdu_2000_temiz.csv")

1. SATIN ALINMA SAYISI — Regex ile sayı çekme

# Ham veri: "Bu üründen 78.854 adet satın alınmıştır."
# Hedef  : 78854 (sayısal)


df <- df %>%
  mutate(
    satin_alinma_sayisi_temiz = str_extract(
      satin_alinma_sayisi,
      "(?<=Bu üründen )[\\d.]+"   
    ) %>%
      str_remove_all("\\.") %>%   
      as.numeric()
  )


df %>%
  select(satin_alinma_sayisi, satin_alinma_sayisi_temiz) %>%
  drop_na() %>%
  head(5) %>%
  print()


#2. KATEGORİ ÇIKARIMI 

df <- df %>%
  mutate(
    ana_kategori = str_split_i(ilgili_kategoriler, ",", 1) %>%  # İlk kategori yolu
                   str_split_i(">", 2) %>%                       # 2. breadcrumb seviyesi
                   str_trim()                                     # Baştaki/sondaki boşluk temizle
  )

# Kontrol
df %>%
  select(ilgili_kategoriler, ana_kategori) %>%
  drop_na() %>%
  head(5) %>%
  print()

# Kategori dağılımı
cat("\nKategori dağılımı:\n")
print(table(df$ana_kategori, useNA = "ifany"))
