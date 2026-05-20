# EMU660 - Yayınevine Göre Ortalama Fiyat Karşılaştırması


library(tidyverse)
library(scales)

# Veri yükleme 
df <- read_csv("kitapyurdu_2000_temiz.csv")

# Parametreler
renk  <- "#2E86AB"
vurgu <- "#E84855"
min_kitap <- 10          # En az 10 kitabı olan yayınevleri

# Özet tablo
genel_ort <- mean(df$fiyat, na.rm = TRUE)

ozet <- df %>%
  group_by(yayinevi) %>%
  summarise(
    kitap_sayisi = sum(!is.na(fiyat)),
    ort_fiyat    = mean(fiyat, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(kitap_sayisi >= min_kitap) %>%
  mutate(
    yayinevi  = fct_reorder(yayinevi, ort_fiyat),
    renk_grup = if_else(ort_fiyat > genel_ort, "Ortalamanın üstü", "Ortalamanın altı")
  )



# Grafik
ggplot(ozet, aes(x = ort_fiyat, y = yayinevi, fill = renk_grup)) +
  geom_col(alpha = 0.88, width = 0.7) +

  
  geom_text(aes(label = paste0(round(ort_fiyat, 1), " TL  (n=", kitap_sayisi, ")")),
            hjust = -0.05, size = 3, color = "#333333") +

  
  geom_vline(xintercept = genel_ort, linetype = "dashed",
             color = "gray50", linewidth = 0.8) +
  annotate("text", x = genel_ort + 2, y = 1.5,
           label = paste0("Genel ort.\n", round(genel_ort, 1), " TL"),
           color = "gray40", size = 3, hjust = 0) +

 
  scale_fill_manual(values = c("Ortalamanın üstü" = vurgu,
                                "Ortalamanın altı"  = renk)) +

 
  scale_x_continuous(
    labels = label_number(suffix = " TL"),
    expand = expansion(mult = c(0, 0.22))
  ) +

  
  labs(
    title    = "Yayınevine Göre Ortalama Kitap Fiyatı",
    subtitle = paste0("En az ", min_kitap, " kitabı olan yayınevleri | n = ", nrow(ozet), " yayınevi"),
    x        = "Ortalama Fiyat (TL)",
    y        = NULL,
    fill     = NULL
  ) +

  
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "gray40", size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position = "bottom",
    axis.text.y     = element_text(size = 9)
  )

ggsave("yayinevi_fiyat.png", width = 11, height = 10, dpi = 150)
