# EMU660 - Kitapyurdu EDA: Dağılım Analizleri
# Fiyat, Puan, Yorum Sayısı, Satın Alınma Sayısı

install.packages("patchwork")
install.packages("tidyverse")
install.packages("scales")


library(tidyverse)
library(scales)
library(patchwork)

# Veri yükleme
df <- read_csv("kitapyurdu_2000_temiz.csv")

# Renk paleti
renk <- "#2E86AB"
vurgu <- "#E84855"

# Ortak tema
tema <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    axis.title    = element_text(size = 10),
    panel.grid.minor = element_blank()
  )


# 1. FİYAT


# Histogram
p_fiyat_hist <- df %>%
  filter(!is.na(fiyat)) %>%
  ggplot(aes(x = fiyat)) +
  geom_histogram(binwidth = 20, fill = renk, color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = median(fiyat, na.rm = TRUE)),
             color = vurgu, linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = median(df$fiyat, na.rm = TRUE) + 15,
           y = Inf, vjust = 2, hjust = 0,
           label = paste0("Medyan: ", round(median(df$fiyat, na.rm = TRUE), 1), " TL"),
           color = vurgu, size = 3.5) +
  scale_x_continuous(labels = label_number(suffix = " TL")) +
  labs(title = "Fiyat Dağılımı",
       subtitle = "Her çubuk 20 TL aralığını temsil eder",
       x = "Fiyat (TL)", y = "Kitap Sayısı") +
  tema

# Boxplot
p_fiyat_box <- df %>%
  filter(!is.na(fiyat)) %>%
  ggplot(aes(x = fiyat, y = "")) +
  geom_boxplot(fill = renk, alpha = 0.7, outlier.color = vurgu,
               outlier.alpha = 0.5, width = 0.4) +
  scale_x_continuous(labels = label_number(suffix = " TL")) +
  labs(title = "Fiyat - Boxplot",
       x = "Fiyat (TL)", y = NULL) +
  tema


# 2. PUAN


p_puan_hist <- df %>%
  filter(!is.na(puan)) %>%
  ggplot(aes(x = puan)) +
  geom_histogram(binwidth = 0.5, fill = renk, color = "white", alpha = 0.85) +
  scale_x_continuous(breaks = 1:5) +
  labs(title = "Puan Dağılımı",
       subtitle = "1–5 arası değerlendirme",
       x = "Puan", y = "Kitap Sayısı") +
  tema

p_puan_box <- df %>%
  filter(!is.na(puan)) %>%
  ggplot(aes(x = puan, y = "")) +
  geom_boxplot(fill = renk, alpha = 0.7, outlier.color = vurgu,
               outlier.alpha = 0.5, width = 0.4) +
  scale_x_continuous(breaks = 1:5) +
  labs(title = "Puan - Boxplot",
       x = "Puan", y = NULL) +
  tema


# 3. YORUM SAYISI (log ölçek - sağa çarpık dağılım)


p_yorum_hist <- df %>%
  filter(!is.na(yorum_sayisi), yorum_sayisi > 0) %>%
  ggplot(aes(x = yorum_sayisi)) +
  geom_histogram(bins = 40, fill = renk, color = "white", alpha = 0.85) +
  scale_x_log10(labels = label_comma()) +
  geom_vline(aes(xintercept = median(yorum_sayisi, na.rm = TRUE)),
             color = vurgu, linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = median(df$yorum_sayisi, na.rm = TRUE) * 1.5,
           y = Inf, vjust = 2, hjust = 0,
           label = paste0("Medyan: ", median(df$yorum_sayisi, na.rm = TRUE)),
           color = vurgu, size = 3.5) +
  labs(title = "Yorum Sayısı Dağılımı",
       subtitle = "Log ölçek (sağa çarpık dağılım)",
       x = "Yorum Sayısı (log)", y = "Kitap Sayısı") +
  tema

p_yorum_box <- df %>%
  filter(!is.na(yorum_sayisi), yorum_sayisi > 0) %>%
  ggplot(aes(x = yorum_sayisi, y = "")) +
  geom_boxplot(fill = renk, alpha = 0.7, outlier.color = vurgu,
               outlier.alpha = 0.4, width = 0.4) +
  scale_x_log10(labels = label_comma()) +
  labs(title = "Yorum Sayısı - Boxplot",
       x = "Yorum Sayısı (log)", y = NULL) +
  tema


# 4. SATIN ALINMA SAYISI (log ölçek)


p_satin_hist <- df %>%
  filter(!is.na(satin_alinma_sayisi), satin_alinma_sayisi > 0) %>%
  ggplot(aes(x = satin_alinma_sayisi)) +
  geom_histogram(bins = 40, fill = renk, color = "white", alpha = 0.85) +
  scale_x_log10(labels = label_comma()) +
  geom_vline(aes(xintercept = median(satin_alinma_sayisi, na.rm = TRUE)),
             color = vurgu, linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = median(df$satin_alinma_sayisi, na.rm = TRUE) * 1.5,
           y = Inf, vjust = 2, hjust = 0,
           label = paste0("Medyan: ", format(median(df$satin_alinma_sayisi, na.rm = TRUE), big.mark = ".")),
           color = vurgu, size = 3.5) +
  labs(title = "Satın Alınma Sayısı Dağılımı",
       subtitle = "Log ölçek (sağa çarpık dağılım)",
       x = "Satın Alınma Sayısı (log)", y = "Kitap Sayısı") +
  tema

p_satin_box <- df %>%
  filter(!is.na(satin_alinma_sayisi), satin_alinma_sayisi > 0) %>%
  ggplot(aes(x = satin_alinma_sayisi, y = "")) +
  geom_boxplot(fill = renk, alpha = 0.7, outlier.color = vurgu,
               outlier.alpha = 0.4, width = 0.4) +
  scale_x_log10(labels = label_comma()) +
  labs(title = "Satın Alınma Sayısı - Boxplot",
       x = "Satın Alınma Sayısı (log)", y = NULL) +
  tema


# 5. BİRLEŞTİRİLMİŞ PANEL (patchwork)


panel <- (p_fiyat_hist | p_fiyat_box) /
  (p_puan_hist  | p_puan_box)  /
  (p_yorum_hist | p_yorum_box) /
  (p_satin_hist | p_satin_box) +
  plot_annotation(
    title   = "Kitapyurdu Veri Seti — Temel Dağılım Analizleri",
    subtitle = "Sol: Histogram | Sağ: Boxplot",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "gray40", size = 11)
    )
  )

ggsave("dagilim_analizi.png", panel, width = 12, height = 16, dpi = 150)
print("Grafik kaydedildi: dagilim_analizi.png")


# 6. ÖZET İSTATİSTİKLER 


ozet <- df %>%
  select(fiyat, puan, yorum_sayisi, satin_alinma_sayisi) %>%
  pivot_longer(everything(), names_to = "degisken", values_to = "deger") %>%
  group_by(degisken) %>%
  summarise(
    n        = sum(!is.na(deger)),
    ortalama = round(mean(deger, na.rm = TRUE), 2),
    medyan   = round(median(deger, na.rm = TRUE), 2),
    std      = round(sd(deger, na.rm = TRUE), 2),
    min      = round(min(deger, na.rm = TRUE), 2),
    maks     = round(max(deger, na.rm = TRUE), 2)
  )

print(ozet)








