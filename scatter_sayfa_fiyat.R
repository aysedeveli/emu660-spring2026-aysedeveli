
# EMU660 - Sayfa Sayısı ile Fiyat Arasındaki İlişki
# Scatter Plot + Hexbin (Yoğunluk Haritası)


library(tidyverse)
library(scales)
library(patchwork)

# Veri
df <- read_csv("kitapyurdu_2000_temiz.csv")

data <- df %>% select(sayfa_sayisi, fiyat) %>% drop_na()

renk  <- "#2E86AB"
vurgu <- "#E84855"

# Korelasyon
r_val <- cor(data$sayfa_sayisi, data$fiyat)
model <- lm(fiyat ~ sayfa_sayisi, data = data)
coef_b0 <- round(coef(model)[1], 1)
coef_b1 <- round(coef(model)[2], 2)

cat(sprintf("Pearson r = %.3f\n", r_val))
cat(sprintf("Regresyon: fiyat = %.2f * sayfa_sayisi + %.1f\n", coef_b1, coef_b0))

# Sol: Scatter + regresyon doğrusu
p1 <- ggplot(data, aes(x = sayfa_sayisi, y = fiyat)) +
  geom_point(color = renk, alpha = 0.25, size = 1.5) +
  geom_smooth(method = "lm", color = vurgu, linewidth = 1.2, se = TRUE, fill = vurgu, alpha = 0.1) +
  annotate("label",
           x = max(data$sayfa_sayisi) * 0.98,
           y = min(data$fiyat) * 1.5,
           label = paste0("Pearson r = ", round(r_val, 3),
                          "\np < 0.001",
                          "\nn = ", format(nrow(data), big.mark = ".")),
           hjust = 1, vjust = 0, size = 3.5,
           fill = "white", color = "gray30", label.size = 0.3) +
  scale_y_continuous(labels = label_number(suffix = " TL")) +
  labs(
    title    = "Scatter Plot + Regresyon Doğrusu",
    subtitle = paste0("fiyat = ", coef_b1, " × sayfa_sayisi + ", coef_b0),
    x = "Sayfa Sayısı",
    y = "Fiyat (TL)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "gray40", size = 9),
    panel.grid.minor = element_blank()
  )

# Sağ: Hexbin yoğunluk haritası
p2 <- ggplot(data, aes(x = sayfa_sayisi, y = fiyat)) +
  geom_hex(bins = 35, color = "white", linewidth = 0.1) +
  geom_smooth(method = "lm", color = "navy", linewidth = 1.2,
              linetype = "dashed", se = FALSE) +
  scale_fill_gradient(low = "#FFF5EB", high = "#D94701",
                      name = "Kitap\nSayısı") +
  scale_y_continuous(labels = label_number(suffix = " TL")) +
  labs(
    title    = "Yoğunluk Haritası (Hexbin)",
    subtitle = "Renk koyulaştıkça o bölgede daha fazla kitap var",
    x = "Sayfa Sayısı",
    y = "Fiyat (TL)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "gray40", size = 9),
    panel.grid.minor = element_blank()
  )

# Birleştir
panel <- p1 + p2 +
  plot_annotation(
    title    = "Sayfa Sayısı ile Fiyat Arasındaki İlişki",
    subtitle = paste0("Pearson r = ", round(r_val, 3), "  |  n = ", format(nrow(data), big.mark = ".")),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 11)
    )
  )

panel


