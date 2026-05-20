
# EMU660 - Yayın Yılına Göre Kitap Fiyatlarının Seyri


library(tidyverse)
library(scales)
library(patchwork)

#  Veri
df <- read_csv("kitapyurdu_2000_temiz.csv")

df <- df %>%
  mutate(yayin_yili = as.numeric(format(as.Date(yayin_tarihi, "%d.%m.%Y"), "%Y")))

ozet <- df %>%
  filter(between(yayin_yili, 2011, 2025)) %>%
  group_by(yayin_yili) %>%
  summarise(
    n         = sum(!is.na(fiyat)),
    ort_fiyat = mean(fiyat, na.rm = TRUE),
    med_fiyat = median(fiyat, na.rm = TRUE),
    q25       = quantile(fiyat, 0.25, na.rm = TRUE),
    q75       = quantile(fiyat, 0.75, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  drop_na()

print(ozet)

# Üst grafik: Fiyat trendi
p_trend <- ggplot(ozet, aes(x = yayin_yili)) +

  # IQR bandı
  geom_ribbon(aes(ymin = q25, ymax = q75),
              fill = "#2E86AB", alpha = 0.15) +

  # Ortalama çizgisi
  geom_line(aes(y = ort_fiyat, color = "Ortalama"),
            linewidth = 2, lineend = "round") +
  geom_point(aes(y = ort_fiyat, color = "Ortalama"),
             size = 3.5, shape = 19) +

  # Medyan çizgisi
  geom_line(aes(y = med_fiyat, color = "Medyan"),
            linewidth = 1.8, linetype = "dashed") +
  geom_point(aes(y = med_fiyat, color = "Medyan"),
             size = 3, shape = 15) +

  # Değer etiketleri (ortalama)
  geom_text(aes(y = ort_fiyat, label = round(ort_fiyat, 0)),
            vjust = -1, size = 2.8, fontface = "bold", color = "#1F5C8B") +

  # Dikkat çeken noktalar
  annotate("label", x = 2021, y = 70,
           label = "2021\nDüşüş\n(94.7 TL)",
           size = 3, fill = "white", color = "#333333", label.size = 0.3) +
  annotate("segment", x = 2021, xend = 2021, y = 80, yend = 92,
           arrow = arrow(length = unit(0.2, "cm")), color = "gray50") +
  annotate("label", x = 2022, y = 195,
           label = "2022\nSıçrama\n(150 TL)",
           size = 3, fill = "white", color = "#333333", label.size = 0.3) +
  annotate("segment", x = 2022, xend = 2022, y = 186, yend = 154,
           arrow = arrow(length = unit(0.2, "cm")), color = "gray50") +
  annotate("label", x = 2024, y = 205,
           label = "2024\nZirve\n(157.5 TL)",
           size = 3, fill = "white", color = "#333333", label.size = 0.3) +
  annotate("segment", x = 2024, xend = 2024, y = 196, yend = 163,
           arrow = arrow(length = unit(0.2, "cm")), color = "gray50") +

  scale_color_manual(values = c("Ortalama" = "#1F5C8B", "Medyan" = "#E84855"),
                     name = NULL) +
  scale_x_continuous(breaks = 2011:2025) +
  scale_y_continuous(labels = label_number(suffix = " TL"),
                     limits = c(40, 230)) +
  labs(title    = "Yayın Yılına Göre Kitap Fiyatlarının Seyri (2011–2025)",
       subtitle = "Gölgeli alan: IQR (Q25–Q75) | Kesikli: Medyan | Düz: Ortalama",
       x = NULL, y = "Fiyat (TL)") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(color = "gray40", size = 9),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "top"
  )

#Alt grafik: Kitap sayısı
p_sayi <- ggplot(ozet, aes(x = yayin_yili, y = n)) +
  geom_col(fill = "#2E86AB", alpha = 0.75, width = 0.7) +
  geom_text(aes(label = n), vjust = -0.4, size = 3, color = "#333333") +
  scale_x_continuous(breaks = 2011:2025) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = "Yayın Yılı", y = "Kitap Sayısı") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
  )

# Birleştir
panel <- p_trend / p_sayi +
  plot_layout(heights = c(3, 1))

panel


