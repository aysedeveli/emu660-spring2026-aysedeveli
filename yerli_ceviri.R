
# EMU660 - Yerli ve Çeviri Kitap Karşılaştırması
# Fiyat, Puan, Satın Alınma Sayısı — Violin + Boxplot


library(tidyverse)
library(scales)
library(patchwork)

# Veri
df <- read_csv("kitapyurdu_2000_temiz.csv")

renk_yerli  <- "#2E86AB"
renk_ceviri <- "#E84855"

# Yerli / Çeviri etiketleme
data <- df %>%
  mutate(tur = case_when(
    str_detect(ilgili_kategoriler, "Yerli")  ~ "Yerli",
    str_detect(ilgili_kategoriler, "Çeviri") ~ "Çeviri",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(tur)) %>%
  mutate(tur = factor(tur, levels = c("Yerli", "Çeviri")))

cat("Dağılım:\n")
print(table(data$tur))

# İstatistiksel testler 
for (col in c("fiyat", "puan", "satin_alinma_sayisi")) {
  yerli  <- data %>% filter(tur == "Yerli")  %>% pull(!!sym(col)) %>% na.omit()
  ceviri <- data %>% filter(tur == "Çeviri") %>% pull(!!sym(col)) %>% na.omit()
  test   <- wilcox.test(yerli, ceviri, alternative = "two.sided")
  cat(sprintf("%s: W = %.0f, p = %.4f\n", col, test$statistic, test$p.value))
}

#  Ortak violin+boxplot fonksiyonu
violin_box <- function(data, col, ylabel, title, log_scale = FALSE, pval_label = "") {

  p <- ggplot(data, aes(x = tur, y = .data[[col]], fill = tur, color = tur)) +
    geom_violin(alpha = 0.3, trim = TRUE, linewidth = 0) +
    geom_boxplot(width = 0.25, alpha = 0.85, outlier.alpha = 0.2,
                 outlier.size = 1.5,
                 color = "white",
                 medianprops = list(color = "white", linewidth = 2)) +
    # Medyan etiketleri
    stat_summary(fun = median, geom = "text",
                 aes(label = if (log_scale)
                       scales::comma(round(..y..))
                     else
                       round(..y.., 1)),
                 vjust = -0.6, size = 3.2, fontface = "bold", color = "#222222") +
    scale_fill_manual(values  = c("Yerli" = renk_yerli, "Çeviri" = renk_ceviri)) +
    scale_color_manual(values = c("Yerli" = renk_yerli, "Çeviri" = renk_ceviri)) +
    labs(title = title,
         subtitle = paste0("Mann-Whitney U: ", pval_label),
         x = NULL, y = ylabel) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold", size = 12),
      plot.subtitle   = element_text(color = "gray40", size = 9),
      legend.position = "none",
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(size = 11)
    )

  if (log_scale) {
    p <- p + scale_y_log10(labels = label_comma())
  } else {
    p <- p + scale_y_continuous(labels = if (col == "fiyat") label_number(suffix = " TL") else label_comma())
  }
  p
}

# 3 grafik
p_fiyat <- violin_box(data, "fiyat",              "Fiyat (TL)",          "Fiyat",  FALSE, "p < 0.001 ***")
p_puan  <- violin_box(data, "puan",               "Puan",                "Puan",   FALSE, "p < 0.001 ***")
p_satis <- violin_box(data, "satin_alinma_sayisi", "Satın Alınma (log)", "Satış",  TRUE,  "p = 0.288 (ns)")

# Birleştir
n_yerli  <- sum(data$tur == "Yerli")
n_ceviri <- sum(data$tur == "Çeviri")

panel <- p_fiyat + p_puan + p_satis +
  plot_annotation(
    title    = "Yerli ve Çeviri Kitap Karşılaştırması",
    subtitle = paste0("Yerli: n = ", n_yerli, "  |  Çeviri: n = ", n_ceviri),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 11)
    )
  )

panel


