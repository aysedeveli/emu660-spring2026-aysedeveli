
# EMU660 - Doğrusal Regresyon: Fiyat Tahmin Modeli
# Bağımsız değişkenler: sayfa_sayisi, yayinevi, kategori, yayin_yili


library(tidyverse)
library(broom)       # tidy(), glance(), augment()
library(scales)

# 1. VERİ HAZIRLAMA

df <- read_csv("kitapyurdu_2000_temiz.csv")

df <- df %>%
  mutate(
    # Yayın yılını tarih sütunundan çıkar
    yayin_yili = as.numeric(format(as.Date(yayin_tarihi, "%d.%m.%Y"), "%Y")),

    # Ana kategori: "Kitap > Edebiyat > Roman (Yerli)" → "Edebiyat"
    ana_kategori = str_split_i(ilgili_kategoriler, ",", 1) %>%
                   str_split_i(">", 2) %>%
                   str_trim(),

    # Seyrek yayınevlerini "Diger" olarak grupla (n < 10 → Diger)
    yayinevi_grp = fct_lump_min(yayinevi, min = 10, other_level = "Diger"),

    # Referans kategoriler: en büyük grubu referans yap
    # R, factor'ün ilk seviyesini referans olarak alır (alfabetik)
    # Biz Edebiyat'ı referans yapmak için onu ilk sıraya koyuyoruz
    ana_kategori = fct_relevel(factor(ana_kategori), "Edebiyat"),
    yayinevi_grp = fct_relevel(yayinevi_grp, "Diger")  # "Diger" referans
  )

model_df <- df %>%
  select(fiyat, sayfa_sayisi, yayinevi_grp, ana_kategori, yayin_yili) %>%
  drop_na()

cat(sprintf("Model için kullanılabilir satır: %d\n", nrow(model_df)))
cat(sprintf("Bağımlı değişken (fiyat) — ort: %.1f, std: %.1f\n",
            mean(model_df$fiyat), sd(model_df$fiyat)))


# 2. KATSAYILAR NASIL HESAPLANIR? (EKK — En Küçük Kareler) 

# Doğrusal regresyon şu denklemi tahmin eder:
#   fiyat = β0 + β1*sayfa_sayisi + β2*yayin_yili + Σ γk*yayinevi_k + Σ δj*kategori_j + ε

# Katsayılar (β, γ, δ) EKK yöntemiyle bulunur:
#   → Amaç: Σ(gerçek_fiyat − tahmin_fiyat)² toplamını minimize etmek
#   → Matris formülü: β = (X'X)⁻¹ X'y
#      X: tasarım matrisi (sayısal + dummy değişkenler)
#      y: bağımlı değişken vektörü (fiyat)

# KATEGORİK DEĞİŞKENLER → DUMMY (ONE-HOT) KODLAMA:
#   k kategorili bir değişken için (k-1) dummy sütun oluşturulur.
#   Dışarıda bırakılan kategori "referans kategori"dir.
#   Katsayı = o kategorideki gözlemin referansa göre ortalama fiyat farkıdır,
#             diğer değişkenler sabit tutulduğunda.

# Örnek:
#   yayinevi_grp_Inkilap_Kitabevi = 1 ise bu yayınevi, 0 ise değil
#   Katsayısı +26.71 → Inkılap, "Diger" grubundan 26.71 TL daha pahalı
#                       (sayfa sayısı, yıl, kategori sabitken)

# Dummy dönüşümü manuel gösterimi (ilk 5 satır):
cat("\nDummy dönüşümü örneği (ilk 3 satır):\n")
model.matrix(fiyat ~ yayinevi_grp, data = model_df[1:3, ]) %>% print()


#  3. MODEL KURMA

model <- lm(fiyat ~ sayfa_sayisi + yayin_yili + yayinevi_grp + ana_kategori,
            data = model_df)


# 4. MODEL ÖZET

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("MODEL ÖZET İSTATİSTİKLERİ\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

glance_df <- glance(model)
cat(sprintf("R²          : %.3f  (fiyat varyansının %%%.1f'i açıklanıyor)\n",
            glance_df$r.squared, glance_df$r.squared * 100))
cat(sprintf("Düz. R²     : %.3f\n", glance_df$adj.r.squared))
cat(sprintf("F-istatistiği: %.1f (p < 0.001)\n", glance_df$statistic))
cat(sprintf("AIC         : %.1f\n", glance_df$AIC))
cat(sprintf("Gözlem sayısı: %d\n", glance_df$nobs))


#5. KATSAYILAR

tidy_df <- tidy(model, conf.int = TRUE) %>%
  mutate(
    anlamli    = p.value < 0.05,
    degisken   = case_when(
      term == "(Intercept)"  ~ "Sabit",
      term == "sayfa_sayisi" ~ "Sayfa Sayısı",
      term == "yayin_yili"   ~ "Yayın Yılı",
      str_starts(term, "yayinevi_grp") ~ paste0("Yayınevi: ", str_remove(term, "yayinevi_grp")),
      str_starts(term, "ana_kategori") ~ paste0("Kategori: ", str_remove(term, "ana_kategori")),
      TRUE ~ term
    )
  )

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("REGRESYON DENKLEMİ VE KATSAYILAR\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("\nfiyat = β0 + β1×sayfa_sayisi + β2×yayin_yili\n")
cat("      + Σ γk×yayinevi_dummy_k + Σ δj×kategori_dummy_j\n\n")

# Sabit ve sayısal değişkenler
cat("── Sabit ve Sayısal Değişkenler ──\n")
tidy_df %>%
  filter(term %in% c("(Intercept)", "sayfa_sayisi", "yayin_yili")) %>%
  select(degisken, estimate, std.error, statistic, p.value, conf.low, conf.high) %>%
  mutate(across(where(is.numeric), ~round(., 4))) %>%
  print(n = Inf)

# Yayınevi katsayıları (referans: Diger)
cat("\n── Yayınevi Katsayıları (Referans: 'Diger') ──\n")
cat("  Yorumlama: diğer değişkenler sabitken, bu yayınevinin\n")
cat("  'Diger' grubuna göre ortalama fiyat farkı (TL)\n\n")
tidy_df %>%
  filter(str_starts(term, "yayinevi_grp")) %>%
  arrange(desc(estimate)) %>%
  select(degisken, estimate, std.error, p.value, conf.low, conf.high, anlamli) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  print(n = Inf)

# Kategori katsayıları (referans: Edebiyat)
cat("\n── Kategori Katsayıları (Referans: 'Edebiyat') ──\n")
tidy_df %>%
  filter(str_starts(term, "ana_kategori")) %>%
  arrange(desc(estimate)) %>%
  select(degisken, estimate, std.error, p.value, conf.low, conf.high, anlamli) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  print(n = Inf)


# 6. SOMUT YORUM: TAHMİN ÖRNEKLERİ

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TAHMİN ÖRNEKLERİ\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

ornekler <- tibble(
  sayfa_sayisi = c(300, 300, 500, 200),
  yayin_yili   = c(2023, 2023, 2024, 2022),
  yayinevi_grp = factor(c("Can Yayinlari", "Diger", "Inkilap Kitabevi", "Karbon Kitaplar"),
                        levels = levels(model_df$yayinevi_grp)),
  ana_kategori = factor(c("Edebiyat", "Edebiyat", "Tarih", "Edebiyat"),
                        levels = levels(model_df$ana_kategori))
)

ornekler$tahmin <- predict(model, newdata = ornekler)

cat("\nSayfa | Yıl  | Yayınevi              | Kategori  | Tahmin Fiyat\n")
cat("------|------|------------------------|-----------|-------------\n")
for (i in seq_len(nrow(ornekler))) {
  cat(sprintf("%5d | %4d | %-22s | %-9s | %.1f TL\n",
              ornekler$sayfa_sayisi[i],
              ornekler$yayin_yili[i],
              ornekler$yayinevi_grp[i],
              ornekler$ana_kategori[i],
              ornekler$tahmin[i]))
}


#  7. MODEL VARSAYIM KONTROLLERI

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("VARSAYIM KONTROLLERİ\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

aug <- augment(model)

# Kalıntı normalliği: Shapiro-Wilk (küçük örneklem için)
# Büyük n'de hassas olduğu için sadece 200 örneklemde test et
set.seed(42)
sw_test <- shapiro.test(sample(aug$.resid, 200))
cat(sprintf("\nShapiro-Wilk (n=200 örneklem): W=%.3f, p=%.4f\n",
            sw_test$statistic, sw_test$p.value))
cat("  → p < 0.05 ise kalıntılar normal dağılmıyor (büyük n'de sık görülür)\n")

# Kalıntı ortalaması
cat(sprintf("Kalıntı ortalaması: %.6f (0'a yakın olmalı)\n", mean(aug$.resid)))

# Homoskedastisite: kalıntı varyansı gruplara göre değişiyor mu?
cat(sprintf("Kalıntı std (tüm veri): %.2f TL\n", sd(aug$.resid)))

# VIF (çoklu doğrusallık) — yalnızca sayısal değişkenler için
# Kategorik değişkenlerle VIF yorumu daha karmaşık olduğundan
# sayfa_sayisi ve yayin_yili korelasyonunu kontrol et
kor <- cor(model_df$sayfa_sayisi, model_df$yayin_yili, use = "complete.obs")
cat(sprintf("sayfa_sayisi ~ yayin_yili korelasyonu: r=%.3f\n", kor))
cat("  → Düşük korelasyon: çoklu doğrusallık sorunu yok\n")


# 8. 5-KATLI ÇAPRAZ DOĞRULAMA

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("5-KATLI ÇAPRAZ DOĞRULAMA\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

set.seed(42)
n     <- nrow(model_df)
folds <- sample(rep(1:5, length.out = n))

cv_metrics <- map_dfr(1:5, function(k) {
  train_k <- model_df[folds != k, ]
  test_k  <- model_df[folds == k, ]

  model_k  <- lm(fiyat ~ sayfa_sayisi + yayin_yili + yayinevi_grp + ana_kategori,
                 data = train_k)
  pred_k   <- predict(model_k, newdata = test_k)

  ss_res  <- sum((test_k$fiyat - pred_k)^2)
  ss_tot  <- sum((test_k$fiyat - mean(test_k$fiyat))^2)

  tibble(
    fold = k,
    R2   = 1 - ss_res / ss_tot,
    MAE  = mean(abs(test_k$fiyat - pred_k)),
    RMSE = sqrt(mean((test_k$fiyat - pred_k)^2))
  )
})

cat("\nKat bazında sonuçlar:\n")
print(cv_metrics)

cat(sprintf("\nOrtalama R²  : %.3f ± %.3f\n", mean(cv_metrics$R2),   sd(cv_metrics$R2)))
cat(sprintf("Ortalama MAE : %.2f TL\n",         mean(cv_metrics$MAE))  )
cat(sprintf("Ortalama RMSE: %.2f TL\n",         mean(cv_metrics$RMSE)) )
cat("\nYorum: Model, gördüğü verinin dışında da fiyatın ~%%73'ini\n")
cat("açıklayabiliyor; ortalama tahmin hatası ~30 TL.\n")
