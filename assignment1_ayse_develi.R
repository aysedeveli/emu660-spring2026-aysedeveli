

#  3c ve 3d

my_first      <- "Ayse"      
my_birth_year <- 2003        


compute_stats <- function(x) {
  list(
    mean   = mean(x,   na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    var    = var(x,    na.rm = TRUE),
    iqr    = IQR(x,    na.rm = TRUE),
    min    = min(x,    na.rm = TRUE),
    max    = max(x,    na.rm = TRUE)
  )
}


for (col in names(mtcars)) {
  cat("\n── Sütun:", col, "──\n")
  stats <- compute_stats(mtcars[[col]])
  print(stats)
}


sapply_result <- sapply(mtcars, compute_stats)
print(sapply_result)

# ── apply ile (matrix üzerinde, margin=2 → sütunlar) ─────────
apply_result <- apply(as.matrix(mtcars), 2, compute_stats)
print(apply_result)




library(dslabs)


data("polls_us_election_2016")
polls <- polls_us_election_2016


k <- (nchar(my_first) + my_birth_year) %% 15 + 8
cat("k =", k, "\n")


if (k %% 2 == 0) {
  print(head(polls, k))
} else {
  print(tail(polls, k))
}


# Toplam NA sayısı
cat("\nToplam NA sayısı:", sum(is.na(polls)), "\n")

# Sütun bazında NA sayısı azalan şekilde
na_per_col <- sort(colSums(is.na(polls)), decreasing = TRUE)
print(head(na_per_col, 8))


new_data <- polls

for (col in names(new_data)) {
  if (is.numeric(new_data[[col]])) {
    new_data[[col]][is.na(new_data[[col]])] <- my_birth_year + k
  } else {
    na_idx <- is.na(new_data[[col]])
    new_data[[col]][na_idx] <- paste0(my_first, "_", k)
  }
}


if (k %% 2 == 0) {
  print(head(new_data, k))
} else {
  print(tail(new_data, k))
}


cat("\nYeni veri setindeki toplam NA:", sum(is.na(new_data)), "\n")
cat("anyNA(new_data):", anyNA(new_data), "\n")   
