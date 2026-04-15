install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggrepel")
install.packages("dslabs")

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(ggrepel)
library(dslabs)

load("myemu_data.RData")

unique_companies <- length(unique(myemu_data$work_company))
print(unique_companies)

unique_industries <- lenght(unique(myemu_data$work_industry))
print(unique_industries)

jobs_per_graduate <- myemu_data |> group_by(student_id) |> summarise(job_count = n())
print(mean(jobs_per_graduate$job_count))
print(max(jobs_per_graduate$job_count))

top10_companies <- myemu_data |>
  count(work_company) |>
  arrange(desc(n)) |>
  slice_head(n = 10)
print(top10_companies)



myemu_data <- myemu_data |> mutate(graduation_year = as.factor(year(dmy(graduation_date))))

myemu_data |>
  distinct(student_id, keep_all = TRUE) |>
  group_by(graduation_month) |>
  summarise(
    count_graduates = n(),
    avg_cgpa = mean(graduation_cgpa),
    median_cgpa = median(graduation_cgpa)
  )

myemu_data |>
  distinct(student_id, .keep_all = TRUE) |>
  ggplot(aes(x = graduation_month, y = graduation_cgpa, fill = graduation_month)) +
  geom_boxplot() +
  labs(title = "Boxplot of Graduation CGPA by Month",
       x = "Graduation Month",
       y = "Graduation CGPA") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





myemu_data <- myemu_data |>
  mutate(graduation_year = as.factor(year(dmy(graduation_date))))
myemu_data |>
  distinct(student_id, .keep_all = TRUE) |>
  group_by(graduation_year) |>
  summarise(
    count_graduates = n(),
    median_cgpa = median(graduation_cgpa),
    min_cgpa = min(graduation_cgpa),
    max_cgpa = max(graduation_cgpa)
  )

myemu_data |>
  distinct(student_id, .keep_all = TRUE) |>
  ggplot(aes(x = graduation_year, fill = graduation_year)) +
  geom_bar() +
  labs(title = "Number of Graduates by Year",
       x = "Graduation Year",
       y = "Number of Graduates") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

myemu_data |>
  distinct(student_id, .keep_all = TRUE) |>
  ggplot(aes(x = graduation_cgpa)) +
  geom_density(fill = "pink", alpha = 0.5) +
  facet_wrap(~ graduation_year) +
  labs(title = "Density of Graduation CGPA by Year",
       x = "Graduation CGPA",
       y = "Density") +
  theme_minimal()

top8_industries <- myemu_data |>
  count(work_industry) |>
  slice_max(n, n = 8)
print(top8_industries)

myemu_data |>
  filter(work_industry %in% top8_industries$work_industry) |>
  ggplot(aes(x = reorder(work_industry, graduation_cgpa, FUN = median), y = graduation_cgpa, fill = work_industry)) +
  geom_boxplot() +
  labs(title = "CGPA Distribution by Top 8 Industries (Ordered by Median)",
       x = "Industry",
       y = "Graduation CGPA") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
  legend.position = "none")


myemu_data <- myemu_data |>
  mutate(erasmus_factor = factor(ifelse(is.na(erasmus_participation), "Hayır", "Evet")))

erasmus_by_industry |>
  ggplot(aes(x = reorder(work_industry, erasmus_rate), y = erasmus_rate)) +
  geom_col(fill = "orange") +
  labs(title = "Erasmus Participation Rate by Industry (Top 6)",
       x = "Industry",
       y = "Erasmus Participation Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent)

) +
  theme_minimal()






myemu_data <- myemu_data |>
  mutate(erasmus_factor = factor(ifelse(is.na(erasmus_participation), "Hayır", "Evet")))
top6_industries <- myemu_data |>
  count(work_industry) |>
  slice_max(n, n = 6)
erasmus_by_industry <- myemu_data |>
  filter(work_industry %in% top6_industries$work_industry) |>
  group_by(work_industry) |>
  summarise(erasmus_rate = mean(erasmus_factor == "Evet")) |>
  arrange(desc(erasmus_rate))
print(erasmus_by_industry)

erasmus_by_industry |>
  ggplot(aes(x = reorder(work_industry, erasmus_rate), y = erasmus_rate)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Erasmus Participation Rate by Industry (Top 6)", x = "Industry",y = "Erasmus Participation Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent)
