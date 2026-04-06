# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#
# Hazard analysis of German Lotto 6/49 numbers
# Kaplan-Meier survival curves per lottery number
#
# PL - 2025 - V 0.0.1
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Notes -----------------------------------------------------------------------
# German Lotto 6/49: each draw picks 6 unique numbers from 1 to 49.
# We treat the gap between consecutive appearances of each number
# (measured in number of draws) as a survival time.
# "Event" = the number is drawn again.
# Numbers that have never re-appeared after their last draw are right-censored.


# Settings --------------------------------------------------------------------

set.seed(42)

file_lottery <- here::here("data", "lottery.xlsx")  # path to optional real data file
output_dir   <- here::here("output")                # where to save plots

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


# Libraries -------------------------------------------------------------------

library(here)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(survival)
library(survminer)


# 1) Data ---------------------------------------------------------------------

## 1.1 Read or generate lottery data ------------------------------------------

if (file.exists(file_lottery)) {

  # Real data: expects columns Date, N1, N2, N3, N4, N5, N6
  # Date column may be an Excel numeric date or a proper date string.
  library(openxlsx)
  df_raw <- openxlsx::read.xlsx(file_lottery)

  # Convert Excel serial date if needed
  if (is.numeric(df_raw$Date)) {
    df_raw$Date <- as.Date(df_raw$Date, origin = "1899-12-30")
  } else {
    df_raw$Date <- as.Date(df_raw$Date)
  }

} else {

  message("No lottery data file found – generating synthetic draws for demonstration.")

  n_draws <- 500
  dates   <- seq.Date(from = as.Date("2003-10-11"), by = "week", length.out = n_draws)

  draws <- t(replicate(n_draws, sort(sample(1:49, 6, replace = FALSE))))
  colnames(draws) <- paste0("N", 1:6)

  df_raw <- cbind(data.frame(Date = dates), as.data.frame(draws))

}


## 1.2 Build structured dataframe: Date + N1..N6 ------------------------------

df_draws <- df_raw %>%
  select(Date, N1, N2, N3, N4, N5, N6) %>%
  arrange(Date) %>%
  mutate(draw_id = row_number())

cat("Lottery draws loaded:", nrow(df_draws), "\n")
cat("Period:", format(min(df_draws$Date)), "–", format(max(df_draws$Date)), "\n")


# 2) Time since last draw (per number) ----------------------------------------

## Long format: one row per (draw, number)
df_long <- df_draws %>%
  pivot_longer(cols = N1:N6, names_to = "position", values_to = "number")

## For each number in 1:49, find the last draw_id in which it appeared
last_draw <- df_long %>%
  group_by(number) %>%
  summarise(last_draw_id = max(draw_id),
            last_date    = max(Date),
            .groups      = "drop")

## Merge with the full 1:49 set (some numbers may never have appeared)
df_last <- data.frame(number = 1:49) %>%
  left_join(last_draw, by = "number") %>%
  mutate(
    draws_since_last = max(df_draws$draw_id) - last_draw_id,
    ever_drawn       = !is.na(last_draw_id)
  )

cat("\nDraws since last appearance (top 10 most overdue):\n")
print(df_last %>% arrange(desc(draws_since_last)) %>% head(10))


# 3) Kaplan-Meier survival analysis -------------------------------------------
#
# For each number we build a sequence of inter-draw gaps:
#   gap = draw_id(k+1) - draw_id(k)  for every consecutive pair of appearances
#
# These are the "survival times" with event = 1 (number was drawn again).
# The final gap after the last appearance is right-censored (event = 0).

## Appearance list per number
df_appearances <- df_long %>%
  arrange(number, draw_id) %>%
  group_by(number) %>%
  mutate(
    gap   = lead(draw_id) - draw_id,   # gap to NEXT appearance
    event = as.integer(!is.na(gap))    # 1 if re-appeared, 0 if last obs (censored)
  ) %>%
  ungroup()

## Add the censored final observation for each number
## (gap from last appearance to end of data)
df_censored <- df_appearances %>%
  group_by(number) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  mutate(
    gap   = max(df_draws$draw_id) - draw_id,
    event = 0L
  )

## Combine: all completed gaps + one censored gap per number
df_survival <- bind_rows(
  df_appearances %>% filter(event == 1),
  df_censored
) %>%
  filter(gap >= 0)   # safety guard


# 3.1 Pooled KM curve (all numbers together) ----------------------------------

fit_pooled <- survfit(Surv(gap, event) ~ 1, data = df_survival)

p_pooled <- ggsurvplot(
  fit_pooled,
  data          = df_survival,
  title         = "Kaplan-Meier: pooled re-draw gap (all numbers 1–49)",
  xlab          = "Draws since last appearance",
  ylab          = "Probability of not being drawn",
  conf.int      = TRUE,
  ggtheme       = theme_minimal(),
  palette       = "#2E9FDF",
  surv.median.line = "hv"
)

ggsave(
  filename = file.path(output_dir, "km_pooled.png"),
  plot     = p_pooled$plot,
  width    = 10, height = 6, dpi = 150
)
cat("\nSaved: km_pooled.png\n")


# 3.2 Individual KM curves per number (faceted, 7×7 grid) --------------------

df_survival <- df_survival %>%
  mutate(number_label = paste0("Nr. ", number))

fit_per_number <- survfit(
  Surv(gap, event) ~ number_label,
  data = df_survival
)

p_per_number <- ggsurvplot_facet(
  fit        = fit_per_number,
  data       = df_survival,
  facet.by   = "number_label",
  title      = "Kaplan-Meier curves – re-draw gap per lottery number (1–49)",
  xlab       = "Draws since last appearance",
  ylab       = "P(not drawn)",
  conf.int   = FALSE,
  ggtheme    = theme_minimal(base_size = 7),
  nrow       = 7,
  ncol       = 7,
  short.panel.labs = TRUE
)

ggsave(
  filename = file.path(output_dir, "km_per_number.png"),
  plot     = p_per_number,
  width    = 18, height = 18, dpi = 150
)
cat("Saved: km_per_number.png\n")


# 3.3 Highlight overdue numbers (top 9 most overdue) --------------------------

top_overdue <- df_last %>%
  filter(ever_drawn) %>%
  arrange(desc(draws_since_last)) %>%
  slice(1:9) %>%
  pull(number)

df_overdue <- df_survival %>%
  filter(number %in% top_overdue)

fit_overdue <- survfit(
  Surv(gap, event) ~ number_label,
  data = df_overdue
)

p_overdue <- ggsurvplot(
  fit_overdue,
  data      = df_overdue,
  title     = "KM curves – top 9 most overdue numbers",
  xlab      = "Draws since last appearance",
  ylab      = "P(not drawn)",
  conf.int  = FALSE,
  legend    = "right",
  ggtheme   = theme_minimal()
)

ggsave(
  filename = file.path(output_dir, "km_overdue.png"),
  plot     = p_overdue$plot,
  width    = 12, height = 7, dpi = 150
)
cat("Saved: km_overdue.png\n")

cat("\nAnalysis complete. Plots saved to:", output_dir, "\n")
