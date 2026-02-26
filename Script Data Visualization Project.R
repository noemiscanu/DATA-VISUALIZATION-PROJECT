# ============================================================
# EU Digital Divide — Urban vs Rural Internet Access
# Project: Urban–Rural Internet Convergence in the EU (2006–2025)
# Author: Noemi Scanu
# ============================================================

rm(list = ls())

# ----------------------------
# 1) Libraries
# ----------------------------
library(tidyverse)
library(readr)
library(janitor)
library(countrycode)
library(stringr)

# ----------------------------
# 2) Import and initial cleaning
# ----------------------------

raw_path <- "~/Downloads/isoc_ci_in_h__custom_20214599_linear.csv"

df <- read_csv(raw_path, show_col_types = FALSE) %>%
  clean_names() %>%
  rename(
    year  = time_period,
    value = obs_value
  ) %>%
  mutate(
    year  = as.integer(year),
    value = as.numeric(value),
    geo   = str_squish(as.character(geo))
  ) %>%
  filter(year >= 2006)   # analysis period

# ----------------------------
# 3) Keep only Urban and Rural households
# ----------------------------

df <- df %>%
  filter(hhtyp %in% c("Households located in cities",
                      "Households located in rural areas")) %>%
  mutate(area = case_when(
    hhtyp == "Households located in cities" ~ "urban",
    hhtyp == "Households located in rural areas" ~ "rural"
  ))

# ----------------------------
# 4) Country codes and EU27 selection
# ----------------------------

df <- df %>%
  mutate(
    geo_for_cc = case_when(
      geo == "Kosovo*" ~ NA_character_,
      geo == "Türkiye" ~ "Turkey",
      TRUE ~ geo
    ),
    iso2 = countrycode(geo_for_cc,
                       origin = "country.name",
                       destination = "iso2c")
  )

# EU27 (current members)
eu27_iso2 <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR",
               "HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK",
               "SI","ES","SE")

df_eu <- df %>%
  filter(iso2 %in% eu27_iso2) %>%
  mutate(country = countrycode(iso2, "iso2c", "country.name"))

# ----------------------------
# 5) Create country-year dataset + digital gap
# ----------------------------

data_wide <- df_eu %>%
  select(country, iso2, year, area, value) %>%
  pivot_wider(names_from = area, values_from = value) %>%
  mutate(
    digital_gap = urban - rural   # positive = urban advantage
  )

# ----------------------------
# 6) EU average trend (Visualization 1)
# ----------------------------

trend_eu <- data_wide %>%
  group_by(year) %>%
  summarise(
    mean_urban = mean(urban, na.rm = TRUE),
    mean_rural = mean(rural, na.rm = TRUE),
    mean_gap   = mean(digital_gap, na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------
# 7) Convergence analysis (2006 vs 2025)
# ----------------------------

year_start <- min(data_wide$year, na.rm = TRUE)
year_end   <- max(data_wide$year, na.rm = TRUE)

change_df_abs <- data_wide %>%
  filter(year %in% c(year_start, year_end)) %>%
  mutate(abs_gap = abs(digital_gap)) %>%
  select(country, iso2, year, abs_gap) %>%
  pivot_wider(names_from = year,
              values_from = abs_gap,
              names_prefix = "gap_") %>%
  rename(
    gap_2006 = !!paste0("gap_", year_start),
    gap_2025 = !!paste0("gap_", year_end)
  ) %>%
  filter(!is.na(gap_2006), !is.na(gap_2025)) %>%
  mutate(
    gap_2006   = round(gap_2006, 2),
    gap_2025   = round(gap_2025, 2),
    gap_change = round(gap_2006 - gap_2025, 2)  # >0 = reduction
  )

# ----------------------------
# 8) Highlight selection (Visualization 2 – slope chart)
# ----------------------------

# Countries chosen for storytelling purposes:
# - Poland (strong improvement)
# - Cyprus (strong convergence)
# - Greece (persistent gap)
# - Portugal (stagnation)
# - Germany (widening)

manual_highlight <- c("PL","CY","GR","PT","DE")

slope_wide_viz <- change_df_abs %>%
  mutate(
    highlight = if_else(iso2 %in% manual_highlight,
                        "highlight",
                        "other"),
    label = if_else(highlight == "highlight",
                    country,
                    NA_character_)
  )

# ----------------------------
# 9) Export datasets for visualization tools
# ----------------------------

dir.create("output", showWarnings = FALSE)

# EU trend (line chart)
write_csv(trend_eu,
          "output/viz1_eu_trend.csv")

# Country convergence (slope chart)
write_csv(slope_wide_viz,
          "output/viz2_slope_2006_2025.csv")

message("✅ Analysis complete.")
message("Latest year in dataset: ", year_end)