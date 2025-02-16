# List of required packages
packages <- c("tidyverse", "readxl", "httr", "dplyr", "glue")

# Function to install and load missing packages
install_and_load <- function(pkg){
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Apply the function to the list of packages
lapply(packages, install_and_load)

# Function to download file
download_rom_file <- function(index, file_name) {
  endpoint <- jsonlite::read_json("https://arbetsformedlingen.se/rest/analysportalen/lev/sitevision")
  file_path <- glue::glue("data/raw/{file_name}.xlsx")
  
  download.file(
    paste0(endpoint[[index]]$properties$link),
    destfile = file_path,
    mode = "wb"
  )
}

# Download RoM data
download_rom_file(7, "af_data")

# Load data
df <- read_excel("C:/Users/henry/Documents/GitHub/geo-analysis/data/raw/af_data.xlsx",
                 sheet = "1) LEVERANTÖR")
df2 <- read_excel("C:/Users/henry/Documents/GitHub/geo-analysis/data/raw/af_data.xlsx",
                 sheet = "5) PLACERING_KOMMUN_LEVERANTÖR")

# Fuction for converting values to numeric
apply_numeric <- function(column) {
  column <- suppressWarnings(as.numeric(column))
}

# Clean data and create columns for total and market share
df <- df %>%
  # Filter for the relevant tjanst values and replace with "Rusta och Matcha"
  filter(tjanst %in% c("Rusta och matcha 2", "Kundval Rusta och Matcha")) %>%
  mutate(
    tjanst = "Rusta och Matcha",  # Combine both categories into "Rusta och Matcha"
    across(c(tjanstdelt_ny, tjanstdelt_pagaende, tjanstdelt_avslutat), apply_numeric)
  ) %>%
  # Group by leverantor, period, and the combined tjanst value
  group_by(leverantor, period, tjanst) %>%
  # Summing tjanstdelt_ny and tjanstdelt_pagaende across the new grouping
  summarise(
    tjanstdelt_ny = sum(tjanstdelt_ny, na.rm = TRUE),
    tjanstdelt_pagaende = sum(tjanstdelt_pagaende, na.rm = TRUE),
    tjanstdelt_avslutat = sum(tjanstdelt_avslutat, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Group by period to calculate the total for each period
  group_by(period) %>%
  mutate(
    total_ny = sum(tjanstdelt_ny, na.rm = TRUE),
    total_pagaende = sum(tjanstdelt_pagaende, na.rm = TRUE),
    andel_ny = tjanstdelt_ny / total_ny,
    andel_pagaende = tjanstdelt_pagaende / total_pagaende
  ) %>%
  ungroup()

df2 <- df2 %>%
  # Filter for the relevant tjanst values and replace with "Rusta och Matcha"
  filter(tjanst %in% c("Rusta och matcha 2", "Kundval Rusta och Matcha")) %>%
  mutate(
    tjanst = "Rusta och Matcha",  # Combine both categories into "Rusta och Matcha"
    across(c(tjanstdelt_ny, tjanstdelt_pagaende, tjanstdelt_avslutat), apply_numeric)
  ) %>%
  # Group by leverantor, period, placering_kommun, and the combined tjanst value
  group_by(leverantor, period, placering_kommun, tjanst) %>%
  # Summing values within each grouping of leverantor, period, and municipality
  summarise(
    tjanstdelt_ny = sum(tjanstdelt_ny, na.rm = TRUE),
    tjanstdelt_pagaende = sum(tjanstdelt_pagaende, na.rm = TRUE),
    tjanstdelt_avslutat = sum(tjanstdelt_avslutat, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Group by period and municipality to calculate totals within each municipality
  group_by(period, placering_kommun) %>%
  mutate(
    total_ny = sum(tjanstdelt_ny, na.rm = TRUE),
    total_pagaende = sum(tjanstdelt_pagaende, na.rm = TRUE),
    andel_ny = tjanstdelt_ny / total_ny,
    andel_pagaende = tjanstdelt_pagaende / total_pagaende
  ) %>%
  ungroup()

write.csv(df, "simple_marketshare/simple_marketshare.csv", row.names = FALSE)
write.csv(df2, "simple_marketshare/simple_marketshare_municipality.csv", row.names = FALSE)
