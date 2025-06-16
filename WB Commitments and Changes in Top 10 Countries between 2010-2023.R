# Load required libraries
library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)
library(ggpattern)

# Read Excel file
wb <- read_excel("~/Downloads/all(1).xlsx")

# Remove dropped projects
wb <- wb %>% filter(tolower(status) != "dropped")

# Clean and reformat board approval date
wb$boardapprovaldate <- as.Date(sub("T.*", "", wb$boardapprovaldate))
wb$boardapprovaldate <- format(wb$boardapprovaldate, "%m-%d-%Y")

# Add year column
wb$year <- format(as.Date(wb$boardapprovaldate, format = "%m-%d-%Y"), "%Y")

# Remove unnecessary columns
columns_to_remove <- c(
  "status", "last_stage_reached_name", "project_name", "pdo", "impagency",
  "public_disclosure_date", "boardapprovaldate", "closingdate", "borrower",
  "lendinginstr", "envassesmentcategorycode", "esrc_ovrl_risk_rate",
  "supplementprojectflg", "loan_effective_date", "cons_serv_reqd_ind", "proj_last_upd_date",
  "curr_project_cost", "projectfinancialtype"
)
wb_cleaned <- wb %>% select(-all_of(columns_to_remove))

# Convert relevant columns to numeric
wb_cleaned <- wb_cleaned %>%
  mutate(
    curr_ibrd_commitment = as.numeric(case_when(
      trimws(as.character(curr_ibrd_commitment)) %in% c("", "NA", "na", "n/a") ~ "0",
      TRUE ~ as.character(curr_ibrd_commitment)
    )),
    idacommamt = as.numeric(case_when(
      trimws(as.character(idacommamt)) %in% c("", "NA", "na", "n/a") ~ "0",
      TRUE ~ as.character(idacommamt)
    )),
    grantamt = as.numeric(case_when(
      trimws(as.character(grantamt)) %in% c("", "NA", "na", "n/a") ~ "0",
      TRUE ~ as.character(grantamt)
    ))
  )

# Redefine total commitment to exclude grants (IBRD + IDA only)
wb_cleaned <- wb_cleaned %>%
  mutate(
    curr_total_commitment = curr_ibrd_commitment + idacommamt
  ) %>%
  select(-grantamt)  # Drop grantamt column

# Remove rows where both IBRD and IDA commitments are zero, NA, or one is zero and the other NA
wb_cleaned <- wb_cleaned %>%
  filter(
    !is.na(year) & tolower(year) != "n/a" &
      !(
        (is.na(curr_ibrd_commitment) & is.na(idacommamt)) |
          (curr_ibrd_commitment == 0 & idacommamt == 0) |
          (curr_ibrd_commitment == 0 & is.na(idacommamt)) |
          (is.na(curr_ibrd_commitment) & idacommamt == 0)
      )
  )

# Convert year to numeric for filtering and sorting
wb_cleaned$year <- as.numeric(wb_cleaned$year)

# Sort data
wb_sorted <- wb_cleaned %>% arrange(year, countryshortname)

# Country-year summary (each countries total commitments per year)
country_year_summary <- wb_sorted %>%
  group_by(year, countryshortname) %>%
  summarise(
    total_commitment = sum(curr_total_commitment, na.rm = TRUE),
    total_ibrd = sum(curr_ibrd_commitment, na.rm = TRUE),
    total_ida = sum(idacommamt, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    percent_ibrd = round(100 * total_ibrd / total_commitment, 2),
    percent_ida = round(100 * total_ida / total_commitment, 2)
  )

# Add yearly summary
yearly_summary <- country_year_summary %>%
  group_by(year) %>%
  summarise(
    year_total_commitment = sum(total_commitment, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(year) %>%
  mutate(
    cumulative_commitment = if_else(
      year >= 2010,
      cumsum(if_else(year >= 2010, year_total_commitment, 0)),
      0
    )
  )

# Limit to years 2010–2023 & merge country-year data with cumulative total
final_output
final_output <- country_year_summary %>%
  left_join(yearly_summary, by = "year") %>%
  mutate(
    country_share_of_year_total = round(100 * total_commitment / year_total_commitment, 2)
  ) %>%
  arrange(year, desc(country_share_of_year_total)) %>%
  filter(year >= 2010 & year <= 2023)

filtered_data <<- final_output %>% filter(year >= 2010 & year <= 2023)


# ========================
#  make a top 10 list with countries that change year to year 
# ========================

# Identify top 10 countries by year
top10_countries_per_year <- final_output %>%
  group_by(year) %>%
  slice_max(order_by = country_share_of_year_total, n = 10, with_ties = FALSE) %>%
  ungroup()

# Create the table structure: Countries as rows, Years as columns
library(tidyr)
table_data <- top10_countries_per_year %>%
  select(year, countryshortname, country_share_of_year_total) %>%
  pivot_wider(
    names_from = year,
    values_from = country_share_of_year_total
  )

# View the table
View(table_data)


# make a graph for it
# Prepare the dataset: Top 10 countries per year
top10_countries_per_year <- final_output %>%
  group_by(year) %>%
  slice_max(order_by = country_share_of_year_total, n = 10, with_ties = FALSE) %>%
  ungroup()

# Optional: Reorder country names within each year for better plot appearance
top10_countries_per_year <- top10_countries_per_year %>%
  group_by(year) %>%
  mutate(countryshortname = fct_reorder(countryshortname, country_share_of_year_total)) %>%
  ungroup()

# Create a unique mapping of countries to patterns and greyscale fills
unique_countries <- unique(plot_data$countryshortname)

# Define patterns and grey colors
patterns <- c('stripe', 'crosshatch', 'circle', 'wave', 'none', 'grid', 'pch', 'polygon_tiling')
greys <- seq(0.2, 0.8, length.out = length(unique_countries))  # Shades of grey from dark to light

# Assign each country a pattern and grey
country_style <- data.frame(
  countryshortname = unique_countries,
  pattern = rep(patterns, length.out = length(unique_countries)),
  fill = gray(greys)
)

# Merge styles into plotting data
plot_data <- plot_data %>% left_join(country_style, by = "countryshortname")

# Plot
p <- ggplot(plot_data, aes(
  x = factor(year),
  y = country_share_of_year_total,
  pattern = pattern,
  pattern_fill = fill,
  pattern_colour = fill
)) +
  geom_bar_pattern(
    stat = "identity",
    color = "black",
    pattern_density = 0.5,
    pattern_spacing = 0.03,
    pattern_key_scale_factor = 0.6
  ) +
  scale_pattern_manual(values = setNames(country_style$pattern, country_style$countryshortname)) +
  scale_pattern_fill_manual(values = setNames(country_style$fill, country_style$countryshortname)) +
  scale_pattern_colour_manual(values = setNames(country_style$fill, country_style$countryshortname)) +
  labs(
    title = "Top 10 Countries' Share of WB Commitments (2010–2023)",
    x = "Year",
    y = "Country Share of Yearly Total (%)",
    pattern = "Country"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)

View(plot_data)

# Save table to Excel
write_xlsx(table_data, "~/Downloads/Top10_Countries_Share_by_Year.xlsx")


# ========================
# next graph
# ========================

# Step 1: Calculate global totals for IBRD, IDA, and total commitments between 2010-2023 
global_totals <- filtered_data %>%
  summarise(
    global_ibrd_commitment = sum(total_ibrd, na.rm = TRUE),
    global_ida_commitment = sum(total_ida, na.rm = TRUE),
    global_total_commitment = sum(total_commitment, na.rm = TRUE)
  )

# Step 2: Calculate total commitments per country between 2010-2023
country_totals <- filtered_data %>%
  group_by(countryshortname) %>%
  summarise(
    total_ibrd_commitment = sum(total_ibrd, na.rm = TRUE),
    total_ida_commitment = sum(total_ida, na.rm = TRUE),
    total_commitment = sum(total_commitment, na.rm = TRUE),
    .groups = 'drop'
  )

# Step 3: Calculate each country's % share of global commitments and get top 10 countries
top_10_countries <- country_totals %>%
  mutate(
    percent_of_global_ibrd = round(100 * total_ibrd_commitment / global_totals$global_ibrd_commitment, 2),
    percent_of_global_ida = round(100 * total_ida_commitment / global_totals$global_ida_commitment, 2),
    percent_of_global_total = round(100 * total_commitment / global_totals$global_total_commitment, 2)
  ) %>%
  arrange(desc(total_commitment)) %>%
  slice_head(n = 10)

# View the top 10 countries and their share of global commitments
View (top_10_countries)

# =========================================
# 🔥 graphs 🔥
# =========================================

# Create a complete grid of top 10 countries and all years (2010-2023)
all_years <- 2010:2023
top_countries <- top_10_countries$countryshortname

complete_grid <- expand.grid(
  year = all_years,
  countryshortname = top_countries
)

# Merge this grid with your final_output to fill in missing years
top_10_yearly_full <- complete_grid %>%
  left_join(final_output, by = c("year", "countryshortname")) %>%
  mutate(
    total_ibrd = ifelse(is.na(total_ibrd), 0, total_ibrd),
    total_ida = ifelse(is.na(total_ida), 0, total_ida),
    total_commitment = ifelse(is.na(total_commitment), 0, total_commitment),
    year_total_commitment = ifelse(is.na(year_total_commitment), 0, year_total_commitment),
    country_share_of_year_total = ifelse(year_total_commitment > 0, 
                                         round(100 * total_commitment / year_total_commitment, 2), 
                                         0)
  ) %>%
  arrange(countryshortname, year)

View(top_10_yearly_full)

# Export both summary tables to Excel
write_xlsx(
  list(
    "Top 10 Countries Summary" = top_10_countries,
    "Yearly Commitments (Top 10)" = top_10_yearly_full
  ),
  path = "World_Bank_Top_10_Countries_Details.xlsx"
)

# ====================
# graph the data for top 10 between 2010-2023
#-====================

# Make sure year is treated as a factor to keep all years on x-axis
top_10_yearly_full$year <- as.factor(top_10_yearly_full$year)

# Create the stacked bar plot
ggplot(top_10_yearly_full, aes(x = year, y = country_share_of_year_total, fill = countryshortname)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Share of Total World Bank Commitments (2010–2023)",
    x = "",
    y = "Share of Total WB Commitment (%)",
    fill = "Country"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# Example pattern types and color assignments
pattern_mapping <- c(
  "China" = "stripe",
  "Ukraine" = "crosshatch",
  "Turkiye" = "circle",
  "Pakistan" = "none",
  "Nigeria" = "crosshatch",
  "Indonesia" = "circle",
  "India" = "none",
  "Ethiopia" = "none",
  "Brazil" = "circle",
  "Bangladesh" = "stripe"
)

fill_mapping <- c(
  "China" = "black",
  "Ukraine" = "grey30",
  "Turkiye" = "grey",
  "Pakistan" = "grey60",
  "Nigeria" = "grey80",
  "Indonesia" = "grey90",
  "India" = "grey50",
  "Ethiopia" = "grey90",
  "Brazil" = "grey40",
  "Bangladesh" = "grey80"
)


# Make sure 'year' is a factor to preserve order
top_10_yearly_full$year <- as.factor(top_10_yearly_full$year)

# Plot
ggplot(top_10_yearly_full, aes(x = year, y = country_share_of_year_total, 
                          pattern = countryshortname, fill = countryshortname)) +
  geom_bar_pattern(
    stat = "identity",
    color = "black",
    pattern_fill = "black",
    pattern_color = "black",
    pattern_density = 0.1,
    pattern_spacing = 0.02,
    pattern_angle = 45
  ) +
  scale_pattern_manual(values = pattern_mapping) +
  scale_fill_manual(values = fill_mapping) +
  labs(
    title = "Share of Total World Bank Commitments (2010–2023)",
    x = "",
    y = "Share of Total WB Commitment (%)",
    fill = "Country",
    pattern = "Country"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

