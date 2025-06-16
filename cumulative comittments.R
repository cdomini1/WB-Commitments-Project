# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# Download and read Excel file, but remove the first 2 rows from the excel b/c they are not necessary
wb <- read_excel("~/Downloads/all-2.xlsx", skip = 2)

# Remove dropped projects (those with status == "Dropped")
wb <- wb %>% filter(tolower(status) != "dropped")

# Assign years based on board approval or loan effective dates (board approval date takes priority)
wb$boardapprovaldate <- as.Date(sub("T.*", "", wb$boardapprovaldate))
wb$loan_effective_date <- as.Date(sub("T.*", "", wb$loan_effective_date))
wb$closingdate <- as.Date(sub("T.*", "", wb$closingdate))

wb$year <- ifelse(!is.na(wb$boardapprovaldate),
                  as.numeric(format(wb$boardapprovaldate, "%Y")),
                  ifelse(!is.na(wb$loan_effective_date),
                         as.numeric(format(wb$loan_effective_date, "%Y")),
                         ifelse(!is.na(wb$closingdate) & wb$closingdate < as.Date("2009-01-01"),
                                2009,NA)))  # If none of the conditions are met, keep as NA

# Remove rows with NA year (meaning they only had "last updated date" or missing all relevant dates)
wb <- wb[!is.na(wb$year), ]

# Remove specified columns
columns_to_remove <- c(
  "status", "last_stage_reached_name", "project_name", "pdo", "impagency", 
  "public_disclosure_date", "boardapprovaldate", "closingdate", "borrower", 
  "lendinginstr", "envassesmentcategorycode", "esrc_ovrl_risk_rate", 
  "supplementprojectflg", "loan_effective_date", "cons_serv_reqd_ind", "proj_last_upd_date", 
  "curr_project_cost", "projectfinancialtype"
)
wb_cleaned <- wb %>% select(-all_of(columns_to_remove))

# Convert character "NA", "n/a", or "" to actual NA, then convert columns to numeric
wb_cleaned <- wb_cleaned %>%
  mutate(
    curr_ibrd_commitment = as.numeric(na_if(trimws(as.character(curr_ibrd_commitment)), "")),
    idacommamt = as.numeric(na_if(trimws(as.character(idacommamt)), "")),
    grantamt = as.numeric(na_if(trimws(as.character(grantamt)), ""))
  )

# Remove rows where both IDA and IBRD commitments are zero or missing 
wb_cleaned <- wb_cleaned %>%
  filter(!((is.na(curr_ibrd_commitment) | curr_ibrd_commitment == 0) & 
             (is.na(idacommamt) | idacommamt == 0)))

# replace NAs with 0, if the total commitment amount adds up correctly
wb_cleaned <- wb_cleaned %>%
  # Apply row-wise logic
  rowwise() %>%
  mutate(
    # Case 1: If curr_ibrd_commitment is NA, but idacommamt is a number
    curr_ibrd_commitment = ifelse(
      is.na(curr_ibrd_commitment) & !is.na(idacommamt) & (idacommamt + grantamt == curr_total_commitment),
      0,
      curr_ibrd_commitment
    ),
    # Case 2: If idacommamt is NA, but curr_ibrd_commitment is a number
    idacommamt = ifelse(
      is.na(idacommamt) & !is.na(curr_ibrd_commitment) & (curr_ibrd_commitment + grantamt == curr_total_commitment),
      0,
      idacommamt
    ),
    # Flag invalid rows that don't satisfy the total_commitment_amount condition
    keep_row = case_when(
      is.na(curr_ibrd_commitment) & !is.na(idacommamt) ~ idacommamt + grantamt == curr_total_commitment,
      is.na(idacommamt) & !is.na(curr_ibrd_commitment) ~ curr_ibrd_commitment + grantamt == curr_total_commitment,
      TRUE ~ TRUE  # Keep all other rows
    )
  ) %>%
  # Keep only valid rows
  filter(keep_row) %>%
  # Remove the helper column
  select(-keep_row) %>%
  ungroup()

# compute total commitment excluding grants 
wb_cleaned <- wb_cleaned %>%
  mutate(
    curr_total_commitment = idacommamt + curr_ibrd_commitment
  )

# get rid of the grant column
wb_cleaned <- wb_cleaned %>% select(-grantamt)

# calculate yearly IDA, IBRD, and total commitments per country
country_yearly_totals <- wb_cleaned %>%
  group_by(countryshortname, year) %>%
  summarise(
    curr_ibrd_commitment = sum(curr_ibrd_commitment, na.rm = TRUE),
    idacommamt = sum(idacommamt, na.rm = TRUE),
    curr_total_commitment = sum(curr_total_commitment, na.rm = TRUE),
  ) %>%
  arrange(countryshortname, year)

# create columns for t and t-1 cummulative IDA and IBRD totals
country_yearly_totals <- country_yearly_totals %>%
  group_by(countryshortname) %>%
  arrange(year, .by_group = TRUE) %>%   # sort years within each country
  mutate(
    # 1) cumulative IBRD up to but not including that year
    cum_ibrd_before_year = lag(cumsum(curr_ibrd_commitment), default = 0),
    # 2) cumulative IDA up to but not including that year
    cum_ida_before_year = lag(cumsum(idacommamt), default = 0),
    # 3) cumulative IBRD including that year
    cum_ibrd_including_year = cumsum(curr_ibrd_commitment),
    # 4) cumulative IDA including that year
    cum_ida_including_year = cumsum(idacommamt),
    # 5) cumulative TOTAL up to but not including that year
    cum_total_before_year = lag(cumsum(curr_total_commitment), default = 0),
    # 6) cumulative TOTAL including that year
    cum_total_including_year = cumsum(curr_total_commitment)
  ) %>%
  ungroup() %>%
  arrange(countryshortname, year)

# only display data from 2010-2023
country_yearly_totals_filtered <- country_yearly_totals %>%
  filter(year >= 2010, year <= 2023)

# displaying final results
View(country_yearly_totals_filtered)



# Create a full grid of all countries and years 1996-2023
all_countries <- unique(country_yearly_totals$countryshortname)
all_years <- 1996:2023

full_grid <- expand.grid(
  countryshortname = all_countries,
  year = all_years
)

# Merge your existing summary into the full grid
country_yearly_totals_expanded <- full_grid %>%
  left_join(country_yearly_totals, by = c("countryshortname", "year")) %>%
  # Replace NAs (years with no projects) with 0 commitments
  mutate(
    curr_ibrd_commitment = ifelse(is.na(curr_ibrd_commitment), 0, curr_ibrd_commitment),
    idacommamt = ifelse(is.na(idacommamt), 0, idacommamt),
    curr_total_commitment = ifelse(is.na(curr_total_commitment), 0, curr_total_commitment)
  ) %>%
  arrange(countryshortname, year) %>%
  group_by(countryshortname) %>%
  # Recalculate cumulative sums (keep your logic)
  mutate(
    cum_ibrd_before_year = cum_ibrd_before_year,
    cum_ida_before_year = cum_ida_before_year,
    cum_ibrd_including_year = cum_ibrd_including_year,
    cum_ida_including_year = cum_ida_including_year,
    cum_total_before_year = cum_total_before_year,
    cum_total_including_year = cum_total_including_year,
  ) %>%
  mutate(
    # Identify years with zero commitments
    zero_commitments = (curr_ibrd_commitment == 0 & idacommamt == 0),
    
    # For IBRD cumulative including year: if zero commitments, carry over previous year
    cum_ibrd_including_year = ifelse(
      zero_commitments,
      lag(cum_ibrd_including_year, default = 0),
      cum_ibrd_including_year
    ),
    
    # For IDA cumulative including year: if zero commitments, carry over previous year
    cum_ida_including_year = ifelse(
      zero_commitments,
      lag(cum_ida_including_year, default = 0),
      cum_ida_including_year
    ),
    
    # For total cumulative including year: if zero commitments, carry over previous year
    cum_total_including_year = ifelse(
      zero_commitments,
      lag(cum_total_including_year, default = 0),
      cum_total_including_year
    ),
    
    
    # totals calulations
    cum_total_including_year = ifelse(
      zero_commitments,
      lag(cum_total_including_year, default = 0),
      cum_total_including_year
    ),
    
    # Now recompute the "before year" columns based on the new cumulative totals
    cum_ibrd_before_year = lag(cum_ibrd_including_year, default = 0),
    cum_ida_before_year = lag(cum_ida_including_year, default = 0),
    cum_total_before_year = lag(cum_total_including_year, default = 0)
    
  ) %>%
  
  ungroup() %>%
  # Only display data from 2010-2023
  filter(year >= 2010, year <= 2023)

# View results
View(country_yearly_totals_expanded)


# Update your export to use this version
write_xlsx(country_yearly_totals_expanded, "cummulatives_yearly_summaries.xlsx")



# Create totals_key: total commitments per year across all countries
totals_key <- wb_cleaned %>%
  group_by(year) %>%
  summarise(
    total_ida_commitments = sum(idacommamt, na.rm = TRUE),
    total_ibrd_commitments = sum(curr_ibrd_commitment, na.rm = TRUE)
  ) %>%
  arrange(year) %>%
  mutate(
    total_cum_ida_including_year = cumsum(total_ida_commitments),
    total_cum_ida_before_year = lag(total_cum_ida_including_year, default = 0),
    total_cum_ibrd_including_year = cumsum(total_ibrd_commitments),
    total_cum_ibrd_before_year = lag(total_cum_ibrd_including_year, default = 0)
  )

# only display data from 2010-2023
totals_key_filtered <- totals_key %>%
  filter(year >= 2010, year <= 2023)

# display this totals_key
View(totals_key_filtered)

# export files to seprate excel 
write_xlsx(country_yearly_totals_filtered, "cummulatives_yearly_summaries.xlsx")
write_xlsx(totals_key, "totals_key.xlsx")

# export files to one excel
write_xlsx(list(
  "Cumm.Summary by Country" = country_yearly_totals_filtered,
  "Cumm.Totals Summary by Year" = totals_key_filtered,
  "Cleaned Data" = wb
), "WorldBank_Summary.xlsx")






"""
# Calculate cumulative sums - THIS STEP WAS WORKING PERFECTLY
country_yearly_totals <- country_yearly_totals %>%
  group_by(countryshortname) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    cum_ibrd_before_year = lag(cumsum(curr_ibrd_commitment), default = 0),
    cum_ida_before_year = lag(cumsum(idacommamt), default = 0),
    cum_ibrd_including_year = cumsum(curr_ibrd_commitment),
    cum_ida_including_year = cumsum(idacommamt),
    cum_total_before_year = lag(cumsum(curr_total_commitment), default = 0),
    cum_total_including_year = cumsum(curr_total_commitment)
  ) %>%
  ungroup()

# display ALL years for ALL countries:
# Build complete grid
country_year_grid <- expand_grid(countryshortname = unique(country_yearly_totals$countryshortname), year = 1996:2023)

# Join to existing cumulative table
country_yearly_totals_full <- country_year_grid %>%
  left_join(country_yearly_totals, by = c("countryshortname", "year")) %>%
  arrange(countryshortname, year)

# Fill cumulative sums forward, but DO NOT recompute them.
country_yearly_totals_full <- country_yearly_totals_full %>%
  group_by(countryshortname) %>%
  fill(
    cum_ibrd_before_year, cum_ida_before_year, cum_ibrd_including_year, cum_ida_including_year,
    cum_total_before_year, cum_total_including_year,
    .direction = "down"
  ) %>%
  mutate(
    curr_ibrd_commitment = ifelse(is.na(curr_ibrd_commitment), 0, curr_ibrd_commitment),
    idacommamt = ifelse(is.na(idacommamt), 0, idacommamt),
    curr_total_commitment = ifelse(is.na(curr_total_commitment), 0, curr_total_commitment)
  ) %>%
  ungroup()


# Filter for display years
country_yearly_totals_full_filtered <- country_yearly_totals_full %>%
  filter(year >= 2010, year <= 2023)

# View final results
View(country_yearly_totals_full_filtered)


# Create totals_key: total commitments per year across all countries
totals_key <- wb_cleaned %>%
  group_by(year) %>%
  summarise(
    total_ida_commitments = sum(idacommamt, na.rm = TRUE),
    total_ibrd_commitments = sum(curr_ibrd_commitment, na.rm = TRUE)
  ) %>%
  arrange(year) %>%
  mutate(
    total_cum_ida_including_year = cumsum(total_ida_commitments),
    total_cum_ida_before_year = lag(total_cum_ida_including_year, default = 0),
    total_cum_ibrd_including_year = cumsum(total_ibrd_commitments),
    total_cum_ibrd_before_year = lag(total_cum_ibrd_including_year, default = 0)
  )
"""








