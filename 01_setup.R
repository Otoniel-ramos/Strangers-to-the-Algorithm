library(tidyverse)
library(dplyr)
library(readr)
library(data.table)
library(ggthemes)

### READING IN RAW DATA

# (1) RAW DATA ON MONTHLY RISK RATE DISTRIBUTION

# Raw Risk Rates Data (Flight and Public Safety): 
# EX. IF the variable med_risk_flight = 48.4 THIS means that 48.4% 
# of indiduals processed that month where given a medium score on flight risk.
risk_rates_data    <- read_csv("data/raw/risk_rates_raw.csv")

# (2) Raw data on Final Decisions: 
# EX. IF det_without_bond = 70.34 THIS means 70.34% 
# of people processed that month were detained without bond.

# Rate of Detained Without Bond
det_no_bond_data   <- read_csv("data/raw/decision_dataset_raw2.csv", col_names = FALSE)

# Rate of Releases
release_data       <- read_csv("data/raw/decision_dataset_raw3.csv", col_names = FALSE)

# Rate of Detained WITH Bond
det_with_bond_data <- read_csv("data/raw/decision_dataset_raw1.csv", col_names = FALSE)


### CLEANING DATA
 
# (1) RISK RATES: CLEANING DATA

risk_rates_data <- risk_rates_data |>
  # Selecting date and risk rates
  select(Date.x, low_risk_flight, med_risk_flight,               
    high_risk_flight, low_risk_pub,                      
    med_risk_pub, high_risk_pub) |>
  # Renaming for consitency
  rename(date = Date.x) |>   
  # Arranging by date 2012-07 to 2016-10
  arrange(date) |> 
  # Standardizing observations to 1st of month
  mutate(date = floor_date(date, "month"))                       

# RISK RATES: Missing Values and Checking totals
risk_rates <- risk_rates_data |> 
  # Making implicit missing value for 2016-09 observation explicit
  complete(date = seq(min(date), max(date), by = "month")) |> 
  # Imputing Missing Values
  mutate(
    across(c(low_risk_flight, med_risk_flight, high_risk_flight, 
      low_risk_pub, med_risk_pub, high_risk_pub),
      # Condition: Is value missing?
      ~ if_else(is.na(.), 
      # If TRUE: Use avg of previous and next observation, else keep current value)
      (lag(.) + lead(.)) / 2, . ))) |> 
  # Checking Totals
  mutate(
    total_flight = low_risk_flight + med_risk_flight + high_risk_flight,
    total_pub    = low_risk_pub + med_risk_pub + high_risk_pub, 
    monthnumber  = row_number()
  ) 

# (2) FINAL DECISIONS: CLEANING DATA

# FUNCTION: Prepping Final Decision Data for Joining
prep_join <- function(data_frame, column_name) {
  colnames(data_frame) <- c("date", column_name) # Setting Column Names
  data_frame |>                                  
    arrange(date) |>                             # Arranging by Date
    mutate(date = floor_date(date, "month"))     # Standardizing observations to 1st of the Month
}

# APPLYING PREP_JOIN

# Release Data
release_data <- prep_join(     
  data_frame  = release_data, 
  column_name = "released")

# Detain Without Bond Data
det_no_bond_data <- prep_join(
  data_frame  = det_no_bond_data, 
  column_name = "detained_without_bond")

# Detain With Bond Data
det_with_bond_data <- prep_join(
  data_frame  = det_with_bond_data, 
  column_name = "detained_with_bond")

# JOINING FINAL DECISION DATA

final_decisions <- det_no_bond_data |> 
  # Columns joined by severity of decision
  left_join(det_with_bond_data, join_by(date)) |> #one-to-one relation implicit
  left_join(release_data, join_by(date))

# ADDRESSING IMPLICIT MISSING VALUE FOR 2016-09-01

final_decisions <- final_decisions |>
  # Implicit Missing Value will now be Explicit
  complete(date = seq(min(date), max(date), by = "month"))      

# Applying Linear Imputation for Missing Obs Values
final_decisions <- final_decisions |>
  mutate(
    across(c(detained_without_bond, detained_with_bond, released),
    # Condition: Is value missing?
    ~ if_else(is.na(.), 
    # If TRUE: Use avg of previous and next observation, else keep current value
    (lag(.) + lead(.)) / 2, . ))) |> 
  # Rescaling to correct for extraction errors
  mutate(total = detained_without_bond + detained_with_bond + released) |>
  mutate(
    across(c(detained_without_bond, detained_with_bond, released),
    ~ . / total * 100)) |> # Normalizing to 100
  # New total = 100 for all observations
  mutate(total = detained_without_bond + detained_with_bond + released, 
    monthnumber = row_number()
  ) 


### CREATING "LONG" VERSIONS OF DATAFRAMES FOR PLOTTING

# (1) RISK RATES: PIVOT LONGER
risk_rates_long <- risk_rates |>
  # Exclude totals
  select(date, monthnumber, low_risk_flight, med_risk_flight, high_risk_flight, 
      low_risk_pub, med_risk_pub, high_risk_pub) |>
  pivot_longer(
    # Specify group of columns we are pivoting
    # New columns
    cols          = c(low_risk_flight, med_risk_flight, high_risk_flight, 
      low_risk_pub, med_risk_pub, high_risk_pub),
    names_to      = c("risk_level", "rate_type"),
    names_pattern = "(.+)_risk_(.+)",
    values_to     = "percentage"
  ) 

# (2) FINAL DECISIONS: PIVOT LONGER

final_decisions_long <- final_decisions |>
  select(date, monthnumber, detained_without_bond, detained_with_bond, released) |>
  pivot_longer(
    # Specify columns we are pivoting
    cols      = c(detained_without_bond, detained_with_bond, released),
    # New column for variable names
    names_to  = "decision",
    values_to = "percentage"
  )


### WRITING RDS FILES

# (1) Risk Rates
write_rds(risk_rates, "data/interim/risk_rates.rds")

# Risk Rates Long
write_rds(risk_rates_long, "data/interim/risk_rates_long.rds")

# (2) FINAL DECISIONS: WRITE RDS
write_rds(final_decisions, "data/interim/final_decisions.rds")

# Final Decisions Long
write_rds(final_decisions_long, "data/interim/final_decisions_long.rds")