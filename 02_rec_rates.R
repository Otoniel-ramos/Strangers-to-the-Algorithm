# The following code will allow us to derive estimates on ICE's RCA Recommendations 
# based on the (1) data available on Risk Rates (2) The RCA Logic Based Scoring Rules 
# which map Risk Combinations (e.g: Low Flight Risk, Low Public Safety Risk) -> 
# (Release) to a Recommendation 

##########################
## RECOMMENDATION RATES ##
##########################

### (1) READ IN DATA (Risk Rates)
input_risk <- read_rds("data/interim/risk_rates.rds")

### (2) APPLY MAPPING RULES 

#   (A) PERIOD 1 MAPPING RULES (2012-07 - 2013-12)
input_risk <- input_risk |> 
  mutate(
    rec_release_p1    = {(low_risk_flight*low_risk_pub)/100},
    rec_spdet_p1      = {(low_risk_flight*med_risk_pub)/100} + 
      {(med_risk_flight*low_risk_pub)/100} + 
      {(med_risk_flight*med_risk_pub)/100},
    rec_detwbond_p1   = {(high_risk_flight*low_risk_pub)/100}  + 
      {(high_risk_flight*med_risk_pub) /100} + 
      {(high_risk_flight*high_risk_pub)/100} + 
      {(low_risk_flight *high_risk_pub)/100} + 
      {(med_risk_flight *high_risk_pub)/100},
    # Detain Without Bond was not a recommendation option in Period 1
    rec_detnobond_p1  = 0,
    total_p1          = rec_release_p1 + 
      rec_spdet_p1    + 
      rec_detwbond_p1 + 
      rec_detnobond_p1                   
  ) 

#   (B) PERIOD 2 MAPPING RULES (2014-01 - 2015-02)
input_risk <- input_risk |>
  mutate(
    rec_release_p2   = 
      {(low_risk_flight * low_risk_pub)/100} + 
      {(med_risk_flight * low_risk_pub)/100},
    rec_spdet_p2     = 
      {(high_risk_flight * low_risk_pub)/100} + 
      {(low_risk_flight  * med_risk_pub)/100} + 
      {(med_risk_flight  * med_risk_pub)/100},
    rec_detwbond_p2  = 
      {(high_risk_flight * med_risk_pub) /100} + 
      {(low_risk_flight  * high_risk_pub)/100},
    rec_detnobond_p2 = 
      {(med_risk_flight  * high_risk_pub)/100} + 
      {(high_risk_flight * high_risk_pub)/100},
    total_p2 = 
      rec_release_p2 + 
      rec_spdet_p2 + 
      rec_detwbond_p2 + 
      rec_detnobond_p2
  ) 

#   (C) PERIOD 3 MAPPING RULES (2015-03 - 2016-10)
recommendation_rates <- input_risk |>
  mutate(
    rec_release_p3   = {(low_risk_flight * low_risk_pub)/100},
    rec_spdet_p3     = {(med_risk_flight * low_risk_pub)/100} +
      {(high_risk_flight * low_risk_pub)/100} +
      {(low_risk_flight  * med_risk_pub)/100},
    # Detain With Bond was NOT Recommendation Option in Period 3
    rec_detwbond_p3  = 0,
    rec_detnobond_p3 = {(med_risk_flight * med_risk_pub)/100} +
           {(med_risk_flight  * high_risk_pub)/100} +
           {(high_risk_flight * med_risk_pub)/100} +
           {(high_risk_flight * high_risk_pub)/100} +
           {(low_risk_flight  * high_risk_pub)/100},
    total_p3 = rec_release_p3 + rec_spdet_p3 + rec_detwbond_p3 + rec_detnobond_p3
  )

### (3) OBSERVED RECOMMENDATION RATES

recommendation_rates <- recommendation_rates |>
  mutate(
    monthnumber = row_number(),
    period = cut(monthnumber, 
      breaks = c(0, 18, 32, 52), 
      labels = c("period_1", "period_2", "period_3")),
    obs_rec_release = case_when(
      period == "period_1" ~ rec_release_p1,
      period == "period_2" ~ rec_release_p2,
      period == "period_3" ~ rec_release_p3,
      TRUE ~ NA_real_ # Catch All give NA as real double
      ),
    obs_rec_spdet = case_when(
      period == "period_1" ~ rec_spdet_p1,
      period == "period_2" ~ rec_spdet_p2,
      period == "period_3" ~ rec_spdet_p3,
      TRUE ~ NA_real_ 
      ),
    obs_rec_detwbond = case_when(
      period == "period_1" ~ rec_detwbond_p1,
      period == "period_2" ~ rec_detwbond_p2,
      period == "period_3" ~ rec_detwbond_p3,
      TRUE ~ NA_real_ 
      ),
    obs_rec_detnobond = case_when(
      period == "period_1" ~ rec_detnobond_p1,
      period == "period_2" ~ rec_detnobond_p2,
      period == "period_3" ~ rec_detnobond_p3,
      TRUE ~ NA_real_ 
      ),
    obs_rec_total = obs_rec_release + 
      obs_rec_spdet + 
      obs_rec_detwbond + 
      obs_rec_detnobond
    ) |>
  select(!low_risk_flight:total_pub) |>
  relocate(date, monthnumber, period)

### (4) Saving Data Frame for Observed Recommendation Rates

# Function to Save Recommendation Subsets
save_recommendation_subset <- function(data, pattern, file_name){
  subset_df <- data |>
    select(
      date, monthnumber, period, 
      contains(pattern)) # All Columns Matching (_p1, _p2, _p3, obs_rec)
  # Construct File Path
  file_path <- paste0("data/interim/", file_name, ".rds")
  write_rds(subset_df, file_path)
  return(invisible(subset_df))
}

# 1. Save Observed Recommendations
save_recommendation_subset(recommendation_rates, "obs_rec", "observed_recommendations")

# 2. Save Period 1 Recommendations
save_recommendation_subset(recommendation_rates, "_p1", "period1_recommendations")

# 3. Save Period 2 Recommendations
save_recommendation_subset(recommendation_rates, "_p2", "period2_recommendations")

# 4. Save Period 3 Recommendations
save_recommendation_subset(recommendation_rates, "_p3", "period3_recommendations")
