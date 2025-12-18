
######################
## DISCRETION RATES ##
######################


### (A) DATA

# 1. Data of Final Decisions
input_final <- read_rds("data/interim/final_decisions.rds")

# 2. Data on RCA Recommendations
input_rca <- read_rds("data/interim/observed_recommendations.rds")


### (B) SETUP & JOIN

# 1. Perform a left_join keeping only select rows from final decisions
discretion_data <- input_rca |>
  left_join(input_final |> 
    select(date, monthnumber, released, detained_with_bond, detained_without_bond),
  by = c("date", "monthnumber")) # ONE-TO-ONE RELATION Implicit


### (C) DISCRETION MEASURES (DECISION - RECOMMENDATION)

discretion_data <- discretion_data |>
  mutate(
    # Net Discretion Measures (Absolute Value)
    release_disc_net   = abs(released - obs_rec_release),
    detwbond_disc_net  = abs(detained_with_bond - obs_rec_detwbond),
    detnobond_disc_net = abs(detained_without_bond - obs_rec_detnobond),
    # Signed Discretion Measures (Allows us to examine Punitive Behavior)
    release_disc_raw   = (released - obs_rec_release),
    detwbond_disc_raw  = (detained_with_bond - obs_rec_detwbond),
    detnobond_disc_raw = (detained_without_bond - obs_rec_detnobond)
  )


### (D) LONG DATAFRAMES FOR PLOTS

# 1. DF for Net Discretion
net_disc_long <- discretion_data |>
  select(date, monthnumber, 
    period, release_disc_net, 
    detwbond_disc_net, detnobond_disc_net) |>
  pivot_longer(
    cols = c(release_disc_net, detwbond_disc_net, detnobond_disc_net),
    names_to = "discretion_vector",
    values_to = "percentage"
  )
# 2. DF for Signed Discretion
raw_disc_long <- discretion_data |>
  select(date, monthnumber, 
    period, release_disc_raw, 
    detwbond_disc_raw, detnobond_disc_raw) |>
  pivot_longer(
    cols = c(release_disc_raw, detwbond_disc_raw, detnobond_disc_raw),
    names_to = "discretion_vector",
    values_to = "percentage"
  )


### (E) SAVING DATAFRAMES

# 1. Discretion Data
write_rds(discretion_data, "data/interim/discretion_data.rds")

# 2. Net Discretion Long
write_rds(net_disc_long, "data/interim/net_discretion_long.rds")

# 3. Signed Discretion Long
write_rds(raw_disc_long, "data/interim/signed_discretion_long.rds")