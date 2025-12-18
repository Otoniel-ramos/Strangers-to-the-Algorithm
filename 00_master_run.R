###################
## MASTER SCRIPT ##
###################


# 1. Read in Raw data (Final Decisions and Risk Rates) and Clean/Transform it.
source("01_setup.R")

# 2. Use Risk Rates and Logic-Based Scoring Rules to get RCA Recommendations.
source("02_rec_rates.R")

# 3. Use Final Decisions and RCA Recommendations to get Discretion Measures.
source("03_discretion_rates.R")

# 4. Create Plots used in Paper
source("04_plots.R")