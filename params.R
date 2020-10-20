# imported from Authors: Hamza Tazi Bouardi (htazi@mit.edu), Michael L. Li (mlli@mit.edu), Omar Skali Lami (oskali@mit.edu)
library(tidyverse)

# Default parameters - TNC & Trust Region

default_parameter_list <-  c(1, 0, 2, 0.2, 0.05, 0.2, 3, 3, 0.1, 3, 1) # Default parameters for the solver

names <- c(  "alpha", "days", "r_s", "r_dth", "p_dth", "r_dthdecay",
                        "k1", "k2", "jump", "t_jump", "std_normal")

values <-  c( 0,  NA,  0, 0.02,  0, 0, 0,  0,  0, 0, 1)  

# Allows for reinitialization of parameters in case they reach a value that is too low/high
dict_default_reinit_parameters <-data.frame(names, values)


values <- c(  0, NA, 0, 0.02, 0, 0, 0, 0,  0, 0, 1)

# Allows for reinitialization of lower bounds in case they reach a value that is too low
dict_default_reinit_lower_bounds <- data.frame(names, values)

values <- c(   0, NA,  0, 0.02,  0, 0, 0,  0,  0,  0,  1)

# Allows for reinitialization of upper bounds in case they reach a value that is too high
dict_default_reinit_upper_bounds <-  data.frame(names, values)

default_upper_bound <- 0.2
percentage_drift_upper_bound<-0.2
default_lower_bound<-0.2
percentage_drift_lower_bound<-0.2

default_bounds_params<-c(
  c(0.75, 1.25), c(-10, 10), c(1, 3), c(0.05, 0.5), c(0.01, 0.25), c(0, 0.5), c(0.1, 10), 
  c(0.1, 10), c(0, 5), c(0, 7), c(0.1, 5)
)  # Bounds for the solver
validcases_threshold <-7  # Minimum number of cases to fit the base-DELPHI
validcases_threshold_policy <- 15  # Minimum number of cases to train the country-level policy predictions
max_iter <- 500  # Maximum number of iterations for the algorithm

# Default parameters - Annealing
percentage_drift_upper_bound_annealing <- 0.5
default_upper_bound_annealing <- 0.5
percentage_drift_lower_bound_annealing <- 0.5
default_lower_bound_annealing <- 0.5
default_lower_bound_jump  <-  0
default_upper_bound_jump  <-  5
default_lower_bound_std_normal  <-  1
default_upper_bound_std_normal  <-  50

# Initial condition of exposed state and infected state
IncubeD  <-  5
RecoverID  <-  10
RecoverHD  <-  15
DetectD  <-  2
VentilatedD  <-  10  # Recovery Time when Ventilated
default_maxT  <-  as.Date("2020-12-15") # Maximum timespan of prediction
#  datetime(2020, 12, 15)  
n_params_without_policy_params  <-  7  # alpha, r_dth, p_dth, a, b, k1, k2
p_v  <-  0.25  # Percentage of ventilated
p_d  <-  0.2  # Percentage of infection cases detected.
p_h  <-  0.15  # Percentage of detected cases hospitalized

# Policies and future times for counterfactual predictions
future_policies  <-  c(
  'No_Measure', 'Restrict_Mass_Gatherings', 'Mass_Gatherings_Authorized_But_Others_Restricted',
  'Restrict_Mass_Gatherings_and_Schools', 'Authorize_Schools_but_Restrict_Mass_Gatherings_and_Others',
  'Restrict_Mass_Gatherings_and_Schools_and_Others', 'Lockdown'
)
# Maximum timespan of prediction under different policy scenarios
default_maxT_policies  <- as.Date("2021-03-15") #datetime(2021, 3, 15) 
future_times  <-  c(0, 7, 14, 28, 42)

# Default normalized gamma shifts from runs in May 2020

values <- c(1.0, 0.873, 0.668, 0.479, 0.794,  0.423, 0.239)

#
#default_dict_normalized_policy_gamma  <- c(
#  'No_Measure': 1.0,
#  'Restrict_Mass_Gatherings': 0.873,
#  'Authorize_Schools_but_Restrict_Mass_Gatherings_and_Others': 0.794,
#  'Mass_Gatherings_Authorized_But_Others_Restricted': 0.668,
#  'Restrict_Mass_Gatherings_and_Schools': 0.479,
#  'Restrict_Mass_Gatherings_and_Schools_and_Others': 0.423,
#  'Lockdown': 0.239
#)

default_dict_normalized_policy_gamma <- data.frame(future_policies, values)

# Additional utils inputs
days <- c(0,7,14,28,42)
desc <- c("Now", "One Week",  "Two Weeks",  "Four Weeks", "Six Weeks")
time_df <- data.frame(days = days, desc = desc, stringsAsFactors=TRUE)
#TIME_DICT  <-  c(c(0, "Now"), c(7, "One Week"), c(14, "Two Weeks"), c(28, "Four Weeks"), c(42, "Six Weeks"))

MAPPING_STATE_CODE_TO_STATE_NAME <- data.frame(stringsAsFactors=FALSE,
                        province = c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
                                  "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
                                  "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
                                  "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
                                  "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana",
                                  "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
                                  "New York", "North Carolina", "North Dakota", "Ohio",
                                  "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                                  "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
                                  "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", 
                                  "American Samoa", "Guam", "Northern Marianas",  "Puerto Rico",  "Virgin Islands" ),
                        state = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                                       "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                                       "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                                       "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD",
                                       "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", 
                                       "AS", "GU", "MP", "PR", "VI")
)

#MAPPING_STATE_CODE_TO_STATE_NAME  <- c(
#  'AL': 'Alabama', 'AK': 'Alaska', 'AZ': 'Arizona', 'AR': 'Arkansas', 'CA',
#  'CO', 'CT', 'DE', 'DC',  'FL', 'GA', 'HI', 'ID': 'Idaho', 'IL': 'Illinois',
#  'IN': 'Indiana', 'IA': 'Iowa', 'KS': 'Kansas', 'KY': 'Kentucky', 'LA': 'Louisiana',
#  'ME': 'Maine', 'MD': 'Maryland', 'MA': 'Massachusetts', 'MI': 'Michigan',
#  'MN': 'Minnesota', 'MS': 'Mississippi', 'MO': 'Missouri', 'MT': 'Montana',
#  'NE': 'Nebraska', 'NV': 'Nevada', 'NH': 'New Hampshire', 'NJ': 'New Jersey',
#  'NM': 'New Mexico', 'NY': 'New York', 'NC': 'North Carolina', 'ND': 'North Dakota',
#  'OH': 'Ohio', 'OK': 'Oklahoma', 'OR': 'Oregon', 'PA': 'Pennsylvania',
#  'RI': 'Rhode Island', 'SC': 'South Carolina', 'SD': 'South Dakota', 'TN': 'Tennessee',
#  'TX': 'Texas', 'UT': 'Utah', 'VT': 'Vermont', 'VA': 'Virginia', 'WA': 'Washington',
#  'WV': 'West Virginia', 'WI': 'Wisconsin', 'WY': 'Wyoming', "AS": "American Samoa",
#  "GU": "Guam", "MP": "Northern Marianas", "PR": "Puerto Rico", "VI": "Virgin Islands"
#)
default_policy  <-  "Lockdown"  # Eventually change to future_policies[-1]
default_policy_enaction_time  <-  'Now'  # Eventually change to TIME_DICT[0]