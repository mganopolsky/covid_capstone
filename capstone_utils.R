#https://github.com/COVIDAnalytics/DELPHI
require(tidyverse)
require(data.table)
require(data.frame)
require(lubridate)
require(readr)
require(devtools)
require(stringr)

library(dplyr)   
source("params.r")
library(dict)
library(lubridate)
library(stringr)

get_testing_data_us <- function(){
  
    #
    #  :return: a DataFrame where the column of interest is 'testing_cnt_daily'
    #  which gives the numbers of new daily tests per state
    
  df_test <- fread("https://covidtracking.com/api/v1/states/daily.csv")
  df_test$country <- "US"
  df_test$continent <- "North America"
  df_test$province = MAPPING_STATE_CODE_TO_STATE_NAME$province[match(df_test$state, MAPPING_STATE_CODE_TO_STATE_NAME$state)]
  
  #df_test.drop("state", axis=1, inplace=True)
  #df_test["date"] <- df_test.date.apply(lambda x: str(x)[:4] + "-" + str(x)[4:6] + "-" + str(x)[6:])
  df_test <- df_test %>% mutate(date = ymd(date), testing_cnt = totalTestResults) # , "%Y%m%d"))

  df_test <- df_test[order(province, date)]
  

  df_test <- df_test %>% select ( "continent", "country", "province", "date", "testing_cnt")

  list_df_concat <- data.frame()
    
  df_temp = data.frame()
  
  for (state in unique(unlist(df_test$province)))
    df_temp <- bind_rows(df_temp, df_test %>% filter(province == state))
  
  
  df_temp$testing_cnt_shift <- shift(  df_temp$testing_cnt , 1)
  df_temp$testing_cnt_daily <- df_temp$testing_cnt - df_temp$testing_cnt_shift
  df_temp$testing_cnt_daily[1] <- df_temp$testing_cnt[1]
  list_df_concat <- bind_rows(list_df_concat, df_temp)
  
  df_test_final <- list_df_concat %>% select (-testing_cnt, -testing_cnt_shift) #    pd.concat(list_df_concat).reset_index(drop=True)
  #df_test_final.drop(["testing_cnt", "testing_cnt_shift"], axis=1, inplace=True)
  df_test_final
}
  

create_df_policy_change_tracking <- function () {
  ""
}


get_bounds_params_from_pastparams <- function(
  optimizer, parameter_list, dict_default_reinit_parameters, percentage_drift_lower_bound,
  default_lower_bound, dict_default_reinit_lower_bounds, percentage_drift_upper_bound,
  default_upper_bound, dict_default_reinit_upper_bounds,
  percentage_drift_lower_bound_annealing, default_lower_bound_annealing,
  percentage_drift_upper_bound_annealing, default_upper_bound_annealing,
  default_lower_bound_jump, default_upper_bound_jump, default_lower_bound_std_normal,
  default_upper_bound_std_normal
) { 
  
    #Generates the lower and upper bounds of the past parameters used as warm starts for the optimization process
    #to predict with DELPHI: the output depends on the optimizer used (annealing or other, i.e. tnc or trust-constr)
    #:param optimizer: optimizer used to obtain the DELPHI predictions
    #:param parameter_list: list of all past parameter values for which we want to create bounds
    #:param dict_default_reinit_parameters: dictionary with default values in case of reinitialization of parameters
    #:param percentage_drift_lower_bound: percentage of drift allowed for the lower bound
    #:param default_lower_bound: default lower bound value
    #:param dict_default_reinit_lower_bounds: dictionary with lower bounds in case of reinitialization of parameters
    #:param percentage_drift_upper_bound: percentage of drift allowed for the upper bound
    #:param default_upper_bound: default upper bound value
    #:param dict_default_reinit_upper_bounds: dictionary with upper bounds in case of reinitialization of parameters
    #:param percentage_drift_lower_bound_annealing: percentage of drift allowed for the lower bound under annealing
    #:param default_lower_bound_annealing: default lower bound value under annealing
    #:param percentage_drift_upper_bound_annealing: percentage of drift allowed for the upper bound under annealing
    #:param default_upper_bound_annealing: default upper bound value under annealing
    #:param default_lower_bound_jump: default lower bound value for the jump parameter
    #:param default_upper_bound_jump: default upper bound value for the jump parameter
    #:param default_lower_bound_std_normal: default lower bound value for the normal standard deviation parameter
    #:param default_upper_bound_std_normal: default upper bound value for the normal standard deviation parameter
    #:return: a list of bounds for all the optimized parameters based on the optimizer and pre-fixed parameters

    if ( optimizer %in% c("tnc", "trust-constr")) 
    {
      # Allowing a drift for parameters
      alpha = parameter_list[1]
      days = parameter_list[2]
      r_s = parameter_list[3]
      r_dth = parameter_list[4]
      p_dth = parameter_list[5]
      r_dthdecay = parameter_list[6]
      k1 = parameter_list[7]
      k2 = parameter_list[8]
      jump = parameter_list[9]
      t_jump = parameter_list[10]
      std_normal = parameter_list[11] 
    
      parameter_list = c(
        max(alpha, dict_default_reinit_parameters["alpha"]),
        days,
        max(r_s, dict_default_reinit_parameters["r_s"]),
        max(min(r_dth, 1), dict_default_reinit_parameters["r_dth"]),
        max(min(p_dth, 1), dict_default_reinit_parameters["p_dth"]),
        max(r_dthdecay, dict_default_reinit_parameters["r_dthdecay"]),
        max(k1, dict_default_reinit_parameters["k1"]),
        max(k2, dict_default_reinit_parameters["k2"]),
        max(jump, dict_default_reinit_parameters["jump"]),
        max(t_jump, dict_default_reinit_parameters["t_jump"]),
        max(std_normal, dict_default_reinit_parameters["std_normal"])
      )
      
      param_list_lower <- function() {
        
        tmp_parameter_list = parameter_list
        for ( i in 1:length(parameter_list) )
           tmp_parameter_list[i] <- parameter_list[i] - max(percentage_drift_lower_bound * abs(parameter_list[i]), default_lower_bound) 
      
        alpha_lower = tmp_parameter_list[1]
        days_lower = tmp_parameter_list[2]
        r_s_lower = tmp_parameter_list[3] 
        r_dth_lower = tmp_parameter_list[4] 
        p_dth_lower = tmp_parameter_list[5]
        r_dthdecay_lower = tmp_parameter_list[6]
        k1_lower = tmp_parameter_list[7] 
        k2_lower = tmp_parameter_list[8] 
        jump_lower = tmp_parameter_list[9] 
        t_jump_lower = tmp_parameter_list[10] 
        std_normal_lower = tmp_parameter_list[11]
      }
   
    
    param_list_lower = c(
      max(alpha_lower, dict_default_reinit_lower_bounds["alpha"]),
      days_lower,
      max(r_s_lower, dict_default_reinit_lower_bounds["r_s"]),
      max(min(r_dth_lower, 1), dict_default_reinit_lower_bounds["r_dth"]),
      max(min(p_dth_lower, 1), dict_default_reinit_lower_bounds["p_dth"]),
      max(r_dthdecay_lower, dict_default_reinit_lower_bounds["r_dthdecay"]),
      max(k1_lower, dict_default_reinit_lower_bounds["k1"]),
      max(k2_lower, dict_default_reinit_lower_bounds["k2"]),
      max(jump_lower, dict_default_reinit_lower_bounds["jump"]),
      max(t_jump_lower, dict_default_reinit_lower_bounds["t_jump"]),
      max(std_normal_lower, dict_default_reinit_lower_bounds["std_normal"])
    )
    
  
    param_list_upper <- function() {
      
      tmp_parameter_list = parameter_list
      for ( i in 1:length(parameter_list) )
        tmp_parameter_list[i] <- parameter_list[i] + max(percentage_drift_upper_bound * abs(parameter_list[i]), default_upper_bound) 
      
      alpha_upper = tmp_parameter_list[1]
      days_upper = tmp_parameter_list[2]
      r_s_upper = tmp_parameter_list[3] 
      r_dth_upper = tmp_parameter_list[4] 
      p_dth_upper = tmp_parameter_list[5]
      r_dthdecay_upper = tmp_parameter_list[6]
      k1_upper = tmp_parameter_list[7] 
      k2_upper = tmp_parameter_list[8] 
      jump_upper = tmp_parameter_list[9] 
      t_jump_upper = tmp_parameter_list[10] 
      std_normal_upper = tmp_parameter_list[11]
    }
    
    
  
    param_list_upper = c(
      max(alpha_upper, dict_default_reinit_upper_bounds["alpha"]),
      days_upper,
      max(r_s_upper, dict_default_reinit_upper_bounds["r_s"]),
      max(min(r_dth_upper, 1), dict_default_reinit_upper_bounds["r_dth"]),
      max(min(p_dth_upper, 1), dict_default_reinit_upper_bounds["p_dth"]),
      max(r_dthdecay_upper, dict_default_reinit_upper_bounds["r_dthdecay"]),
      max(k1_upper, dict_default_reinit_upper_bounds["k1"]),
      max(k2_upper, dict_default_reinit_upper_bounds["k2"]),
      max(jump_upper, dict_default_reinit_upper_bounds["jump"]),
      max(t_jump_upper, dict_default_reinit_upper_bounds["t_jump"]),
      max(std_normal_upper, dict_default_reinit_upper_bounds["std_normal"])
    )
  } 
  else if ( optimizer == "annealing") 
  {  # Annealing procedure for global optimization
    param_list_lower = function() {
      tmp_param_list <-  parameter_list
      for (i in 1:length(parameter_list))
        tmp_param_list[i] <- tmp_param_list[i] - max(percentage_drift_lower_bound_annealing * abs(tmp_param_list[i]), default_lower_bound_annealing) 
    }
    
      param_list_upper = function() {
          tmp_param_list <-  parameter_list
          for (i in 1:length(parameter_list))
            tmp_param_list[i] <- tmp_param_list[i] + max(percentage_drift_upper_bound_annealing * abs(tmp_param_list[i]), default_upper_bound_annealing)
        }
      
      param_list_lower[8] = default_lower_bound_jump  # jump lower bound
      param_list_upper[8] = default_upper_bound_jump  # jump upper bound
      param_list_lower[10] = default_lower_bound_std_normal  # std_normal lower bound
      param_list_upper[10] = default_upper_bound_std_normal  # std_normal upper bound     
    
  }
    else {
      stop("Optimizer {optimizer} not supported in this implementation so can't generate bounds")
    }
    
    #bounds_params = [(lower, upper) for lower, upper in zip(param_list_lower, param_list_upper)]
    #return bounds_params
  
  bounds_params <- bind_rows(as.data.frame(param_list_lower), as.data.frame(param_list_upper))
}

convert_dates_us_policies <- function(raw_date_str = "Not implemented") 
{
      #Converts dates from the dataframe with raw policies implemented in the US
      #param raw_date: a certain date string in a raw format
      #:return: a datetime in the right format for the final policy dataframe
    
  result <- NA_Date_
  if (raw_date_str != "Not implemented") 
  {
    x_long = raw_date + "20"
    result <- as.Date(x_long, format="%d-%b-%Y")
  }
  result
}

check_us_policy_data_consistency <- function(policies, df_policy_raw_us)
{
  
    #Checks consistency of the policy data in the US retrieved e.g. from IHME by verifying that 
    # if there is an end date
    #there must also be a start date for the policy implemented
    #:param policies: list of policies under consideration
    #:param df_policy_raw_us: slightly processed dataframe with policies implemented in the US
    #:return:
    
  for (policy in policies)
  {
    #assert (
    #  len(
    #    df_policy_raw_us.loc[
    #      (df_policy_raw_us[paste(policy, "_start_date"].isnull())
    #            & (~df_policy_raw_us[f"{policy}_end_date"].isnull()),
    #      :,
    #      ]
    #  )
    #  == 0
    #), stop("Problem in data, policy {policy} has no start date but has an end date")
    
    if (!is.null( df_policy_raw_us[paste(policy, "_end_date")] & 
                                   is.null( df_policy_raw_us[paste(policy, "_start_date")])  ))  
        
      stop(paste("Problem in data, policy ", policy, " has no start date but has an end date"))
    
  }

}

create_intermediary_policy_features_us <- function (df_policy_raw_us, dict_state_to_policy_dates, policies) 
{
  
  #  Processes the IHME policy data in the US to create the right intermediary features with the right names
  #  :param df_policy_raw_us: raw dataframe with policies implemented in the US
  #  :param dict_state_to_policy_dates: dictionary of the format {state: {policy: [start_date, end_date]}}
  #  :param policies: list of policies under consideration
  #  :return: an intermediary dataframe with processed columns containing binary variables as to whether or not a
  #  policy is implemented in a given state at a given date
    
  list_df_concat = {}
  date_range <- seq(  as.Date("2020-03-01"), Sys.Date() , by = "day")

  
  
  for (location in distinct(df_policy_raw_us$location_name))
  {
    #df_temp = as.data.frame(
    #  
    #    "continent": ["North America" for _ in range(len(date_range))],
    #    "country": ["US" for _ in range(len(date_range))],
    #    "province": [location for _ in range(len(date_range))],
    #    "date": date_range,
    #  }
    
    continent <- rep("North America", len(date_range))
    country <- rep("US", len(date_range))
    province <- rep(location, len(date_range))
    date <- date_range
    
    df_temp <- data.frame(continent, country, province, date)
  


    for (policy in policies) 
    {
      start_date_policy_location = dict_state_to_policy_dates[location][policy][0]
  
      start_date_policy_location <- ifelse(is.null(start_date_policy_location) | is_empty(start_date_policy_location), 
                                         "2030-01-02", start_date_policy_location)
    
    
      end_date_policy_location = dict_state_to_policy_dates[location][policy][1]

      end_date_policy_location <- ifelse(is.null(end_date_policy_location) | is_empty(end_date_policy_location), 
                                       "2030-01-01", end_date_policy_location)
    
      df_temp$policy <- ifelse (((df_temp$date >= start_date_policy_location)
                          & (df_temp$date <= end_date_policy_location)), 1, 0)
    }
    
    list_df_concat <- bind_rows(list_df_concat, df_temp)
  }
  
  #df_policies_US.rename(
  #  columns={
  #    "travel_limit": "Travel_severely_limited",
  #    "stay_home": "Stay_at_home_order",
  #    "educational_fac": "Educational_Facilities_Closed",
  #    "any_gathering_restrict": "Mass_Gathering_Restrictions",
  #    "any_business": "Initial_Business_Closure",
  #    "all_non-ess_business": "Non_Essential_Services_Closed",
  #  },
  #  inplace=True,
  #)
  
  df_policies_US <- df_policies_US %>% 
    mutate(Travel_severely_limited = travel_limit, 
           Stay_at_home_order = stay_home,
           Educational_Facilities_Closed = educational_fac,
           Mass_Gathering_Restrictions = any_gathering_restrict,
           Initial_Business_Closure = any_business,
           Non_Essential_Services_Closed = all_non-ess_business)
  
  df_policies_US
}




create_final_policy_features_us <- function (df_policies_US)
{  
  
    #Creates the final MECE policies in the US from the intermediary policies dataframe
    #:param df_policies_US: intermediary dataframe with processed columns containing binary variables as to whether or 
    #not a policy is implemented in a given state at a given date
    #:return: dataframe with the final MECE policies in the US used for DELPHI policy predictions
    
    df_policies_US_final <- df_policies_US
    msr <- future_policies
    
    #TODO : figure out what this does.
    df_policies_US_final[msr[0]] = (df_policies_US.sum(axis=1) == 0).apply(
      lambda x: int(x)
    )
    
    df_policies_US_final[msr[1]] = [
      int(a and b)
      for a, b in zip(
        df_policies_US.sum(axis=1) == 1,
        df_policies_US["Mass_Gathering_Restrictions"] == 1,
      )
      ]
    df_policies_US_final[msr[2]] = [
      int(a and b and c)
      for a, b, c in zip(
        df_policies_US.sum(axis=1) > 0,
        df_policies_US["Mass_Gathering_Restrictions"] == 0,
        df_policies_US["Stay_at_home_order"] == 0,
      )
      ]
    df_policies_US_final[msr[3]] = [
      int(a and b and c)
      for a, b, c in zip(
        df_policies_US.sum(axis=1) == 2,
        df_policies_US["Educational_Facilities_Closed"] == 1,
        df_policies_US["Mass_Gathering_Restrictions"] == 1,
      )
      ]
    df_policies_US_final[msr[4]] = [
      int(a and b and c and d)
      for a, b, c, d in zip(
        df_policies_US.sum(axis=1) > 1,
        df_policies_US["Educational_Facilities_Closed"] == 0,
        df_policies_US["Mass_Gathering_Restrictions"] == 1,
        df_policies_US["Stay_at_home_order"] == 0,
      )
      ]
    df_policies_US_final[msr[5]] = [
      int(a and b and c and d)
      for a, b, c, d in zip(
        df_policies_US.sum(axis=1) > 2,
        df_policies_US["Educational_Facilities_Closed"] == 1,
        df_policies_US["Mass_Gathering_Restrictions"] == 1,
        df_policies_US["Stay_at_home_order"] == 0,
      )
      ]
    df_policies_US_final[msr[6]] = (df_policies_US["Stay_at_home_order"] == 1).apply(
      lambda x: int(x)
    )
    df_policies_US_final["country"] = "US"
    df_policies_US_final = df_policies_US_final.loc[
      :, ["country", "province", "date"] + msr
      ]
    return df_policies_US_final
    
}

read_policy_data_us_only <- function(filepath_data_sandbox) 
  {
  #  Reads and processes the policy data from IHME to obtain the MECE policies defined for DELPHI Policy Predictions
  #  :param filepath_data_sandbox: string, path to the data sandbox drawn from the config.yml file in the main script
  #  :return: fully processed dataframe containing the MECE policies implemented in each state of the US for the full 
  #  time period necessary until the day when this function is called
  
  policies <- c(  "travel_limit", "stay_home", "educational_fac", "any_gathering_restrict",
    "any_business", "all_non.ess_business")
  
  list_US_states <- 
    c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware",
    "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
    "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
    "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
    "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
    "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
    "Washington", "West Virginia", "Wisconsin", "Wyoming") 
  


  df = read.csv(paste("data_sandbox/" , "12062020_raw_policy_data_us_only.csv", sep="" ))
  df <- df %>% mutate(location_name = as.factor(location_name),
                      peak_bed_day_mean = as.Date(peak_bed_day_mean, format="%m/%e/%Y"),
                      peak_bed_day_lower = as.Date(peak_bed_day_lower, format="%m/%e/%Y"),
                      peak_bed_day_lower = as.Date(peak_bed_day_lower, format="%m/%e/%Y"),
                      peak_bed_day_upper = as.Date(peak_bed_day_lower, format="%m/%e/%Y"),
                      peak_icu_bed_day_mean = as.Date(peak_bed_day_lower, format="%m/%e/%Y"),
                      peak_icu_bed_day_lower = as.Date(peak_bed_day_lower, format="%m/%e/%Y"),
                      peak_icu_bed_day_upper = as.Date(peak_bed_day_lower, format="%m/%e/%Y"),
                      peak_vent_day_mean = as.Date(peak_bed_day_lower, format="%m/%e/%Y"),
                      peak_vent_day_lower = as.Date(peak_bed_day_lower, format="%m/%e/%Y"),
                      peak_vent_day_upper = as.Date(peak_bed_day_lower, format="%m/%e/%Y"),
                     travel_limit_start_date = as.Date(travel_limit_start_date, format="%m/%e/%Y"),
                     travel_limit_end_date = as.Date(travel_limit_end_date, format="%m/%e/%Y"),
                     stay_home_start_date = as.Date(stay_home_start_date, format="%m/%e/%Y"),
                     stay_home_end_date = as.Date(stay_home_end_date, format="%m/%e/%Y"),
                     educational_fac_start_date = as.Date(educational_fac_start_date, format="%m/%e/%Y"),
                     educational_fac_end_date = as.Date(educational_fac_end_date, format="%m/%e/%Y"),
                     any_gathering_restrict_start_date = as.Date(any_gathering_restrict_start_date, format="%m/%e/%Y"),
                     any_gathering_restrict_end_date = as.Date(any_gathering_restrict_end_date, format="%m/%e/%Y"),
                     any_business_start_date = as.Date(any_business_start_date, format="%m/%e/%Y"),
                     any_business_end_date = as.Date(any_business_end_date, format="%m/%e/%Y") ,
                     all_non.ess_business_start_date = as.Date(all_non.ess_business_start_date,  format="%m/%e/%Y"),
                     all_non.ess_business_end_date = as.Date(all_non.ess_business_end_date,  format="%m/%e/%Y")
                      ) #%>% select( -starts_with('all_non.ess_business'))
    
  df <- df %>% filter(  location_name %in% list_US_states) %>%  mutate(location_name = 
                                                                         
                             data.frame(location_name = location_name,
                                        travel_limit_start_date = travel_limit_start_date, 
                                        travel_limit_end_date = travel_limit_end_date, 
                                        stay_home_start_date = stay_home_start_date,
                                        stay_home_end_date = stay_home_end_date, 
                                        educational_fac_start_date = educational_fac_start_date,
                                        educational_fac_end_date = educational_fac_end_date,
                                        any_gathering_restrict_start_date = any_gathering_restrict_start_date, 
                                        any_gathering_restrict_end_date = any_gathering_restrict_end_date, 
                                        any_business_start_date = any_business_start_date,
                                        any_business_end_date = any_business_end_date, 
                                        all_non.ess_business_start_date = all_non.ess_business_start_date  , 
                                        all_non.ess_business_end_date = all_non.ess_business_end_date    
                                        )) 
  
  
    
    dict_state_to_policy_dates <- tibble()
    
    #for location in df.location_name.unique():
    #  df_temp = df[df.location_name == location].reset_index(drop=True)
    #dict_state_to_policy_dates[location] = {
    #    policy: [
    #      df_temp.loc[0, f"{policy}_start_date"],
    #      df_temp.loc[0, f"{policy}_end_date"],
    #      ]
    #    for policy in policies
    #}
    
    #df_temp <- df %>% filter(location_name$location_name %in% list_US_states)
    
    df <- df %>% filter(location_name$location_name == "Florida")
    
    for (location in unique(df$location_name$location_name))
    {
      
      print(paste("location is " , location))
      if (location %in% list_US_states) 
      {
        df_location <- df %>% filter(location_name$location_name == location)
        print(paste("breaking after first location ", location))
        for (policy in policies) 
        {
          #print(paste("location is ", location))
          #print(paste("policy is ", policy))
          
          start_date_field_name = paste(policy, "_start_date", sep = "")
          end_date_field_name = paste(policy, "_end_date", sep = "")
   
          #print(paste("dates are : ", start_date_field_name, " and ", end_date_field_name))
          
          start_date <- df_location[["location_name"]][start_date_field_name][[1]]
          #print(paste("start_date is :", start_date))
          end_date <- df_location[["location_name"]][end_date_field_name][[1]]
          #print(paste("end_date is :", end_date))
          
          tbl_1 <- data.frame(location = location, 
                              policy = policy, 
                              dates =  tibble( start_date, end_date))
          
          print(tbl_1)
          dict_state_to_policy_dates <- bind_rows(dict_state_to_policy_dates, tbl_1)
        }
      }
    }
    
    check_us_policy_data_consistency(policies=policies, df_policy_raw_us=df)
    
    df_policies_US <- create_intermediary_policy_features_us(
      df_policy_raw_us=df,
      dict_state_to_policy_dates=dict_state_to_policy_dates,
      policies=policies,
    )
    
    df_policies_US_final <- create_final_policy_features_us(
      df_policies_US=df_policies_US
    )
     
   df_policies_US_final
    
}

gamma_t <- function(day, state, params_dict)
{ 
  #Computes values of our gamma(t) function that was used before the second wave modeling with the extra normal
   # distribution, but is still being used for policy predictions
  #  :param day: day on which we want to compute the value of gamma(t)
  #  :param state: string, state name
  #  :param params_dict: dictionary with format {state: (dsd, median_day_of_action, rate_of_action)}
  #  :return: value of gamma(t) for that particular state on that day and with the input parameters

    #let(dsd, median_day_of_action, rate_of_action,  params_dict[state])
    dsd <- params_dict[[state]][[1]]
    median_day_of_action <- params_dict[[state]][[2]]
    rate_of_action <- params_dict[[state]][[3]]
    #t = (day - as.Date(dsd)).days
    t <- as.numeric(difftime(day, dsd, units = "days"))

    gamma <- (2 / pi ) * atan(-1 * (t - median_day_of_action) / 20 * rate_of_action) + 1
    gamma
}

make_increasing <- function(sequence) {
  
  #  Used to force the Confidence Intervals generated for DELPHI to be always increasing
  #  :param sequence: list, sequence of values
  #  :return: list, forcefully increasing sequence of values
  sort(sequence)
}

get_normalized_policy_shifts_and_current_policy_us_only <- function( policy_data_us_only, past_parameters ) {
      # Computes the normalized policy shifts and the current policy in each state of the US
        #:param policy_data_us_only: processed dataframe with the MECE policies implemented per state for every day
        #:param past_parameters: past parameters file used for policy shift generation (specifically computation of gamma(t)
        #values in the process
        #:return: a tuple of two dictionaries, {policy: normalized_shift_float_US} and {US_state: current_policy}
        
    dict_current_policy <- dict()
    policy_list <- future_policies
    #policy_data_us_only["province_cl"] <- policy_data_us_only["province"].apply(
    #  lambda x: x.replace(",", "").strip().lower()
    #)
    
    policy_data_us_only <- policy_data_us_only %>% 
      mutate(province_cl = str_to_lower(str_trim(str_replace(province, "," ))))
    
    
    states_upper_set = set(policy_data_us_only["province"])
    for (state in states_upper_set)
    {
      dict_current_policy[("US", state)] = list(
        compress(
          policy_list,
          (
            policy_data_us_only.query("province == @state")[
              policy_data_us_only.query("province == @state")["date"]
              == policy_data_us_only.date.max()
              ][policy_list]
            == 1
          )
          .values.flatten()
          .tolist(),
        )
      )[0]
    }
    
    states_set <- set(policy_data_us_only["province_cl"])
    past_parameters_copy <- past_parameters
    #past_parameters_copy["Province"] <- past_parameters_copy["Province"].apply(
    #  lambda x: str(x).replace(",", "").strip().lower()
    #)
    
    past_parameters_copy <- past_parameters_copy %>% 
      mutate(Province = str_to_lower(str_trim(str_replace(province, "," ))))
    
    
    params_dic = {}
    for state in states_set:
      params_dic[state] = past_parameters_copy.query("Province == @state")[
        ["Data Start Date", "Median Day of Action", "Rate of Action"]
        ].iloc[0]
    
    policy_data_us_only["Gamma"] = [
      gamma_t(day, state, params_dic)
      for day, state in zip (
        policy_data_us_only["date"], policy_data_us_only["province_cl"]
      )
      ]
    
    n_measures = policy_data_us_only.iloc[:, 3:-2].shape[1]
    
    dict_normalized_policy_gamma = {
      policy_data_us_only.columns[3 + i]: policy_data_us_only[
        policy_data_us_only.iloc[:, 3 + i] == 1
        ]
      .iloc[:, -1]
      .mean()
      for i in range(n_measures)
    }
    
    normalize_val = dict_normalized_policy_gamma[policy_list[0]]
    for policy in dict_normalized_policy_gamma.keys():
      dict_normalized_policy_gamma[policy] = (
        dict_normalized_policy_gamma[policy] / normalize_val
      )
    
    vector(dict_normalized_policy_gamma, dict_current_policy)

}


get_testing_data_us <- function() 
{
    #  Function that retrieves testing data in the US from the CovidTracking website
    #  :return: a DataFrame where the column of interest is 'testing_cnt_daily'
    #  which gives the numbers of new daily tests per state
    
  df_test = read_csv("https://covidtracking.com/api/v1/states/daily.csv")
  df_test["country"] <- "US"
  df_test["continent"] <- "North America"

  df_test <- inner_join(df_test, MAPPING_STATE_CODE_TO_STATE_NAME, by = "state"  )  %>% 
    select(-state)
  
  #order the data by state and date, ascending
  df_test <- df_test %>% mutate(date = as.Date(as.character(df_test$date), format='%Y%m%d')) %>% 
    arrange(province, date) %>% select(continent, country, province, date, totalTestResults) %>%
    mutate(testing_cnt = totalTestResults) %>% select(-totalTestResults)
  
  list_df_concat = data.frame()
  
  for (state in unique(df_test$province)) {
    df_temp <- df_test %>% filter(province == state)
    df_temp <- df_temp %>% mutate(testing_cnt_shift = shift(testing_cnt, n=1)) %>%
              mutate(testing_cnt_daily = testing_cnt - testing_cnt_shift)
    df_temp$testing_cnt_daily[1] <- df_temp$testing_cnt[1]
    
    list_df_concat <- bind_rows(list_df_concat, df_temp)
    
  }
  
  df_test_final <- list_df_concat %>% select(-testing_cnt, -testing_cnt_shift)
  
  df_test_final
}

#states <- c("Florida", "Alabama")
#fl_params <- list(Sys.Date() - 30, 15, .5)
#al_params <- list( Sys.Date() = 60, 12, .3)
#test_dict <- dict()
#test_dict[[states[1]]] <- fl_params
#test_dict[[states[2]]] <- al_params


#output_test <- gamma_t( as.Date( Sys.Date()), state = "Florida", params_dict = test_dict)

us <- get_testing_data_us()
