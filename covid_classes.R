source("params.r")
source("capstone_utils.R")


DELPHIDataCreator <- setClass( 
  #setting the class name
  "DELPHIDataCreator", 
  
  
  #defining the slots
  slots = c(
    x_sol_final="list", 
    date_day_since100 = "Date",
    best_params = "list",
    continent = "character",
    country = "character",
    province = "character",
    testing_data_included = "logical"),
  
  prototype = list(
    testing_data_included = TRUE, 
    date_day_since100 = Sys.Date() - 100)
  
)

validDELPHIDataCreatorObject <- function(object) {
  
  if ((testing_data_included & (length(best_params) == 14))  |
      (!testing_data_included & (length(best_params) == 11)))
    TRUE
  
  if (testing_data_included) {
    if (length(best_params) != 14) {
      paste("Expected 9 best parameters, got ", length(best_params))
    }
  } else {
    if (length(best_params) != 11) {
      paste("Expected 7 best parameters, got ", length(best_params))
    }
  }
}
## assign the function as the validity method for the class
setValidity("DELPHIDataCreator", validDELPHIDataCreatorObject)


create_dataset_parameters <- function (object, mape) 
{
  #Creates the parameters dataset with the results from the optimization and the pre-computed MAPE
  #:param mape: MAPE a float on the last 15 days (or less if less historical days available) for that particular area
  #:return: dataframe with parameters and MAPE
  
  if (object@testing_data_included)
    print( "Parameters dataset created without the testing data parameters beta_0, beta_1: code will have to be modified")
  
  df_parameters = data.frame(
    Continent: object@continent,
    Country: object@country,
    Province: object@province,
    "Data Start Date": object@date_day_since100,
    MAPE: mape,
    "Infection Rate": object@best_params[1],
    "Median Day of Action": object@best_params[2],
    "Rate of Action": object@best_params[3],
    "Rate of Death": object@best_params[4],
    "Mortality Rate": object@best_params[5],
    "Rate of Mortality Rate Decay": object@best_params[6],
    "Internal Parameter 1": object@best_params[7],
    "Internal Parameter 2": object@best_params[8],
    "Jump Magnitude": object@best_params[9],
    "Jump Time": object@best_params[10],
    "Jump Decay": object@best_params[11]
  )
  df_parameters  
}

create_datasets_predictions <- function(object)
{ #-> (pd.DataFrame, pd.DataFrame):
      #  Creates two dataframes with the predictions of the DELPHI model, the first one since the day of the prediction,
      #  the second since the day the area had 100 cases
      #  :return: tuple of dataframes with predictions from DELPHI model
        
    n_days_btw_today_since_100 <- as.numeric(difftime(Sys.Date() , object@date_day_since100, units = "days"))
    
    n_days_since_today <- as.numeric(difftime(object@x_sol_final[[2]]  , n_days_btw_today_since_100, units = "days"))
    
    all_dates_since_today <- c()
    for (i in 1:n_days_since_today){
      temp_date <- Sys.Date() + i
      all_dates_since_today <- c(all_dates_since_today, toString(temp_date))
    }
    
    # Predictions
    total_detected <- object@x_sol_final[[16]]  # DT
    #total_detected =   [int(round(x, 0)) for x in total_detected]
    total_detected <- as.integer(total_detected)
    #active_cases = (
    #  object@x_sol_final[5, :]
    #  + object@x_sol_final[6, :]
    #  + object@x_sol_final[8, :]
    #  + object@x_sol_final[9, :]
    #)  # DHR + DQR + DHD + DQD
    
    # DHR + DQR + DHD + DQD
    active_cases <- object@x_sol_final[[5]] + object@x_sol_final[[6]]  + 
      object@x_sol_final[[8]] + object@x_sol_final[[9]]
    
    active_cases <- as.integer(active_cases)
    
    #active_cases = [int(round(x, 0)) for x in active_cases]
    #active_hospitalized = (
    #  object@x_sol_final[5, :] + object@x_sol_final[8, :]
    #)  # DHR + DHD
    
    active_hospitalized <- object@x_sol_final[[5]] + object@x_sol_final[[8]]  # DHR + DHD
    active_hospitalized <- as.integer(active_hospitalized)
    
    #active_hospitalized = [int(round(x, 0)) for x in active_hospitalized]
    #cumulative_hospitalized = object@x_sol_final[12, :]  # TH
    #cumulative_hospitalized = [int(round(x, 0)) for x in cumulative_hospitalized]
    
    cumulative_hospitalized <- object@x_sol_final[[12]] #TH
    cumulative_hospitalized <- as.integer(cumulative_hospitalized)
    
    
    #total_detected_deaths = object@x_sol_final[15, :]  # DD
    #total_detected_deaths = [int(round(x, 0)) for x in total_detected_deaths]
    
    total_detected_deaths <- object@x_sol_final[[15]] #TH
    total_detected_deaths <- as.integer(total_detected_deaths)
    
    
    
    #active_ventilated = (
    #  object@x_sol_final[13, :] + object@x_sol_final[14, :]
    #)  # DVR + DVD
    #active_ventilated = [int(round(x, 0)) for x in active_ventilated]
    
    
    active_ventilated <- object@x_sol_final[[13]] + object@x_sol_final[[14]] # DVR + DVD
    active_ventilated <- as.integer(active_ventilated)
    
    
    # Generation of the dataframe since today
    df_predictions_since_today_cont_country_prov <- data.frame
    (
      
        "Continent" = rep( object@continent, n_days_since_today), # for _ in range(n_days_since_today)],
        "Country"= rep(object@country, n_days_since_today), #for _ in range(n_days_since_today)],
        "Province"= rep(object@province, n_days_since_today), #for _ in range(n_days_since_today)],
        "Day"= all_dates_since_today,
        "Total Detected": total_detected[n_days_btw_today_since_100],
        "Active": active_cases[n_days_btw_today_since_100],
        "Active Hospitalized": active_hospitalized[n_days_btw_today_since_100],
        "Cumulative Hospitalized": cumulative_hospitalized[
          n_days_btw_today_since_100
            ],
        "Total Detected Deaths": total_detected_deaths[
          n_days_btw_today_since_100
            ],
        "Active Ventilated": active_ventilated[n_days_btw_today_since_100]
      
    )
    
    # Generation of the dataframe from the day since 100th case
    #all_dates_since_100 = [
    #  str((object@date_day_since100 + timedelta(days=i)).date())
    #  for i in range(object@x_sol_final.shape[1])
    #  ]
    
    
    all_dates_since_100 <- c()
    start_date <- as.date(object@self.date_day_since100)
    end_date <-  as.date(object@x_sol_final[[1]])
    length_of_days <- as.numeric(difftime(end_date , start_date, units = "days"))
    for (i in 1:length_of_days)
    {
      temp_date <- start_date + i
      all_dates_since_100 <- c(all_dates_since_100, toString(temp_date))
    }
    
    df_predictions_since_100_cont_country_prov = pd.DataFrame(
        "Continent" = rep(object@continent, length(all_dates_since_100)) , #[object@continent for _ in range(len(all_dates_since_100))],
        "Country"= rep(object@country, length(all_dates_since_100)), #[object@country for _ in range(len(all_dates_since_100))],
        "Province"=rep(object@province,  length(all_dates_since_100)), #[object@province for _ in range(len(all_dates_since_100))],
        "Day"= all_dates_since_100,
        "Total Detected"= total_detected,
        "Active"= active_cases,
        "Active Hospitalized"= active_hospitalized,
        "Cumulative Hospitalized"= cumulative_hospitalized,
        "Total Detected Deaths"= total_detected_deaths,
        "Active Ventilated"= active_ventilated
    )
   list(
      df_predictions_since_today_cont_country_prov,
      df_predictions_since_100_cont_country_prov,
    )
}

create_datasets_raw <- function (object) 
{
    #Creates a dataset in the right format (with values for all 16 states of the DELPHI model)
    #for the Optimal Vaccine Allocation team
    
    n_days_btw_today_since_100 <- 
     as.numeric(difftime(Sys.Date()  , object@date_day_since100, units = "days"))
  
    n_days_since_today = object@x_sol_final[[1]] - n_days_btw_today_since_100
     
     all_dates_since_today <- c()
     for (i in 1:n_days_since_today){
       temp_date <- Sys.Date() + i
       all_dates_since_today <- c(all_dates_since_today, toString(temp_date))
     }
     
    
    df_predictions_since_today_cont_country_prov = pd.DataFrame(
    
        "Continent" = object@continent, #[object@continent for _ in range(n_days_since_today)],
        "Country"= object@country, #[object@country for _ in range(n_days_since_today)],
        "Province"= object@province, # [object@province for _ in range(n_days_since_today)],
        "Day"= all_dates_since_today
    )
    
    intr_since_today <- data.frame( transpose(object@x_sol_final[[]][ n_days_btw_today_since_100]) )
    
    colnames(intr_since_today) <-  c(
      "S",      "E",      "I",      "AR",
      "DHR",      "DQR",      "AD",      "DHD",
      "DQD",      "R",      "D",      "TH",      "DVR",
      "DVD",      "DD",      "DT")
    
    #df_predictions_since_today_cont_country_prov = pd.concat(
    #  [df_predictions_since_today_cont_country_prov, intr_since_today], axis=1
    #)
    
    df_predictions_since_today_cont_country_prov <- bind_cols(df_predictions_since_today_cont_country_prov, intr_since_today)
    
    
    # Generation of the dataframe from the day since 100th case
    #all_dates_since_100 = [
    #  str((object@date_day_since100 + timedelta(days=i)).date())
    #  for i in range(object@x_sol_final.shape[1])
    #  ]
    
    all_dates_since_100 <- c()
    start_date <- as.date(object@self.date_day_since100)
    end_date <-  as.date(object@x_sol_final[[1]])
    length_of_days <- as.numeric(difftime(end_date , start_date, units = "days"))
    for (i in 1:length_of_days)
    {
      temp_date <- start_date + i
      all_dates_since_100 <- c(all_dates_since_100, toString(temp_date))
    }
    
    
    
    df_predictions_since_100_cont_country_prov <- pd.DataFrame(
        "Continent"= rep(object@continent,  length(all_dates_since_100)), # [object@continent for _ in range(len(all_dates_since_100))],
        "Country"= rep(object@country,  length(all_dates_since_100)) , #: [object@country for _ in range(len(all_dates_since_100))],
        "Province"= rep(object@province,  length(all_dates_since_100)), #: [object@province for _ in range(len(all_dates_since_100))],
        "Day": rep(all_dates_since_100,   length(all_dates_since_100))
    )
    
    #intr_since_100 = pd.DataFrame(object@x_sol_final.transpose())
    intr_since_100 <- data.frame(transpose(object@sol_final))
    
    colnames(intr_since_100) <-  c("S",   "E",    "I",   "AR",
      "DHR",  "DQR",    "AD",   "DHD",    "DQD",    "R",    "D",
      "TH",    "DVR",    "DVD",    "DD",    "DT")
    
    #df_predictions_since_100_cont_country_prov = pd.concat(
    #  [df_predictions_since_100_cont_country_prov, intr_since_100], axis=1
    #)
    
    df_predictions_since_100_cont_country_prov <- bind_cols(df_predictions_since_100_cont_country_prov, 
                                                            intr_since_100)
    
    list (
      df_predictions_since_today_cont_country_prov,
      df_predictions_since_100_cont_country_prov
    )
}

create_datasets_with_confidence_intervals <- function(
      object,
      cases_data_fit, #: list,
      deaths_data_fit, #: list,
      past_prediction_file = "I://covid19orc//danger_map//predicted//Global_V2_20200720.csv",
      past_prediction_date = "2020-07-04",
      q = 0.5,
    ) 
{     # Generates the prediction datasets from the date with 100 cases and from the day of running, including columns
          #  containing Confidence Intervals used in the website for cases and deaths
          #  :param cases_data_fit: list, contains data used to fit on number of cases
          #  :param deaths_data_fit: list, contains data used to fit on number of deaths
          #  :param past_prediction_file: past prediction file's path for CI generation
          #  :param past_prediction_date: past prediction's date for CI generation
          #  :param q: quantile used for the CIs
          #  :return: tuple of dataframes (since day of optimization & since 100 cases in the area) with predictions and
          #  confidence intervals
           
    n_days_btw_today_since_100 <- as.numeric(difftime(Sys.Date() , object@self.date_day_since100, units = "days"))
  
    
    n_days_since_today = as.numeric(self.x_sol_final[[1]]) - n_days_btw_today_since_100
    
    #all_dates_since_today = [
    #  str((datetime.now() + timedelta(days=i)).date())
    #  for i in range(n_days_since_today)
    #  ]
    
    all_dates_since_today <- c()
    for (i in 1:n_days_since_today){
      temp_date <- Sys.Date() + i
      all_dates_since_today <- c(all_dates_since_today, toString(temp_date))
    }
    
    
    # Predictions
    #total_detected = self.x_sol_final[15, :]  # DT
    
    total_detected <- object@x_sol_final[[16]]  # DT
    
    #total_detected = [int(round(x, 0)) for x in total_detected]
    total_detected <- as.integer(total_detected)

    
    active_cases <- object@x_sol_final[[5]] + object@x_sol_final[[6]]  + 
      object@x_sol_final[[8]] + object@x_sol_final[[9]]  # DHR + DQR + DHD + DQD
    
    active_cases <- as.integer(active_cases)
    
    active_hospitalized <- object@x_sol_final[[5]] + object@x_sol_final[[8]]  # DHR + DHD
    active_hospitalized <- as.integer(active_hospitalized)
    
    
    cumulative_hospitalized <- object@x_sol_final[[12]] #TH
    cumulative_hospitalized <- as.integer(cumulative_hospitalized)
    
    total_detected_deaths <- object@x_sol_final[[15]] #TH
    total_detected_deaths <- as.integer(total_detected_deaths)
    
    
    active_ventilated <- object@x_sol_final[[13]] + object@x_sol_final[[14]] # DVR + DVD
    active_ventilated <- as.integer(active_ventilated)
    
    past_predictions = read_csv(past_prediction_file)
    #past_predictions = (
    #  past_predictions[
    #    (past_predictions["Day"] > past_prediction_date)
    #    & (past_predictions["Country"] == self.country)
    #    & (past_predictions["Province"] == self.province)
    #    ]
    #).sort_values("Day")
    
    past_predictions <- past_predictions %>% 
      filter(Day > past_prediction_date & Country == object@country & Province == object@province) %>%
      arrange(Day)
    
    if (nrow(past_predictions) > 0)
    {
      #known_dates_since_100 = [
      #  str((self.date_day_since100 + timedelta(days=i)).date())
      #  for i in range(len(cases_data_fit))
      #  ]
      
      known_dates_since_100 <- c()
      for (i in 1:length(cases_data_fit)) 
      {
        temp_date <- Sys.Date() + i
        known_dates_since_100 <- c(known_dates_since_100, toString(temp_date))
      }
        
      
      #cases_data_fit_past = [
      #  y
      #  for x, y in zip(known_dates_since_100, cases_data_fit)
      #  if x > past_prediction_date
      #  ]
      
      zipped_data <- data.frame(x = known_dates_since_100, y = cases_data_fit)
      cases_data_fit_past <- zipped_data %>% filter(x > past_prediction_date) %>% select(y) %>% as.list()
      
      
      death_zipped_data <- data.frame(x = known_dates_since_100, y = deaths_data_fit)
      #deaths_data_fit_past = [
      #  y
      #  for x, y in zip(known_dates_since_100, deaths_data_fit)
      #  if x > past_prediction_date
      #  ]
      
      deaths_data_fit_past <- death_zipped_data %>% filter(x > past_prediction_date) %>% select(y)%>% as.list()
      
      
      #total_detected_past = past_predictions["Total Detected"].values[
      #  : len(cases_data_fit_past)
      #  ]
      
      total_detected_past <- past_predictions["Total Detected"][1:length(cases_data_fit_past)]
      
      #total_detected_deaths_past = past_predictions[
      #  "Total Detected Deaths"
      #  ].values[: len(deaths_data_fit_past)]
      
      total_detected_deaths_past <- past_predictions["Total Detected Deaths"][1:length(deaths_data_fit_past)]
      
      #residual_cases_lb = np.sqrt(
      #  np.mean(
      #    [(x - y) ** 2 for x, y in zip(cases_data_fit_past, total_detected_past)]
      #  )
      #) * scipy.stats.norm.ppf(0.5 - q / 2)
      
      
      calc_data <- data.frame(cases_data_fit_past, total_detected_past)
      #mean squared error calculations
      calc_data <- calc_data %>% 
        mutate(residual_cases_mean_sq = (as.numeric(cases_data_fit_past) - as.numeric(total_detected_past))**2)
      
      residual_cases_lb <- sqrt( mean(calc_data$residual_cases_ouput)  ) * qnorm(0.5 - q / 2)
      residual_cases_ub <- sqrt( mean(calc_data$residual_cases_ouput)  ) * qnorm(0.5 + q / 2)
      
      calc_data["deaths_data_fit_past"] <- deaths_data_fit_past
      calc_data["total_detected_deaths_past"] <- total_detected_deaths_past
      
      calc_data <- calc_data %>% mutate(residual_deaths_mean_sq = (as.numeric(x) - as.numeric(y))**2)
      
      residual_deaths_lb <- sqrt( mean(calc_data$residual_deaths_mean_sq)  ) * qnorm(0.5 - q / 2)
      residual_deaths_ub <- sqrt( mean(calc_data$residual_deaths_mean_sq)  ) * qnorm(0.5 + q / 2)
    
    
      #make_increasing
      total_detected_LB_column <- c()
      total_detected_UB_column <- c()
      total_detected_death_column_lb <- c()
      total_detected_death_column_ub <- c()
      
      for(c in n_days_btw_today_since_100:length(total_detected))
      {
          #total detected lower & upper bound
          v <- total_detected[c]
          tmp_lb <- max(int(round(v + residual_cases_lb * sqrt(c), 0)), 0)
          tmp_ub <- max(int(round(v + residual_cases_ub * sqrt(c), 0)), 0)
          total_detected_LB_column <- c(total_detected_LB_column, tmp_lb)
          total_detected_UB_column <- c(total_detected_UB_column, tmp_ub)
          #total deaths lb & up
          v <- total_detected_deaths[c]
          tmp_lb <- max(int(round(v + residual_deaths_lb * sqrt(c), 0)), 0)
          tmp_ub <- max(int(round(v + residual_deaths_ub * sqrt(c), 0)), 0)
          total_detected_death_column_lb <- c(total_detected_death_column_lb, tmp_lb)
          total_detected_death_column_ub <- c(total_detected_death_column_ub, tmp_ub)
      }  
      total_detected_LB_column <- make_increasing(total_detected_LB_column)
      
      # Generation of the dataframe since today
      df_predictions_since_today_cont_country_prov <- data.frame(
      
          "Continent" = rep(object@continent, n_days_since_today), #: [self.continent for _ in range(n_days_since_today)],
          "Country" =  rep(object@country, n_days_since_today), #: [self.country for _ in range(n_days_since_today)],
          "Province"=  rep(object@province, n_days_since_today),#: [self.province for _ in range(n_days_since_today)],
          "Day"= all_dates_since_today,
          "Total Detected"= total_detected[n_days_btw_today_since_100:length(total_detected)],
          "Active"= active_cases[n_days_btw_today_since_100:length(active_cases)],
          "Active Hospitalized"= active_hospitalized[ n_days_btw_today_since_100: length(active_hospitalized) ],
          "Cumulative Hospitalized"= cumulative_hospitalized[  n_days_btw_today_since_100: length(cumulative_hospitalized) ],
          "Total Detected Deaths"= total_detected_deaths[   n_days_btw_today_since_100:length(total_detected_deaths) ],
          "Active Ventilated"= active_ventilated[n_days_btw_today_since_100:length(active_ventilated)],
          "Total Detected True" = rep(NaN, n_days_since_today),
          "Total Detected Deaths True" = rep(NaN, n_days_since_today), #[ np.nan for _ in range(n_days_since_today)  ],
          "Total Detected LB" = total_detected_LB_column, 
          "Total Detected Deaths LB" = total_detected_death_column_lb,
          "Total Detected UB" = total_detected_UB_column,
          "Total Detected Deaths UB" = total_detected_death_column_ub
        )
      

        # Generation of the dataframe from the day since 100th case
        
        all_dates_since_100 <- c()
        start_date <- as.date(object@self.date_day_since100)
        end_date <-  as.date(object@x_sol_final[[1]])
        length_of_days <- as.numeric(difftime(end_date , start_date, units = "days"))
        for (i in 1:length_of_days)
        {
          temp_date <- start_date + i
          all_dates_since_100 <- c(all_dates_since_100, toString(temp_date))
        }
        
        
        #make_increasing
        total_detected_LB_column <- c()
        total_detected_UB_column <- c()
        total_detected_death_column_lb <- c()
        total_detected_death_column_ub <- c()
        
        for(c in n_days_btw_today_since_100:length(total_detected))
        {
          #total detected lower & upper bound
          v <- total_detected[c]
          tmp_lb <- max(int(round(v + residual_cases_lb * sqrt(max(c - n_days_btw_today_since_100, 0)), 0,)), 0)
          tmp_ub <- max(int(round(v + residual_cases_ub * sqrt(max(c - n_days_btw_today_since_100, 0)), 0,)), 0)
          total_detected_LB_column <- c(total_detected_LB_column, tmp_lb)
          total_detected_UB_column <- c(total_detected_UB_column, tmp_ub)
          #total deaths lb & up
          v <- total_detected_deaths[c]
          tmp_lb <- max( int( round(  v + residual_deaths_lb * sqrt(max(c - n_days_btw_today_since_100, 0)), 0, ) ), 0 )
          tmp_ub <- max( int( round(  v + residual_deaths_ub * sqrt(max(c - n_days_btw_today_since_100, 0)), 0, ) ), 0 )
          total_detected_death_column_lb <- c(total_detected_death_column_lb, tmp_lb)
          total_detected_death_column_ub <- c(total_detected_death_column_ub, tmp_ub)
        }  
        total_detected_LB_column <- make_increasing(total_detected_LB_column)
        total_detected_UB_column <- make_increasing(total_detected_UB_column)
        total_detected_death_column_lb <- make_increasing(total_detected_death_column_lb)
        total_detected_death_column_ub <- make_increasing(total_detected_death_column_ub)
        
        df_predictions_since_100_cont_country_prov <- data.frame
        (
            "Continent" = rep(objecet@continent, length(all_dates_since_100)),
            "Country"=  rep(objecet@country, length(all_dates_since_100)),  #[self.country for _ in range(len(all_dates_since_100))],
            "Province" =  rep(objecet@province, length(all_dates_since_100)),   #: [  self.province for _ in range(len(all_dates_since_100))  ],
            "Day" = all_dates_since_100,
            "Total Detected" = total_detected,
            "Active" = active_cases,
            "Active Hospitalized"= active_hospitalized,
            "Cumulative Hospitalized"= cumulative_hospitalized,
            "Total Detected Deaths"= total_detected_deaths,
            "Active Ventilated"= active_ventilated,
            "Total Detected True" = c( unlist( cases_data_fit), rep(NaN, (len(all_dates_since_100) - len(cases_data_fit)))),
            "Total Detected Deaths True" = c(unlist( deaths_data_fit), rep(NaN, (len(all_dates_since_100) - len(deaths_data_fit)))),
            "Total Detected LB" = total_detected_LB_column , 
            "Total Detected Deaths LB" = total_detected_death_column_lb,
            "Total Detected UB" = total_detected_death_column_ub
        )
    
    }
    else 
    {
      df_predictions_since_today_cont_country_prov = pd.DataFrame(
        
          "Continent" = rep(object@continent,  n_days_since_today ), #: [self.continent for _ in range(n_days_since_today)],
          "Country" = rep(object@country,  n_days_since_today ), #: [self.country for _ in range(n_days_since_today)],
          "Province"= rep(object@province,  n_days_since_today ), #: [self.province for _ in range(n_days_since_today)],
          "Day" =  all_dates_since_today,
          "Total Detected"= total_detected[n_days_btw_today_since_100:length(total_detected)],
          "Active"= active_cases[n_days_btw_today_since_100:length(active_cases)],
          "Active Hospitalized": active_hospitalized[  n_days_btw_today_since_100: length(active_hospitalized)   ],
          "Cumulative Hospitalized"= cumulative_hospitalized[   n_days_btw_today_since_100: length(cumulative_hospitalized)   ],
          "Total Detected Deaths"= total_detected_deaths[  n_days_btw_today_since_100:  length(total_detected_deaths)  ],
          "Active Ventilated"= active_ventilated[n_days_btw_today_since_100:length(active_ventilated)],
          "Total Detected True" = rep(NaN, n_days_since_today),  #: [np.nan for _ in range(n_days_since_today)],
          "Total Detected Deaths True" = rep(NaN, n_days_since_today),  #: [  np.nan for _ in range(n_days_since_today)  ],
          "Total Detected LB"  = rep(NaN, n_days_since_today), #: [np.nan for _ in range(n_days_since_today)],
 
          "Total Detected Deaths LB" = rep(NaN, n_days_since_today), #: [ np.nan for _ in range(n_days_since_today)  ],
          "Total Detected UB" = rep(NaN, n_days_since_today), #: [np.nan for _ in range(n_days_since_today)],
          "Total Detected Deaths UB" = rep(NaN, n_days_since_today) #: [   np.nan for _ in range(n_days_since_today)   ]
      )
 
       # Generation of the dataframe from the day since 100th case
       # all_dates_since_100 = [
      #    str((self.date_day_since100 + timedelta(days=i)).date())
      #    for i in range(self.x_sol_final.shape[1])
       #   ]
        
        all_dates_since_100 <- c()
        start_date <- as.date(object@self.date_day_since100)
        end_date <-  as.date(object@x_sol_final[[1]])
        length_of_days <- as.numeric(difftime(end_date , start_date, units = "days"))
        for (i in 1:length_of_days)
        {
          temp_date <- start_date + i
          all_dates_since_100 <- c(all_dates_since_100, toString(temp_date))
        }
        
        
        df_predictions_since_100_cont_country_prov <- data.frame(
          
            "Continent"= rep(object@continent, all_dates_since_100), #: [ self.continent for _ in range(len(all_dates_since_100)) ],
            "Country"= rep(object@country, all_dates_since_100), #:     [ self.country for _ in range(len(all_dates_since_100))],
            "Province" =  rep(object@province, all_dates_since_100),  #:[ self.province for _ in range(len(all_dates_since_100)) ],
            "Day" = all_dates_since_100,
            "Total Detected" = total_detected,
            "Active" = active_cases,
            "Active Hospitalized" = active_hospitalized,
            "Cumulative Hospitalized" = cumulative_hospitalized,
            "Total Detected Deaths" = total_detected_deaths,
            "Active Ventilated" = active_ventilated,
            "Total Detected True"= c(unlist(cases_data_fit), rep(NaN, length(all_dates_since_100) - length(cases_data_fit))),
            #+ [ np.nan  for _ in range(len(all_dates_since_100) - len(cases_data_fit))  ],
            "Total Detected Deaths True" =
              c(unlist(deaths_data_fit), rep(NaN, length(all_dates_since_100) - length(cases_data_fit))),  #: deaths_data_fit + [ np.nan for _ in range(len(all_dates_since_100) - len(deaths_data_fit)) ],
            "Total Detected LB" =  rep(NaN, all_dates_since_100), #: [   np.nan for _ in range(len(all_dates_since_100))   ],
            "Active LB" =  rep(NaN, all_dates_since_100), #: [np.nan for _ in range(len(all_dates_since_100))],
            "Active Hospitalized LB" =  rep(NaN, all_dates_since_100), #: [  np.nan for _ in range(len(all_dates_since_100))  ],
            "Cumulative Hospitalized LB" =  rep(NaN, all_dates_since_100), #: [  np.nan for _ in range(len(all_dates_since_100))  ],
            "Total Detected Deaths LB" =  rep(NaN, all_dates_since_100), #: [   np.nan for _ in range(len(all_dates_since_100))  ],
            "Active Ventilated LB"=  rep(NaN, all_dates_since_100), #: [  np.nan for _ in range(len(all_dates_since_100))  ],
            "Total Detected UB"=  rep(NaN, all_dates_since_100), #: [  np.nan for _ in range(len(all_dates_since_100))  ],
            "Active UB"=  rep(NaN, all_dates_since_100), #: [np.nan for _ in range(len(all_dates_since_100))],
            "Active Hospitalized UB"=  rep(NaN, all_dates_since_100), #: [   np.nan for _ in range(len(all_dates_since_100))   ],
            "Cumulative Hospitalized UB"=  rep(NaN, all_dates_since_100), #: [  np.nan for _ in range(len(all_dates_since_100))  ],
            "Total Detected Deaths UB"=  rep(NaN, all_dates_since_100), #: [   np.nan for _ in range(len(all_dates_since_100))  ],
            "Active Ventilated UB"=  rep(NaN, all_dates_since_100) #: [  np.nan for _ in range(len(all_dates_since_100))  ],
        )
    }
        
    list (
      df_predictions_since_today_cont_country_prov,
      df_predictions_since_100_cont_country_prov,
    ) 
}

create_datasets_predictions_scenario <- function(  object, policy = "Lockdown", time =  0, totalcases=NaN   )
{
      n_days_btw_today_since_100 <- #( Sys.Date()  - as.Date(object@date_day_since100).days
        as.numeric(difftime(Sys.Date()  , as.Date(object@date_day_since100), units = "days"))
      #n_days_since_today = self.x_sol_final.shape[1] - n_days_btw_today_since_100
      n_days_since_today <- as.numeric(difftime(object@x_sol_final[[2]]  , n_days_btw_today_since_100, units = "days"))
      #all_dates_since_today = [
      #  str((datetime.now() + timedelta(days=i)).date())
      #  for i in range(n_days_since_today)
      #  ]
      
      all_dates_since_today <- c()
      for (i in 1:n_days_since_today){
        temp_date <- Sys.Date() + i
        all_dates_since_today <- c(all_dates_since_today, toString(temp_date))
      }
      
      # Predictions
      total_detected <- unlist(self.x_sol_final[[16]]) #, :]  # DT
      total_detected <- as.integer(unlist(total_detected)) #= [int(round(x, 0)) for x in total_detected]
      active_cases <- c(
        unlist(object@x_sol_final[[5]]),
        unlist(object@x_sol_final[[6]]),
        unlist(object@x_sol_final[[8]]),
        unlist( object@x_sol_final[[9]])
      )  # DHR + DQR + DHD + DQD
      active_cases <- as.integer(active_cases) #= [int(round(x, 0)) for x in active_cases]
      active_hospitalized <- c(unlist(object@x_sol_final[[5]]),  object@x_sol_final[[8]] )  # DHR + DHD
              #= (  self.x_sol_final[4, :] + self.x_sol_final[7, :]  )  # DHR + DHD
      active_hospitalized <- #= [int(round(x, 0)) for x in active_hospitalized]
      cumulative_hospitalized <- as.integer(unlist(object@sol_final[[11]])) #= self.x_sol_final[11, :]  # TH
     # cumulative_hospitalized  = [int(round(x, 0)) for x in cumulative_hospitalized]
      total_detected_deaths <- as.integer(unlist(object@x_sol_final[[14]])) #  = self.x_sol_final[14, :]  # DD
      #total_detected_deaths = [int(round(x, 0)) for x in total_detected_deaths]
      active_ventilated <- as.integer(c(unlist(object@x_sol_final[[13]]), unlist(object@x_sol_final[[14]])))  #= (  self.x_sol_final[12, :] + self.x_sol_final[13, :])  # DVR + DVD
      #active_ventilated = [int(round(x, 0)) for x in active_ventilated]
      # Generation of the dataframe since today
      df_predictions_since_today_cont_country_prov = data.frame(
        
          "Policy" = rep(policy, n_days_since_today), #: [policy for _ in range(n_days_since_today)],
          "Time" = rep(TIME_DICT[[time]], n_days_since_today) , #: [TIME_DICT[time] for _ in range(n_days_since_today)],
          "Continent" = rep(object@continent, n_days_since_today), #: [self.continent for _ in range(n_days_since_today)],
          "Country" = rep(object@country, n_days_since_today), #: [self.country for _ in range(n_days_since_today)],
          "Province" = rep(object@province, n_days_since_today), #: [self.province for _ in range(n_days_since_today)],
          "Day" = all_dates_since_today,
          "Total Detected" =  total_detected[n_days_btw_today_since_100:length(total_detected)],
          "Active" =  active_cases[n_days_btw_today_since_100:length(active_cases)],
          "Active Hospitalized" =  active_hospitalized[n_days_btw_today_since_100:length(active_hospitalized)],
          "Cumulative Hospitalized" =  cumulative_hospitalized[ n_days_btw_today_since_100:length(cumulative_hospitalized)],
          "Total Detected Deaths"= total_detected_deaths[  n_days_btw_today_since_100: length(total_detected_deaths) ],
          "Active Ventilated"= active_ventilated[n_days_btw_today_since_100:length(active_ventilated)]
      )
      
      # Generation of the dataframe from the day since 100th case
      #all_dates_since_100 = [
      #  str((self.date_day_since100 + timedelta(days=i)).date())
      #  for i in range(self.x_sol_final.shape[1])
      #  ]
      
      all_dates_since_100 <- c()
      start_date <- as.date(object@self.date_day_since100)
      end_date <-  as.date(object@x_sol_final[[1]])
      length_of_days <- as.numeric(difftime(end_date , start_date, units = "days"))
      for (i in 1:length_of_days)
      {
        temp_date <- start_date + i
        all_dates_since_100 <- c(all_dates_since_100, toString(temp_date))
      }
      
      
      
      df_predictions_since_100_cont_country_prov = data.frame
      (
        
          "Policy"= rep(policy, all_dates_since_100), #: [policy for _ in range(len(all_dates_since_100))],
          "Time" = rep(TIME_DICT[[time]], all_dates_since_100) , #: [TIME_DICT[time] for _ in range(len(all_dates_since_100))],
          "Continent" = rep(object@continent, all_dates_since_100), #: [self.continent for _ in range(len(all_dates_since_100))],
          "Country"= rep(object@country, all_dates_since_100), #: [self.country for _ in range(len(all_dates_since_100))],
          "Province" = rep(object@province, all_dates_since_100), #: [self.province for _ in range(len(all_dates_since_100))],
          "Day"= all_dates_since_100,
          "Total Detected"= total_detected,
          "Active"= active_cases,
          "Active Hospitalized"= active_hospitalized,
          "Cumulative Hospitalized"= cumulative_hospitalized,
          "Total Detected Deaths"= total_detected_deaths,
          "Active Ventilated"= active_ventilated
      )
      
      
      if ( !is.nan(  totalcases )) #is not None
      {
        # Merging the historical values to both dataframes when available
        #df_predictions_since_today_cont_country_prov = df_predictions_since_today_cont_country_prov.merge(
        #  totalcases[
        #    ["country", "province", "date", "case_cnt", "death_cnt"]
        #    ].fillna("None"),
        #  left_on=["Country", "Province", "Day"],
        #  right_on=["country", "province", "date"],
        #  how="left",
        #)
        
        df_predictions_since_today_cont_country_prov <- 
          left_join(df_predictions_since_today_cont_country_prov, totalcases, by = c(
            "Country" = "country", "Province" = "province", "Day" = "date"
          ) )
        
        df_predictions_since_today_cont_country_prov <- df_predictions_since_today_cont_country_prov %>%
          mutate("Total Detected True" = "case_cnt", "Total Detected Deaths True" = "death_cnt") %>%
          select (-"country", -"province", -"date", -"case_cnt", -"death_cnt")
          
        
          #df_predictions_since_today_cont_country_prov.rename(
          #  columns={
          #    "case_cnt": "Total Detected True",
          #    "death_cnt": "Total Detected Deaths True",
          #  },
          #  inplace=True,
          #)
          #df_predictions_since_today_cont_country_prov.drop(
          #  ["country", "province", "date"], axis=1, inplace=True
          #)
          #df_predictions_since_100_cont_country_prov = df_predictions_since_100_cont_country_prov.merge(
          #  totalcases[
          #    ["country", "province", "date", "case_cnt", "death_cnt"]
          #    ].fillna("None"),
          #  left_on=["Country", "Province", "Day"],
          #  right_on=["country", "province", "date"],
          #  how="left",
          #)
          
          
          df_predictions_since_100_cont_country_prov <- 
            left_join(df_predictions_since_100_cont_country_prov, totalcases, by = c(
              "Country" = "country", "Province" = "province", "Day" = "date"
            ) )
          
          df_predictions_since_100_cont_country_prov <- df_predictions_since_100_cont_country_prov %>%
            mutate("Total Detected True" = "case_cnt", "Total Detected Deaths True" = "death_cnt") %>%
              select (-"country", -"province", -"date", -"case_cnt", -"death_cnt")
          
          
          #df_predictions_since_100_cont_country_prov.rename(
          #  columns={
          #    "case_cnt": "Total Detected True",
          #    "death_cnt": "Total Detected Deaths True",
          #  },
          #  inplace=True,
          #)
          #df_predictions_since_100_cont_country_prov.drop(
          #  ["country", "province", "date"], axis=1, inplace=True
          #)
      }
     list(
        df_predictions_since_today_cont_country_prov,
        df_predictions_since_100_cont_country_prov
      )
}

get_aggregation_per_country <- function(df_predictions) 
{
        #Aggregates predictions at the country level from the predictions dataframe
        #:param df_predictions: DELPHI predictions dataframe
        #:return: DELPHI predictions dataframe aggregated at the country level
        
    #df_predictions = df_predictions[df_predictions["Province"] != "None"]
    df_predictions <- df_predictions %>% filter(Province != "None")
    #df_agg_country = df_predictions.groupby(["Continent", "Country", "Day"]).sum().reset_index()
    #WHAT ARE WE SUMMARIZING HERE
    df_agg_country <- df_predictions %>% group_by("Continent", "Country", "Day") %>% summarise(sum = sum())
    df_agg_country["Province"] <- "None"
    df_agg_country <- df_agg_country[names(df_predictions)]
    df_agg_country
}

get_aggregation_per_continent <- function(df_predictions) 
{
  #       Aggregates predictions at the continent level from the predictions dataframe
  #      :param df_predictions: DELPHI predictions dataframe
  #      :return: DELPHI predictions dataframe aggregated at the continent level
      
  #df_agg_continent = df_predictions.groupby(["Continent", "Day"]).sum().reset_index()
  df_agg_continent <- df_predictions %>% group_by("Continent", "Day") %>% summarize()
  df_agg_continent["Country"] <- "None"
  df_agg_continent["Province"] <- "None"
  df_agg_continent <- df_agg_continent[names(df_predictions)]
  df_agg_continent
}

get_aggregation_world(df_predictions) 
{
  #Aggregates predictions at the world level from the predictions dataframe
  #      :param df_predictions: DELPHI predictions dataframe
  #      :return: DELPHI predictions dataframe aggregated at the world level (only one row in this dataframe)
   
  #df_agg_world = df_predictions.groupby("Day").sum().reset_index()
  df_agg_world <-  df_predictions %>% group_by("Day") %>% summarise()
  df_agg_world["Continent"] <- "None"
  df_agg_world["Country"] <- "None"
  df_agg_world["Province"] <- "None"
  df_agg_world <- df_agg_world[names(df_predictions)]
  df_agg_world
}

append_all_aggregations(df_predictions: pd.DataFrame) {

      # Creates and appends all the predictions' aggregations at the country, continent and world levels
      #  :param df_predictions: dataframe with the raw predictions from DELPHI
      #  :return: dataframe with raw predictions from DELPHI and aggregated ones at the country, continent & world levels
       
      df_agg_since_today_per_country <- get_aggregation_per_country( df_predictions )
      df_agg_since_today_per_continent <- get_aggregation_per_continent(  df_predictions)
      df_agg_since_today_world <- get_aggregation_world(df_predictions)
      #df_predictions = pd.concat(
      #  [
      #    df_predictions,
      #    df_agg_since_today_per_country,
      #    df_agg_since_today_per_continent,
      #    df_agg_since_today_world,
      #    ]
      #)
      
      df_predictions <- bind_rows(df_predictions,   df_agg_since_today_per_country, 
                                  df_agg_since_today_per_continent,   df_agg_since_today_world,)
      
      
      #df_predictions.sort_values(["Continent", "Country", "Province", "Day"], inplace=True)
      df_predictions <- df_predictions %>% arrange("Continent", "Country", "Province", "Day")
      df_predictions
}


 get_aggregation_per_country_with_cf <- function(
  df_predictions,  past_prediction_file =  "I://covid19orc//danger_map//predicted//Global_V2_20200720.csv",
  past_prediction_date = "2020-07-04",   q  = 0.5) 
 {
     #   Creates aggregations at the country level as well as associated confidence intervals
    #    :param df_predictions: dataframe containing the raw predictions from the DELPHI model
    #    :param past_prediction_file: past prediction file's path for CI generation
    #    :param past_prediction_date: past prediction's date for CI generation
    #    :param q: quantile used for the CIs
    #    :return: dataframe with country level aggregated predictions & associated confidence intervals
     
      df_predictions <- df_predictions %>% filtere("Province" != "None")
      #columns_without_bounds = [x for x in df_predictions.columns if ("LB" not in x) and ("UB" not in x)]
      columns_without_bounds <- c()
      for ( x in names(df_predictions)) {
        if (str_count(x, pattern = "LB|LB") == 0 )
          columns_without_bounds <- c(columns_without_bounds, x)
      }
      
      #df_agg_country = df_predictions[columns_without_bounds].groupby(["Continent", "Country", "Day"]).sum(min_count = 1).reset_index()
      df_agg_country <- df_predictions %>% group_by(["Continent", "Country", "Day"]) %>%
        summarize(sum = sum( na.rm = TRUE)) %>% select(columns_without_bounds)
      
    
      df_agg_country["Province"] <- "None"
      
      df_agg_country <- df_agg_country[columns_without_bounds]
      
      #aggregated_countries = set(zip(df_agg_country["Country"],df_agg_country["Province"]))
      
      aggregated_countries <- df_agg_country %>% select(distinct(Country, Province))
      
      past_predictions = pd.read_csv(past_prediction_file)
      
      list_df_aggregated_countries = list()
      #for country, province in aggregated_countries:
      for(row in 1:nrow(aggregated_countries))  
      {
          country = aggregated_countries[row, "Country"]
          province = aggregated_countries[row, "Province"]
          #past_predictions_temp <- (past_predictions[(past_predictions['Day'] > past_prediction_date) & 
          #                                          (past_predictions['Country'] == country) & (past_predictions['Province'] == province)]).sort_values("Day")
          
          past_predictions_temp <- past_predictions %>% 
                filter(Day > past_prediction_date & Country == country & Province == province) %>%
                    arrange(Day)
      
          #df_agg_country_temp = (df_agg_country[(df_agg_country['Country'] == country) & (df_agg_country['Province'] == province)]).sort_values("Day").reset_index(drop=True)
          
          df_agg_country_temp <- df_agg_country %>% filter(Country == country & Province == province) %>% arrange(Day)
          
          total_detected <- df_agg_country_temp['Total Detected'] 
          total_detected_deaths <- df_agg_country_temp['Total Detected Deaths'] 
          #            active_cases = df_agg_country_temp['Active'] 
          #            active_hospitalized = df_agg_country_temp['Active Hospitalized'] 
          #            cumulative_hospitalized = df_agg_country_temp['Cumulative Hospitalized'] 
          #            active_ventilated = df_agg_country_temp['Active Ventilated'] 
          cases_fit_data <- df_agg_country_temp['Total Detected True'] 
          deaths_fit_data <- df_agg_country_temp['Total Detected Deaths True'] 
          since_100_dates <- df_agg_country_temp['Day'] 
          
          #n_days_btw_today_since_100 = (datetime.now() - pd.to_datetime(min(since_100_dates))).days
          
          n_days_btw_today_since_100 <- as.numeric(difftime(Sys.Date()  , as.Date(since_100_dates), units = "days"))
          
          cases_fit_data_past <- c()
          if (nrow(past_predictions_temp) > 0) {
            #zip <- data.frame(since_100_dates, cases_fit_data)
            #cases_fit_data_past = [y for x, y in zip(since_100_dates,cases_fit_data) if ((x > past_prediction_date) and (not np.isnan(y)))]
            
            cases_fit_data_past <- data.frame(x = since_100_dates, y = cases_fit_data) %>% 
              filter(x > past_prediction_date & ! is.nan(y)) %>% select(y)
            
            
            #deaths_fit_data_past = [y for x, y in zip(since_100_dates,deaths_fit_data) if ((x > past_prediction_date) and (not np.isnan(y)))]
            
            deaths_fit_data_past <- data.frame(x = since_100_dates, y = cases_fit_data) %>% filter(x > past_prediction_date & !is.nan(y)) %>% select(y)
            
            #total_detected_past = past_predictions_temp["Total Detected"].values[:len(cases_fit_data_past)]
            
            total_detected_past <- past_predictions_temp["Total Detected"][1:length(cases_fit_data_past)]
            
            #total_detected_deaths_past = past_predictions_temp["Total Detected Deaths"].values[:len(deaths_fit_data_past)]
            
            total_detected_deaths_past <- past_predictions_temp["Total Detected Deaths"][1:length(deaths_fit_data_past)]
            
            #residual_cases_lb = np.sqrt(np.mean([(x- y) ** 2 for x,y in zip(cases_fit_data_past,total_detected_past)])) * scipy.stats.norm.ppf(0.5 - q /2)
            #residual_cases_ub = np.sqrt(np.mean([(x- y) ** 2 for x,y in zip(cases_fit_data_past,total_detected_past)])) * scipy.stats.norm.ppf(0.5 + q /2)
            
            rt_mn_sq_residual_cases <- data.frame(x=cases_fit_data_past, y=total_detected_past) %>% 
                  summarise(s = sqrt(mean( (x-y)^2) )) %>% as.numeric()
            
            residual_cases_lb <- rt_mn_sq_residual_cases * qnorm(0.5 - q / 2)
            residual_deaths_ub <- rt_mn_sq_residual_cases * qnorm(0.5 + q / 2)
            
            rt_mn_sq_death_cases <- data.frame(x=deaths_fit_data_past, y=total_detected_deaths_past) %>% 
                  summarise(s = sqrt(mean( (x-y)^2) )) %>% as.numeric()
          
            #residual_deaths_lb = np.sqrt(np.mean([(x- y) ** 2 for x,y in zip(deaths_fit_data_past,total_detected_deaths_past)])) * scipy.stats.norm.ppf(0.5 - q /2)
            #residual_deaths_ub = np.sqrt(np.mean([(x- y) ** 2 for x,y in zip(deaths_fit_data_past,total_detected_deaths_past)])) *  scipy.stats.norm.ppf(0.5 + q /2)
            
            residual_cases_lb <- rt_mn_sq_death_cases * qnorm(0.5 - q / 2)
            residual_deaths_ub <- rt_mn_sq_death_cases * qnorm(0.5 + q / 2)
            
            
                        
            # Generation of the dataframe from the day since 100th case
            df_predictions_since_100_cont_country_prov = pd.DataFrame(
              "Total Detected LB"= data.frame(c = c(1:length(total_detected), v = total_detected)) %>%   rowwise() %>%
                mutate(max = max(as.integer(round(v + residual_cases_lb * sqrt(max(c - n_days_btw_today_since_100, 0)),0)),0)) %>% arrange(max) 
                ,
              "Total Detected Deaths LB"= data.frame(c = c(1:length(total_detected_deaths), v = total_detected_deaths)) %>%   rowwise() %>%
                mutate(max = c( total_detected_deaths_LB, 
                                max(as.integer(round(v + residual_deaths_lb * sqrt(max(c - n_days_btw_today_since_100, 0)),0)),0))) 
                ,
              "Total Detected UB"= data.frame(c = c(1:length(total_detected), v = total_detected)) %>%   rowwise() %>%
                mutate(max = max(as.integer(round(v + residual_cases_ub * sqrt(max(c - n_days_btw_today_since_100, 0)),0)),0)) %>% arrange(max) 
                ,
              "Total Detected Deaths UB"= data.frame(c = c(1:length(total_detected_deaths), v = total_detected_deaths)) %>%   rowwise() %>%
                mutate(max = c( total_detected_deaths_LB, 
                                max(as.integer(round(v + residual_deaths_ub * sqrt(max(c - n_days_btw_today_since_100, 0)),0)),0))) 
            )
                            
          #df_agg_country_temp = pd.concat([df_agg_country_temp, df_predictions_since_100_cont_country_prov], axis = 1)
          
          df_agg_country_temp <- bind_rows(df_agg_country_temp, df_predictions_since_100_cont_country_prov)
        } 
        else
        {
            df_predictions_since_100_cont_country_prov = pd.DataFrame
            (
              "Total Detected LB" = rep(NaN, nrow(df_agg_country_temp) ),  #: [np.nan for _ in range(len(df_agg_country_temp))],  
              "Total Detected Deaths LB" = rep(NaN, nrow(df_agg_country_temp) ),  #:  [np.nan for _ in range(len(df_agg_country_temp))],
              "Total Detected UB"  = rep(NaN, nrow(df_agg_country_temp) ),  #:  [np.nan for _ in range(len(df_agg_country_temp))],
              "Total Detected Deaths UB"  = rep(NaN, nrow(df_agg_country_temp) )   #:  [np.nan for _ in range(len(df_agg_country_temp))]
            )
            #df_agg_country_temp = pd.concat([df_agg_country_temp, df_predictions_since_100_cont_country_prov], axis = 1)
            df_agg_country_temp <- bind_rows(df_agg_country_temp, df_predictions_since_100_cont_country_prov)
        }
      
        list_df_aggregated_countries.append(df_agg_country_temp)
      }
      
    df_agg_country_final <- data.frame()
    
    for (i in list_df_aggregated_countries) {
      df_agg_country_final <- bind_rows(df_agg_country_final, i)
    }
      
    df_agg_country_final 
  }
      
  get_aggregation_per_continent_with_cf <- function(
        df_predictions,
        past_prediction_file  = "I://covid19orc//danger_map//predicted//Global_V2_20200720.csv",
        past_prediction_date  = "2020-07-04",
        q = 0.5
      ) 
    {
        #Creates aggregations at the continent level as well as associated confidence intervals
        #param df_predictions: dataframe containing the raw predictions from the DELPHI model
        #param past_prediction_file: past prediction file's path for CI generation
        #param past_prediction_date: past prediction's date for CI generation
        #param q: quantile used for the CIs
        #return: dataframe with continent level aggregated predictions & associated confidence intervals
        
      #columns_without_bounds = [x for x in df_predictions.columns if ("LB" not in x) and ("UB" not in x)]
      
      columns_without_bounds <- c()
      for ( x in names(df_predictions)) {
        if (str_count(x, pattern = "LB|LB") == 0 )
          columns_without_bounds <- c(columns_without_bounds, x)
      }
      
      df_agg_continent <- df_predictions[columns_without_bounds] %>%
        group_by(Continent, Day) %>% summarize(sum(min_count = 1))  #.reset_index()
      df_agg_continent["Country"] <- "None"
      df_agg_continent["Province"] <- "None"
      df_agg_continent <- df_agg_continent[columns_without_bounds]
      
      #aggregated_continents = set(zip(df_agg_continent["Continent"], df_agg_continent["Country"],df_agg_continent["Province"]))
      
      aggregated_continents <- data.frame(df_agg_continent["Continent"],
                                          df_agg_continent["Country"],
                                          df_agg_continent["Province"])
      
      past_predictions <- read_csv(past_prediction_file)
      list_df_aggregated_continents = list()
      
      #for continent, country, province in aggregated_continents{
      #  past_predictions_temp = (past_predictions[(past_predictions['Day'] > past_prediction_date)   & (past_predictions['Continent'] == continent)   & 
      #                                              (past_predictions['Country'] == country) &  (past_predictions['Country'] == province)]).sort_values("Day")}
      
      
      for(i in 1:nrow(aggregated_continents)) 
      {
      
        continent <- aggregated_continents[i]$Continent
        country <- aggregated_continents[i]$Country
        province <- aggregated_continents[i]$Province
        past_predictions_temp <- past_predictions %>% 
              filter(Day > past_prediction_date & Continent == continent & 
                       Country == country & Country == province) %>%  arrange(Day)
            
            #df_agg_continent_temp = (df_agg_continent[(df_agg_continent['Continent'] == continent)]).sort_values("Day").reset_index(drop=True)
            
            df_agg_continent_temp <- df_agg_continent %>% filter('Continent' == continent) %>% arrange(Day)
            
            total_detected <- df_agg_continent_temp['Total Detected'] 
            total_detected_deaths <- df_agg_continent_temp['Total Detected Deaths'] 
           
            cases_fit_data <- df_agg_continent_temp['Total Detected True'] 
            deaths_fit_data <- df_agg_continent_temp['Total Detected Deaths True'] 
            since_100_dates <- df_agg_continent_temp['Day']   
            #n_days_btw_today_since_100 = (datetime.now() - pd.to_datetime(min(since_100_dates))).days
            n_days_btw_today_since_100 <- format(Sys.Date()  - as.datetime(since_100_dates), units = "days")
            
            if (nrows(past_predictions_temp) > 0)
            {
              #cases_fit_data_past = [y for x, y in zip(since_100_dates,cases_fit_data) if ((x > past_prediction_date) and (not np.isnan(y)))]
              
              cases_fit_data_past <- as.data.frame(x = since_100_dates, y = cases_fit_data) %>% 
                filter(x > past_prediction_date & !is.nan(y)) %>% select(y)
              
              #deaths_fit_data_past = [y for x, y in zip(since_100_dates,deaths_fit_data) if ((x > past_prediction_date) and (not np.isnan(y)))]
              
              
              deaths_fit_data_past <- as.data.frame(x = since_100_dates, y = deaths_fit_data) %>% 
                filter(x > past_prediction_date & !is.nan(y)) %>% select(y)
              
              #total_detected_past = past_predictions_temp["Total Detected"].values[:len(cases_fit_data_past)]
              
              total_detected_past <- past_predictions_temp["Total Detected"][1:length(cases_fit_data_past)]
              
              #total_detected_deaths_past = past_predictions_temp["Total Detected Deaths"].values[:len(deaths_fit_data_past)]
              
              total_detected_deaths_past <- past_predictions_temp["Total Detected Deaths"][1:length(deaths_fit_data_past)]
              
              
              #residual_cases_lb = np.sqrt(np.mean([(x- y) ** 2 for x,y in zip(cases_fit_data_past,total_detected_past)])) * scipy.stats.norm.ppf(0.5 - q /2)
              #residual_cases_ub = np.sqrt(np.mean([(x- y) ** 2 for x,y in zip(cases_fit_data_past,total_detected_past)])) * scipy.stats.norm.ppf(0.5 + q /2)
              #residual_deaths_lb = np.sqrt(np.mean([(x- y) ** 2 for x,y in zip(deaths_fit_data_past,total_detected_deaths_past)])) * scipy.stats.norm.ppf(0.5 - q /2)
              #residual_deaths_ub = np.sqrt(np.mean([(x- y) ** 2 for x,y in zip(deaths_fit_data_past,total_detected_deaths_past)])) *  scipy.stats.norm.ppf(0.5 + q /2)
              
              rt_mn_sq_residual_cases <- data.frame(x=cases_fit_data_past, y=total_detected_past) %>% 
                summarise(s = sqrt(mean( (x-y)^2) )) %>% as.numeric()
              
              residual_cases_lb <- rt_mn_sq_residual_cases * qnorm(0.5 - q / 2)
              residual_deaths_ub <- rt_mn_sq_residual_cases * qnorm(0.5 + q / 2)
              
              rt_mn_sq_death_cases <- data.frame(x=deaths_fit_data_past, y=total_detected_deaths_past) %>% 
                summarise(s = sqrt(mean( (x-y)^2) )) %>% as.numeric()

              residual_cases_lb <- rt_mn_sq_death_cases * qnorm(0.5 - q / 2)
              residual_deaths_ub <- rt_mn_sq_death_cases * qnorm(0.5 + q / 2)
              
              # Generation of the dataframe from the day since 100th cas
              
              df_predictions_since_100_cont_country_prov = pd.DataFrame(
                "Total Detected LB"= data.frame(c = c(1:length(total_detected), v = total_detected)) %>%   rowwise() %>%
                  mutate(max = max(as.integer(round(v + residual_cases_lb * sqrt(max(c - n_days_btw_today_since_100, 0)),0)),0)) %>% arrange(max) 
                ,
                "Total Detected Deaths LB"= data.frame(c = c(1:length(total_detected_deaths), v = total_detected_deaths)) %>%   rowwise() %>%
                  mutate(max = c( total_detected_deaths_LB, 
                                  max(as.integer(round(v + residual_deaths_lb * sqrt(max(c - n_days_btw_today_since_100, 0)),0)),0))) 
                ,
                "Total Detected UB"= data.frame(c = c(1:length(total_detected), v = total_detected)) %>%   rowwise() %>%
                  mutate(max = max(as.integer(round(v + residual_cases_ub * sqrt(max(c - n_days_btw_today_since_100, 0)),0)),0)) %>% arrange(max) 
                ,
                "Total Detected Deaths UB"= data.frame(c = c(1:length(total_detected_deaths), v = total_detected_deaths)) %>%   rowwise() %>%
                  mutate(max = c( total_detected_deaths_LB, 
                                  max(as.integer(round(v + residual_deaths_ub * sqrt(max(c - n_days_btw_today_since_100, 0)),0)),0))) 
              )
              
              df_agg_continent_temp <- bind_rows(df_agg_continent_temp, df_predictions_since_100_cont_country_prov)
              
              #df_agg_continent_temp = pd.concat([df_agg_continent_temp, df_predictions_since_100_cont_country_prov], axis = 1)
        }
        else 
        {
    
          df_predictions_since_100_cont_country_prov = pd.DataFrame
          (
            "Total Detected LB" = rep(NaN, nrow(df_agg_continent_temp) ),  #: [np.nan for _ in range(len(df_agg_country_temp))],  
            "Total Detected Deaths LB" = rep(NaN, nrow(df_agg_continent_temp) ),  #:  [np.nan for _ in range(len(df_agg_country_temp))],
            "Total Detected UB"  = rep(NaN, nrow(df_agg_continent_temp) ),  #:  [np.nan for _ in range(len(df_agg_country_temp))],
            "Total Detected Deaths UB"  = rep(NaN, nrow(df_agg_continent_temp) )   #:  [np.nan for _ in range(len(df_agg_country_temp))]
          )
          #df_agg_country_temp = pd.concat([df_agg_country_temp, df_predictions_since_100_cont_country_prov], axis = 1)
          df_agg_continent_temp <- bind_rows(df_agg_continent_temp, df_predictions_since_100_cont_country_prov)
        }  
      
        list_df_aggregated_continents.append(df_agg_continent_temp)
      }
      
      df_agg_continent_final <- data.frame()
      
      for (i in list_df_aggregated_continents.append) {
        df_agg_continent_final <- bind_rows(df_agg_continent_final, i)
      }
    
      df_agg_continent_final 
    }
      
  get_aggregation_world_with_cf <- function(
        df_predictions ,
        past_prediction_file  = "I://covid19orc//danger_map//predicted//Global_V2_20200720.csv",
        past_prediction_date  = "2020-07-04",
        q  = 0.5
      ) 
  {
        #Creates aggregations at the world level as well as associated confidence intervals
        #:param df_predictions: dataframe containing the raw predictions from the DELPHI model
        #:param past_prediction_file: past prediction file's path for CI generation
        #:param past_prediction_date: past prediction's date for CI generation
        #:param q: quantile used for the CIs
        #:return: dataframe with continent world aggregated predictions & associated confidence intervals
      
      #columns_without_bounds = [x for x in df_predictions.columns if ("LB" not in x) and ("UB" not in x)]
      
      columns_without_bounds <- c()
      for ( x in names(df_predictions)) {
        if (str_count(x, pattern = "LB|LB") == 0 )
          columns_without_bounds <- c(columns_without_bounds, x)
      }
      
      #df_agg_world = df_predictions[columns_without_bounds].groupby(["Day"]).sum(min_count = 1).reset_index()
      
      df_agg_world <- df_predictions[columns_without_bounds] %>% group_by(Day) %>% summarise(sum = sum())
      
      
      df_agg_world["Continent"] <- "None"
      df_agg_world["Country"] <- "None"
      df_agg_world["Province"] <- "None"
      df_agg_world <- df_agg_world[columns_without_bounds]
      
      past_predictions <- read_csv(past_prediction_file)
      
      #past_predictions_temp = (past_predictions[(past_predictions['Day'] > past_prediction_date) &  (past_predictions['Continent'] == "None") & (past_predictions['Country'] == "None")   & (past_predictions['Province'] == "None")]).sort_values("Day")
      
      past_predictions_temp <- past_predictions %>% filter(Day > past_prediction_date & Continent == "None" & Country == "None" & Province == "None") %>%
          arrange(Day)
      
      total_detected <- df_agg_world['Total Detected'] 
      total_detected_deaths <- df_agg_world['Total Detected Deaths'] 
    
      cases_fit_data <- df_agg_world['Total Detected True'] 
      deaths_fit_data <- df_agg_world['Total Detected Deaths True'] 
      since_100_dates <- df_agg_world['Day']   
      
      #n_days_btw_today_since_100 = (datetime.now() - pd.to_datetime(min(since_100_dates))).days
      
      n_days_btw_today_since_100 <- as.numeric(difftime(Sys.Date()  , n_days_btw_today_since_100, units = "days"))
      
      
      if (nrow(past_predictions_temp) > 0) 
      {
          cases_fit_data_past <- as.data.frame(x = since_100_dates, y = cases_fit_data) %>% 
                filter(x > past_prediction_date & !is.nan(y)) %>% select(y)
          deaths_fit_data_past <- as.data.frame(x = since_100_dates, y = deaths_fit_data) %>% 
            filter(x > past_prediction_date & !is.nan(y)) %>% select(y)
          total_detected_past <- past_predictions_temp["Total Detected"][1:length(cases_fit_data_past)]
          total_detected_deaths_past <- past_predictions_temp["Total Detected Deaths"][1:length(deaths_fit_data_past)]
          
          #residual_cases_lb = np.sqrt(np.mean([(x- y) ** 2 for x,y in zip(cases_fit_data_past,total_detected_past)])) * scipy.stats.norm.ppf(0.5 - q /2)
          #residual_cases_ub = np.sqrt(np.mean([(x- y) ** 2 for x,y in zip(cases_fit_data_past,total_detected_past)])) * scipy.stats.norm.ppf(0.5 + q /2)
          #residual_deaths_lb = np.sqrt(np.mean([(x- y) ** 2 for x,y in zip(deaths_fit_data_past,total_detected_deaths_past)])) * scipy.stats.norm.ppf(0.5 - q /2)
          #residual_deaths_ub = np.sqrt(np.mean([(x- y) ** 2 for x,y in zip(deaths_fit_data_past,total_detected_deaths_past)])) *  scipy.stats.norm.ppf(0.5 + q /2)
      
          
          rt_mn_sq_residual_cases <- data.frame(x=cases_fit_data_past, y=total_detected_past) %>% 
            summarise(s = sqrt(mean( (x-y)^2) )) %>% as.numeric()
          
          residual_cases_lb <- rt_mn_sq_residual_cases * qnorm(0.5 - q / 2)
          residual_deaths_ub <- rt_mn_sq_residual_cases * qnorm(0.5 + q / 2)
          
          rt_mn_sq_death_cases <- data.frame(x=deaths_fit_data_past, y=total_detected_deaths_past) %>% 
            summarise(s = sqrt(mean( (x-y)^2) )) %>% as.numeric()
          
          residual_cases_lb <- rt_mn_sq_death_cases * qnorm(0.5 - q / 2)
          residual_deaths_ub <- rt_mn_sq_death_cases * qnorm(0.5 + q / 2)
          
          
          
          # Generation of the dataframe from the day since 100th case
          df_predictions_since_100_cont_country_prov = pd.DataFrame(
            "Total Detected LB" = data.frame(c = c(1:length(total_detected), v = total_detected)) %>%   rowwise() %>%
              mutate(max = max(as.integer(round(v + residual_cases_lb * sqrt(max(c - n_days_btw_today_since_100, 0)),0)),0)) %>% arrange(max), 
            "Total Detected Deaths LB"= data.frame(c = c(1:length(total_detected_deaths), v = total_detected_deaths)) %>%   rowwise() %>%
              mutate(max = c( total_detected_deaths_LB, 
                              max(as.integer(round(v + residual_deaths_lb * sqrt(max(c - n_days_btw_today_since_100, 0)),0)),0))) ,
            "Total Detected UB"= data.frame(c = c(1:length(total_detected), v = total_detected)) %>%   rowwise() %>%
              mutate(max = max(as.integer(round(v + residual_cases_ub * sqrt(max(c - n_days_btw_today_since_100, 0)),0)),0)) %>% arrange(max) ,
            "Total Detected Deaths UB"= data.frame(c = c(1:length(total_detected_deaths), v = total_detected_deaths)) %>%   rowwise() %>%
              mutate(max = c( total_detected_deaths_LB, 
                              max(as.integer(round(v + residual_deaths_ub * sqrt(max(c - n_days_btw_today_since_100, 0)),0)),0))) 
          )
          
          #df_agg_world_final = pd.concat([df_agg_world, df_predictions_since_100_cont_country_prov], axis = 1)
          #df_agg_world_final <- bind_rows(df_agg_world, df_predictions_since_100_cont_country_prov)
      }
      else
      {
        
        df_predictions_since_100_cont_country_prov = pd.DataFrame
        (
          "Total Detected LB" = rep(NaN, nrow(df_agg_world) ),  #: [np.nan for _ in range(len(df_agg_country_temp))],  
          "Total Detected Deaths LB" = rep(NaN, nrow(df_agg_world) ),  #:  [np.nan for _ in range(len(df_agg_country_temp))],
          "Total Detected UB"  = rep(NaN, nrow(df_agg_world) ),  #:  [np.nan for _ in range(len(df_agg_country_temp))],
          "Total Detected Deaths UB"  = rep(NaN, nrow(df_agg_world) )   #:  [np.nan for _ in range(len(df_agg_country_temp))]
        )
      }
      df_agg_world_final <- bind_rows(df_agg_world, df_predictions_since_100_cont_country_prov)
      
      df_agg_world_final     
  }
  
  append_all_aggregations_cf <- function(
    df_predictions, past_prediction_file = "I://covid19orc//danger_map//predicted//Global_V2_20200720.csv",
    past_prediction_date = "2020-07-04",
    q = 0.5   ) 
    {
     #Creates and appends all the predictions' aggregations & Confidnece Intervals at the country, continent and  world levels
      #        :param df_predictions: dataframe with the raw predictions from DELPHI
      #        :param past_prediction_file: past prediction file's path for CI generation
      #        :param past_prediction_date: past prediction's date for CI generation
      #        :param q: quantile used for the CIs
      #        :return: dataframe with predictions raw from DELPHI and aggregated ones, as well as associated confidence
      #        intervals at the country, continent & world levels
       
        df_agg_since_today_per_country <- DELPHIAggregations.get_aggregation_per_country_with_cf(
          df_predictions=df_predictions,
          past_prediction_file=past_prediction_file,
          past_prediction_date=past_prediction_date,
          q=q
        )
        df_agg_since_today_per_continent = DELPHIAggregations.get_aggregation_per_continent_with_cf(
          df_predictions=df_predictions,
          past_prediction_file=past_prediction_file,
          past_prediction_date=past_prediction_date,
          q=q
        )
        df_agg_since_today_world = DELPHIAggregations.get_aggregation_world_with_cf(
          df_predictions=df_predictions,
          past_prediction_file=past_prediction_file,
          past_prediction_date=past_prediction_date,
          q=q
        )
        
        #df_predictions = pd.concat([
        #  df_predictions, df_agg_since_today_per_country,
        #  df_agg_since_today_per_continent, df_agg_since_today_world
        #  ],sort=False)
        
        df_predictions <- bind_rows(df_predictions, df_agg_since_today_per_country, df_agg_since_today_per_continent, df_agg_since_today_world)
        
        df_predictions <- df_predictions %>% arrange(Continent, Country, Province, Day)
        
        df_predictions_from_today <- df_predictions %>% filter(Day >= Sys.Date())
        list(df_predictions_from_today, df_predictions)
  }
  
get_aggregation_per_country <- function(df_policy_predictions)
{
  #Aggregates policy predictions at the country level from the predictions dataframe
  #param df_policy_predictions: DELPHI policy predictions dataframe
  #return: DELPHI policy predictions dataframe aggregated at the country level
   
  #df_policy_predictions = df_policy_predictions[df_policy_predictions["Province"] != "None"]
  
    df_policy_predictions <- df_policy_predictions %>% filter(Province != "None")
    
    df_agg_country <- df_policy_predictions %>% groupby(Policy, Time, Continent, Country, Day) %>% aggregate(sum())
  
    df_agg_country["Province"] <- "None"
    df_agg_country <- df_agg_country %>% select("Policy", "Time", "Continent", "Country", "Province", 
        "Day", "Total Detected", "Active", "Active Hospitalized", "Cumulative Hospitalized", 
        "Total Detected Deaths", "Active Ventilated")
    
    df_agg_country
}

get_aggregation_per_continent <- function(df_policy_predictions)
{
        # Aggregates policy predictions at the continent level from the predictions dataframe
        #:param df_policy_predictions: DELPHI policy predictions dataframe
        #:return: DELPHI policy predictions dataframe aggregated at the continent level
        
    df_agg_continent <- df_policy_predictions.groupby %>% group_by(Policy, Time, Continent, Day) %>% aggregate(sum())
    
    df_agg_continent["Country"] <- "None"
    df_agg_continent["Province"] <- "None"
    df_agg_continent <- df_agg_continent %>% select ("Policy", "Time", "Continent", "Country", "Province", "Day", "Total Detected", "Active",
        "Active Hospitalized", "Cumulative Hospitalized", "Total Detected Deaths", "Active Ventilated")
    df_agg_continent
}

get_aggregation_world(df_policy_predictions)
{
 #Aggregates policy predictions at the world level from the predictions dataframe
 #:param df_policy_predictions: DELPHI policy predictions dataframe
#return: DELPHI policy predictions dataframe aggregated at the world level
 
    df_agg_world <- df_policy_predictions %>% group_by(Policy, Time, Day) %>% aggregate(sum())
    df_agg_world["Continent"] <- "None"
    df_agg_world["Country"] <- "None"
    df_agg_world["Province"] <- "None"
    df_agg_world <- df_agg_world %>% select( "Policy", "Time", "Continent", "Country", "Province", "Day", "Total Detected", "Active",
        "Active Hospitalized", "Cumulative Hospitalized", "Total Detected Deaths", "Active Ventilated")
    df_agg_world
}

append_all_aggregations <- function(df_policy_predictions)
{
        #Creates and appends all the policy predictions' aggregations at the country, continent and world levels
        #:param df_predictions: dataframe with the raw policy predictions from DELPHI
        #:return: dataframe with raw policy predictions from DELPHI and aggregated ones at the country,
        #continent & world levels
        
  df_agg_since_today_per_country <- get_aggregation_per_country(df_policy_predictions)
  df_agg_since_today_per_continent <- get_aggregation_per_continent(df_policy_predictions)
  df_agg_since_today_world <- get_aggregation_world(df_policy_predictions)
  df_policy_predictions <- bind_rows(
      df_policy_predictions,
      df_agg_since_today_per_country,
      df_agg_since_today_per_continent,
      df_agg_since_today_world
  )
  df_policy_predictions <- df_policy_predictions %>% arrange( Policy, Time, Continent, Country, Province, Day)
  df_policy_predictions
}



  
  
  

