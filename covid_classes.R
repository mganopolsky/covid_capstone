#add install requirements for packages below

library(futile.logger)
library(zoo)
library(sets)

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


setMethod("initialize", "DELPHIDataCreator",
          function(.Object,
                   x_sol_final, 
                   date_day_since100 ,
                   best_params ,
                   continent ,
                   country ,
                   province ,
                   testing_data_included,
                   ...) 
    {  
      if (!missing(x_sol_final)) { .Object@x_sol_final <- x_sol_final }
      if (!missing(date_day_since100)) { .Object@date_day_since100 <- date_day_since100 }
      if (!missing(best_params)) { .Object@best_params <- best_params }
      if (!missing(continent)) { .Object@continent <- continent }
      if (!missing(country)) { .Object@country <- country }
      if (!missing(province)) { .Object@province <- province }
      if (!missing(testing_data_included)) { .Object@testing_data_included <- testing_data_included }
    }
)

setMethod("create_dataset_parameters", "DELPHIDataCreator",
          function(.Object, mape, ...) 
  {
    #Creates the parameters dataset with the results from the optimization and the pre-computed MAPE
    #:param mape: MAPE a float on the last 15 days (or less if less historical days available) for that particular area
    #:return: dataframe with parameters and MAPE
    
    if (object@testing_data_included)
      print( "Parameters dataset created without the testing data parameters beta_0, beta_1: code will have to be modified")
    
    df_parameters = data.frame(
      Continent:.Object@continent,
      Country:.Object@country,
      Province:.Object@province,
      "Data Start Date":.Object@date_day_since100,
      MAPE: mape,
      "Infection Rate": .Object@best_params[1],
      "Median Day of Action": .Object@best_params[2],
      "Rate of Action": .Object@best_params[3],
      "Rate of Death": .Object@best_params[4],
      "Mortality Rate": .Object@best_params[5],
      "Rate of Mortality Rate Decay": .Object@best_params[6],
      "Internal Parameter 1": .Object@best_params[7],
      "Internal Parameter 2": .Object@best_params[8],
      "Jump Magnitude": .Object@best_params[9],
      "Jump Time": .Object@best_params[10],
      "Jump Decay": .Object@best_params[11]
    )
    df_parameters  
  }
)


setMethod("create_datasets_predictions", "DELPHIDataCreator",function(.Object, ...)
    {
       #-> (pd.DataFrame, pd.DataFrame):
      #  Creates two dataframes with the predictions of the DELPHI model, the first one since the day of the prediction,
      #  the second since the day the area had 100 cases
      #  :return: tuple of dataframes with predictions from DELPHI model
        
        n_days_btw_today_since_100 <- as.numeric(difftime(Sys.Date() , .Object@date_day_since100, units = "days"))
        
        n_days_since_today <- as.numeric(difftime(object@x_sol_final[[2]]  , n_days_btw_today_since_100, units = "days"))
        
        all_dates_since_today <- c()
        for (i in 1:n_days_since_today){
          temp_date <- Sys.Date() + i
          all_dates_since_today <- c(all_dates_since_today, toString(temp_date))
        }
    
        # Predictions
        total_detected <- .Object@x_sol_final[[16]]  # DT
        #total_detected =   [int(round(x, 0)) for x in total_detected]
        total_detected <- as.integer(total_detected)
        #active_cases = (
        #  object@x_sol_final[5, :]
        #  + object@x_sol_final[6, :]
        #  + object@x_sol_final[8, :]
        #  + object@x_sol_final[9, :]
        #)  # DHR + DQR + DHD + DQD
        
        # DHR + DQR + DHD + DQD
        active_cases <- .Object@x_sol_final[[5]] + .Object@x_sol_final[[6]]  + 
          .Object@x_sol_final[[8]] + .Object@x_sol_final[[9]]
        
        active_cases <- as.integer(active_cases)
        
        #active_cases = [int(round(x, 0)) for x in active_cases]
        #active_hospitalized = (
        #  object@x_sol_final[5, :] + object@x_sol_final[8, :]
        #)  # DHR + DHD
        
        active_hospitalized <- .Object@x_sol_final[[5]] + .Object@x_sol_final[[8]]  # DHR + DHD
        active_hospitalized <- as.integer(active_hospitalized)
        
        #active_hospitalized = [int(round(x, 0)) for x in active_hospitalized]
        #cumulative_hospitalized = object@x_sol_final[12, :]  # TH
        #cumulative_hospitalized = [int(round(x, 0)) for x in cumulative_hospitalized]
        
        cumulative_hospitalized <- .Object@x_sol_final[[12]] #TH
        cumulative_hospitalized <- as.integer(cumulative_hospitalized)
        
        
        #total_detected_deaths = object@x_sol_final[15, :]  # DD
        #total_detected_deaths = [int(round(x, 0)) for x in total_detected_deaths]
        
        total_detected_deaths <- .Object@x_sol_final[[15]] #TH
        total_detected_deaths <- as.integer(total_detected_deaths)
        
        
        
        #active_ventilated = (
        #  object@x_sol_final[13, :] + object@x_sol_final[14, :]
        #)  # DVR + DVD
        #active_ventilated = [int(round(x, 0)) for x in active_ventilated]
        
        
        active_ventilated <- .Object@x_sol_final[[13]] + .Object@x_sol_final[[14]] # DVR + DVD
        active_ventilated <- as.integer(active_ventilated)
        
        
        # Generation of the dataframe since today
        df_predictions_since_today_cont_country_prov <- data.frame
        (
          
            "Continent" = rep( .Object@continent, n_days_since_today), # for _ in range(n_days_since_today)],
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
        start_date <- as.date(object@date_day_since100)
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
    })

setMethod("create_datasets_raw", "DELPHIDataCreator",
          function(.Object, ...) 
{
    #Creates a dataset in the right format (with values for all 16 states of the DELPHI model)
    #for the Optimal Vaccine Allocation team
    
    n_days_btw_today_since_100 <- 
     as.numeric(difftime(Sys.Date()  , .Object@date_day_since100, units = "days"))
  
    n_days_since_today = .Object@x_sol_final[[1]] - n_days_btw_today_since_100
     
     all_dates_since_today <- c()
     for (i in 1:n_days_since_today){
       temp_date <- Sys.Date() + i
       all_dates_since_today <- c(all_dates_since_today, toString(temp_date))
     }
     
    
    df_predictions_since_today_cont_country_prov = pd.DataFrame(
    
        "Continent" = .Object@continent, #[object@continent for _ in range(n_days_since_today)],
        "Country"= .Object@country, #[object@country for _ in range(n_days_since_today)],
        "Province"= .Object@province, # [object@province for _ in range(n_days_since_today)],
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
    start_date <- as.date(object@date_day_since100)
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
})


setMethod("create_datasets_with_confidence_intervals", "DELPHIDataCreator",
          function(.Object, 
              cases_data_fit, #: list,
              deaths_data_fit, #: list,
              past_prediction_file = "I://covid19orc//danger_map//predicted//Global_V2_20200720.csv",
              past_prediction_date = "2020-07-04",
              q = 0.5,
              ...)
    
  {     # Generates the prediction datasets from the date with 100 cases and from the day of running, including columns
          #  containing Confidence Intervals used in the website for cases and deaths
          #  :param cases_data_fit: list, contains data used to fit on number of cases
          #  :param deaths_data_fit: list, contains data used to fit on number of deaths
          #  :param past_prediction_file: past prediction file's path for CI generation
          #  :param past_prediction_date: past prediction's date for CI generation
          #  :param q: quantile used for the CIs
          #  :return: tuple of dataframes (since day of optimization & since 100 cases in the area) with predictions and
          #  confidence intervals
           
      n_days_btw_today_since_100 <- as.numeric(difftime(Sys.Date() , .Object@date_day_since100, units = "days"))
    
      
      n_days_since_today = as.numeric(.Object@x_sol_final[[1]]) - n_days_btw_today_since_100
      
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
      
      total_detected <- .Object@x_sol_final[[16]]  # DT
      
      #total_detected = [int(round(x, 0)) for x in total_detected]
      total_detected <- as.integer(total_detected)
  
      
      active_cases <- .Object@x_sol_final[[5]] + .Object@x_sol_final[[6]]  + 
        .Object@x_sol_final[[8]] + .Object@x_sol_final[[9]]  # DHR + DQR + DHD + DQD
      
      active_cases <- as.integer(active_cases)
      
      active_hospitalized <- .Object@x_sol_final[[5]] + .Object@x_sol_final[[8]]  # DHR + DHD
      active_hospitalized <- as.integer(active_hospitalized)
      
      
      cumulative_hospitalized <- .Object@x_sol_final[[12]] #TH
      cumulative_hospitalized <- as.integer(cumulative_hospitalized)
      
      total_detected_deaths <- .Object@x_sol_final[[15]] #TH
      total_detected_deaths <- as.integer(total_detected_deaths)
      
      
      active_ventilated <- .Object@x_sol_final[[13]] + .Object@x_sol_final[[14]] # DVR + DVD
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
        filter(Day > past_prediction_date & Country == .Object@country & Province == .Object@province) %>%
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
          start_date <- as.date(object@date_day_since100)
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
          start_date <- as.date(.Object@date_day_since100)
          end_date <-  as.date(.Object@x_sol_final[[1]])
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
  })


setMethod("create_datasets_predictions_scenario", "DELPHIDataCreator",
          function(.Object, policy = "Lockdown", time =  0, totalcases=NaN, ...   )
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
      total_detected <- unlist(.Object@x_sol_final[[16]]) #, :]  # DT
      total_detected <- as.integer(unlist(total_detected)) #= [int(round(x, 0)) for x in total_detected]
      active_cases <- c(
        unlist(object@x_sol_final[[5]]),
        unlist(object@x_sol_final[[6]]),
        unlist(object@x_sol_final[[8]]),
        unlist(.Object@x_sol_final[[9]])
      )  # DHR + DQR + DHD + DQD
      active_cases <- as.integer(active_cases) #= [int(round(x, 0)) for x in active_cases]
      active_hospitalized <- c(unlist(object@x_sol_final[[5]]), .Object@x_sol_final[[8]] )  # DHR + DHD
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
      start_date <- as.date(.Object@date_day_since100)
      end_date <-  as.date(.Object@x_sol_final[[1]])
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
    })

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

DELPHIBacktest <- setClass( 
  #setting the class name
  "DELPHIBacktest", 
  
  #defining the slots
  slots = c(
    path_to_folder_danger_map = "character", 
    historical_data_path="character", 
    prediction_data_path = "character",
    prediction_date = "Date",
    n_days_backtest = "numeric",
    get_mae = "logical",
    get_mse = "get_mse",
    logger = "flog.logger"),
  
  prototype = list(
    testing_data_included = TRUE, 
    date_day_since100 = Sys.Date() - 100)
  
)

validDELPHIBacktestObject <- function(object) 
{
  historical_data_path
  TRUE
}
## assign the function as the validity method for the class
setValidity("DELPHIBacktest", validDELPHIBacktestObject)

setMethod("initialize", "DELPHIBacktest",
      function(.Object,
               path_to_folder_danger_map, 
               prediction_date ,
               n_days_backtest ,
               get_mae ,
               get_mse ,
               logger ,
               ...) 
      {  
        if (!missing(path_to_folder_danger_map)) { 
          .Object@historical_data_path = path_to_folder_danger_map + "processed/Global/" 
        }
        if (!missing(prediction_data_path)) { 
          .Object@prediction_data_path = path_to_folder_danger_map + "predicted/"
        }
        if (!missing(prediction_date)) { .Object@prediction_date <- prediction_date }
        if (!missing(n_days_backtest)) { .Object@n_days_backtest <- n_days_backtest }
        if (!missing(get_mae)) { .Object@get_mae <- get_mae }
        if (!missing(get_mse)) { .Object@get_mse <- get_mse }
        if (!missing(logger)) { .Object@logger <- logger }
      }
)


# get_historical_data_df(self) -> pd.DataFrame:
 setMethod("get_historical_data_df", "DELPHIBacktest", function(.Object,    ...) 
 {  
  #Generates a concatenation of all historical data available in the danger_map folder, all areas
  #      starting from the prediction date given by the user, and keeping only relevant columns
  #      :return: a dataframe with all relevant historical data
   
   list_historical_data_filepaths = list()
    #list_historical_data_filepaths = [
    #  self.historical_data_path + filename
    #  for filename in os.listdir(self.historical_data_path)
    #  if "Cases_" in filename
    #  ]
   
   
   list_historical_data_filepaths <- list.files(path = .Object@historical_data_path, pattern = "Cases_", all.files = TRUE,
              full.names = TRUE, recursive = FALSE,
              ignore.case = FALSE, include.dirs = FALSE)
   
    df_historical = data.frame()
    for (filepath_historical in list_historical_data_filepaths)
    {
      #df_historical.append(pd.read_csv(filepath_historical))
      df_historical <- bind_rows(df_historical, read_csv(filepath_historical))
    }
    
    #df_historical = pd.concat(df_historical).sort_values(
    #  ["country", "province", "date"]
    #).reset_index(drop=True)[
    #  ["country", "province", "date", "day_since100", "case_cnt", "death_cnt"]
    #  ]
    
    df_historical <- df_historical %>% arrange(country, province, date) %>% 
      select("country", "province", "date", "day_since100", "case_cnt", "death_cnt")
    
    #df_historical["province"].fillna("None", inplace=True)
    
    df_historical$province <- df_historical %>% replace_na(list(province = "None")) %>%
      select(province)
    
    #df_historical.rename(
    #  columns={"country": "Country", "province": "Province", "date": "Day"}, inplace=True
    #)
    df_historical <- df_historical %>% mutate(Country = country, Province = province, Day = date)
    
    #must double check 
    #df_historical["tuple"] #= list(zip(df_historical.Country, df_historical.Province))
     
    df_historical <- df_historical %>% mutate(tuple = list(Country, Province))
    
    #df_historical = df_historical[
    #  (df_historical.Day >= self.prediction_date)
    #  ].reset_index(drop=True)
    
    df_historical <- df_historical %>% filter(Day >= .Object$prediction_date)
    
    #df_historical = df_historical[df_historical.tuple != ("US", "None")].reset_index(drop=True)
    df_historical <- df_historical %>% filter(!(tuple %in%  list("US", "None")))
    df_historical
 })

 
#get_prediction_data   (self) -> pd.DataFrame:
  
setMethod("get_prediction_data", "DELPHIBacktest", function(.Object,    ...) 
{    
  
    #     Retrieve the predicted data on the prediction_date given as an input by the user running
    #    :param prediction_date: prediction date to be used to look for the file in the danger_map folder, format
    #    has to be YYYY-MM-DD (it is asserted outside of this function)
    #    :return: a dataframe that contains the relevant predictions on the relevant prediction date
     
   prediction_date_filename <- paste(str_split(.Object@prediction_date, "-"), sep = "")
   
   if (file.exists(paste(.Object@prediction_data_path , "Global_V2_", prediction_date_filename, ".csv", sep = ""))){
     .Object@logger.info("Backtesting on DELPHI V3.0 predictions because filename contains _V2")
   
      df_prediction <- read_csv(paste(.Object@prediction_data_path , "Global_V2_", prediction_date_filename , ".csv", sep=""))
   #elif os.path.exists(self.prediction_data_path + f"Global_V2_{prediction_date_filename}.csv"):
    } else if (!file.exists(paste(.Object@prediction_data_path , "Global_V2_", prediction_date_filename, ".csv", sep = ""))) {  
      #self.logger.info("Backtesting on DELPHI V1.0 or V2.0 predictions because filename doesn't contain _V2")
      .Object@logger.info("Backtesting on DELPHI V1.0 or V2.0 predictions because filename doesn't contain _V2")
      df_prediction <- read_csv(paste(.Object@prediction_data_path , "Global_V1_", prediction_date_filename, ".csv", sept=""))
    } else {
     stop(paste("The file on prediction date ", .Object@prediction_date, " has never been generated", sep = ""))
   }
   
   df_prediction[["Continent", "Country", "Province", "Day", "Total Detected", "Total Detected Deaths"]]
})
 

#def generate_empty_metrics_dict(self) -> dict:
setMethod("generate_empty_metrics_dict", "DELPHIBacktest", function(.Object,    ...) 
{ 
  
        #Generates the format of the dictionary that will compose the dataframe with all backtest metrics
        #based on the get_mae and get_mse flags given by the user
        #:return: a dictionary with either 5, 7 or 9 keys depending on the MAE and MSE flags
      
  dict_df_backtest_metrics = list(
    "prediction_date" <- data.frame(),
    "n_days_backtest" <- data.frame(),
    "tuple_area"  <- data.frame(),
    "mape_cases"  <- data.frame(),
    "mape_deaths" <- data.frame()
  )
  if (.Object@get_mae) {
    dict_df_backtest_metrics["mae_cases"] <- data.frame()
    dict_df_backtest_metrics["mae_deaths"]  <- data.frame()
  }
  
  if ( .Object@get_mse ) {
    dict_df_backtest_metrics["mse_cases"]  <- data.frame()
    dict_df_backtest_metrics["mse_deaths"]  <- data.frame()
  }
  
  dict_df_backtest_metrics
})

#def get_backtest_metrics_area(  self, df_backtest: pd.DataFrame, tuple_area: tuple, dict_df_backtest_metrics: dict,)
setMethod("get_backtest_metrics_area", "DELPHIBacktest", function(.Object, df_backtest,  tuple_area , dict_df_backtest_metrics, ...) 
{
            #Updates the backtest metrics dictionary with metrics values for that particular area tuple
            #:param df_backtest: pre-processed dataframe containing historical and prediction data
            #:param tuple_area: tuple of the format (Continent, Country, Province)
            #:param dict_df_backtest_metrics: dictionary containing all the metrics and will be used to create final
            #backtest metrics dataframe
            #:return: updated dictionary with backtest metrics for that particular area tuple
          
    df_temp <- df_backtest %>% filter(tuple_complete %in% tuple_area)
    #max_date_backtest = str((pd.to_datetime(self.prediction_date) + timedelta(days=self.n_days_backtest)).date())
    
    max_date_backtest <- as.date(.Object@prediction_date) +  .Object@n_days_backtest
      
    #df_temp = df_temp[(df_temp.Day >= .Object@prediction_date) & (df_temp.Day <= max_date_backtest)].sort_values(
    #  ["Continent", "Country", "Province", "Day"]
    #).reset_index(drop=True)

    df_temp <- df_temp %>% filter(Day >= .Object@prediction_date & Day <= max_date_backtest) %>% 
      arrange(Continent, Country, Province, Day)
    
    
    #mae_cases, mape_cases = compute_mae_and_mape(  y_true=df_temp.case_cnt.tolist(), y_pred=df_temp["Total Detected"].tolist() )
    
    output_list <- compute_mae_and_mape(y_true=as.list(df_temp$case_cnt), y_pred=as.list(df_temp["Total Detected"]))
    
    mae_cases <- output_list[[1]]
    mape_cases <- output_list[[2]]
    
    #mae_deaths, mape_deaths = compute_mae_and_mape(y_true=df_temp.death_cnt.tolist(), y_pred=df_temp["Total Detected Deaths"].tolist())
    output_list <-  compute_mae_and_mape(y_true=as.list(df_temp$death_cnt), y_pred=as.list(df_temp["Total Detected Deaths"]))
    
    mae_deaths <- output_list[[1]]
    mape_deaths <- output_list[[2]]
    
    #mse_cases = compute_mse(y_true=df_temp.case_cnt.tolist(), y_pred=df_temp["Total Detected"].tolist())
    
    mse_cases <- compute_mse(y_true=as.list(df_temp$case_cnt), y_pred=as.list(df_temp["Total Detected"]))
    
    #mse_deaths = compute_mse(y_true=df_temp.death_cnt.tolist(), y_pred=df_temp["Total Detected Deaths"].tolist())
    mse_deaths <- compute_mse(y_true=as.list(df_temp$death_cnt), y_pred=as.list(df_temp["Total Detected Deaths"]))
    
    
    #dict_df_backtest_metrics["prediction_date"].append(.Object@prediction_date)
    dict_df_backtest_metrics["prediction_date"] <- append(dict_df_backtest_metrics["prediction_date"], .Object@prediction_date)
    #dict_df_backtest_metrics["n_days_backtest"].append(.Object@n_days_backtest)
    dict_df_backtest_metrics["n_days_backtest"] <- append(dict_df_backtest_metrics["n_days_backtest"] , .Object@n_days_backtest)
    #dict_df_backtest_metrics["tuple_area"].append(tuple_area)
    dict_df_backtest_metrics["tuple_area"] <- append(dict_df_backtest_metrics["tuple_area"], tuple_area)
    #dict_df_backtest_metrics["mape_cases"].append(mape_cases)
    dict_df_backtest_metrics["mape_cases"] <- append(dict_df_backtest_metrics["mape_cases"], mape_cases)
    #dict_df_backtest_metrics["mape_deaths"].append(mape_deaths)
    dict_df_backtest_metrics["mape_deaths"] <- append(dict_df_backtest_metrics["mape_deaths"], mape_deaths)

    if (.Object@get_mae) {
      #dict_df_backtest_metrics["mae_cases"].append(mae_cases)
      dict_df_backtest_metrics["mae_cases"] <- append(dict_df_backtest_metrics["mae_cases"], mae_cases)
      #dict_df_backtest_metrics["mae_deaths"].append(mae_deaths)
      dict_df_backtest_metrics["mae_deaths"] <- append(dict_df_backtest_metrics["mae_deaths"], mae_deaths)
    }
    
    if (Object@get_mse) {
      #dict_df_backtest_metrics["mse_cases"].append(mse_cases)
      dict_df_backtest_metrics["mse_cases"] <- append(dict_df_backtest_metrics["mse_cases"], mse_cases)
      #dict_df_backtest_metrics["mse_deaths"].append(mse_deaths)
      dict_df_backtest_metrics["mse_deaths"] <- append(dict_df_backtest_metrics["mse_deaths"], mse_deaths)
    }
    
    dict_df_backtest_metrics
} )

get_initial_conditions <- function(params_fitted, global_params_fixed) 
{
      #Generates the initial conditions for the DELPHI model based on global fixed parameters (mostly populations and some
      #constant rates) and fitted parameters (the internal parameters k1 and k2)
      #:param params_fitted: tuple of parameters being fitted, mostly interested in k1 and k2 here (parameters 7 and 8)
      #:param global_params_fixed: tuple of fixed and constant parameters for the model defined a while ago
      #:return: a list of initial conditions for all 16 states of the DELPHI model
    
  #alpha, days, r_s, r_dth, p_dth, r_dthdecay, k1, k2 = params_fitted[:8]
  
  names <- c("alpha", "days", "r_s", "r_dth", "p_dth", "r_dthdecay", "k1", "k2")
  for (i in length(params_fitted[[1]])) {
      assign(names[i], unlist(params_fitted[[1]][i]) )
  }
  
  #N, PopulationCI, PopulationR, PopulationD, PopulationI, p_d, p_h, p_v = global_params_fixed
  
  names <- c("N", "PopulationCI", "PopulationR", "PopulationD", "PopulationI", "p_d", "p_h", "p_v")
  for (i in length(global_params_fixed[[1]])) {
    assign(c[i], unlist(global_params_fixed[[1]][i])) 
  }
  
  S_0 <-  (N - PopulationCI / p_d)  - (PopulationCI / p_d * (k1 + k2))  - (PopulationR / p_d) - (PopulationD / p_d)
  
  E_0 <- PopulationCI / p_d * k1
  I_0 <- PopulationCI / p_d * k2
  UR_0 <- (PopulationCI / p_d - PopulationCI) * (1 - p_dth)
  DHR_0 <- (PopulationCI * p_h) * (1 - p_dth)
  DQR_0 <- PopulationCI * (1 - p_h) * (1 - p_dth)
  UD_0 <- (PopulationCI / p_d - PopulationCI) * p_dth
  DHD_0 <- PopulationCI * p_h * p_dth
  DQD_0 <- PopulationCI * (1 - p_h) * p_dth
  R_0 <- PopulationR / p_d
  D_0 <- PopulationD / p_d
  TH_0 <- PopulationCI * p_h
  DVR_0 <- (PopulationCI * p_h * p_v) * (1 - p_dth)
  DVD_0 <- (PopulationCI * p_h * p_v) * p_dth
  DD_0 <- PopulationD
  DT_0 <- PopulationI
  x_0_cases <- list( S_0, E_0, I_0, UR_0, DHR_0, DQR_0, UD_0, DHD_0, DQD_0, R_0,    D_0, TH_0, DVR_0, DVD_0, DD_0, DT_0)
  x_0_cases
}

get_initial_conditions_with_testing <- function(params_fitted, global_params_fixed)
{ 
  
    #Generates the initial conditions for the DELPHI model based on global fixed parameters (mostly populations and some
    #constant rates) and fitted parameters (the internal parameters k1 and k2) when including testing in the modeling
    #:param params_fitted: tuple of parameters being fitted, mostly interested in k1 and k2 here (parameters 7 and 8)
    #:param global_params_fixed: tuple of fixed and constant parameters for the model defined a while ago
    #:return: a list of initial conditions for all 16 states of the DELPHI model
    
    #alpha, days, r_s, r_dth, p_dth, k1, k2, beta_0, beta_1 = params_fitted
    
    names <- c("alpha", "days", "r_s", "r_dth", "p_dth",  "k1", "k2", "beta_0", "beta_1" )
    for (i in length(params_fitted[[1]])) {
      assign(names[i], unlist(params_fitted[[1]][i]) )
    }
    
    
    #N, PopulationCI, PopulationR, PopulationD, PopulationI, p_d, p_h, p_v = ( global_params_fixed )
    
    names <- c("N", "PopulationCI", "PopulationR", "PopulationD", "PopulationI", "p_d", "p_h", "p_v" )
    for (i in length(global_params_fixed[[1]])) {
      assign(names[i], unlist(global_params_fixed[[1]][i]) )
    }
    
    S_0 <- (
      (N - PopulationCI / p_d)
      - (PopulationCI / p_d * (k1 + k2))
      - (PopulationR / p_d)
      - (PopulationD / p_d)
    )
    E_0 <- PopulationCI / p_d * k1
    I_0 <- PopulationCI / p_d * k2
    UR_0  <-  (PopulationCI / p_d - PopulationCI) * (1 - p_dth)
    DHR_0  <-  (PopulationCI * p_h) * (1 - p_dth)
    DQR_0  <-  PopulationCI * (1 - p_h) * (1 - p_dth)
    UD_0  <-  (PopulationCI / p_d - PopulationCI) * p_dth
    DHD_0  <-  PopulationCI * p_h * p_dth
    DQD_0  <-  PopulationCI * (1 - p_h) * p_dth
    R_0  <-  PopulationR / p_d
    D_0  <-  PopulationD / p_d
    TH_0  <-  PopulationCI * p_h
    DVR_0  <-  (PopulationCI * p_h * p_v) * (1 - p_dth)
    DVD_0  <-  (PopulationCI * p_h * p_v) * p_dth
    DD_0  <-  PopulationD
    DT_0  <-  PopulationI
    x_0_cases = list (      S_0, E_0, I_0, UR_0, DHR_0, DQR_0, UD_0, DHD_0,
      DQD_0, R_0, D_0, TH_0, DVR_0, DVD_0, DD_0, DT_0)
    
    x_0_cases
}

create_fitting_data_from_validcases <- function(validcases) #: pd.DataFrame) -> (float, list, list):
{
    #Creates the balancing coefficient (regularization coefficient between cases & deaths in cost function) as well as
    #the cases and deaths data on which to be fitted
    #:param validcases: Dataframe containing cases and deaths data on the relevant time period for our optimization
    #:return: the balancing coefficient and two lists containing cases and deaths over the right time span for fitting
    
    validcases_nondeath <- list(validcases["case_cnt"]) #validcases["case_cnt"].tolist()
    validcases_death <- list(validcases["death_cnt"])  #validcases["death_cnt"].tolist()
    balance <-  validcases_nondeath[-1] / max(validcases_death[-1], 10) / 3
    cases_data_fit <- validcases_nondeath
    deaths_data_fit <- validcases_death
    #return balance, cases_data_fit, deaths_data_fit
    list(balance, cases_data_fit, deaths_data_fit)
}


get_residuals_value <- function (
  optimizer, balance, x_sol, cases_data_fit, deaths_data_fit, weights) 
{
    #Obtain the value of the loss function depending on the optimizer (as it is different for global optimization using
    #simulated annealing)
    #:param optimizer: String, for now either tnc, trust-constr or annealing
    #:param balance: Regularization coefficient between cases and deaths
    #:param x_sol: Solution previously fitted by the optimizer containing fitted values for all 16 states
    #:param fitcasend: cases data to be fitted on
    #:param deaths_data_fit: deaths data to be fitted on
    #:param weights: time-related weights to give more importance to recent data points in the fit (in the loss function)
    #:return: float, corresponding to the value of the loss function
    
    if (optimizer %in% c("tnc", "trust-constr")) {
      residuals_value = sum(
        np.multiply((x_sol[15, ] - cases_data_fit) ** 2, weights)
        + balance
        * balance
        * np.multiply((x_sol[14, ] - deaths_data_fit) ** 2, weights)
      )
    } 
    else if (optimizer == "annealing") 
    {
        residuals_value = sum(
          np.multiply((x_sol[15, ] - cases_data_fit) ** 2, weights)
          + balance
          * balance
          * np.multiply((x_sol[14, ] - deaths_data_fit) ** 2, weights)
        ) + sum(
          np.multiply(
            (x_sol[15, 7:length(x_sol[15])] - x_sol[15, 1:-7] - cases_data_fit[7:length(cases_data_fit)] + cases_data_fit[1:-7]) ** 2,
            weights[7:length(weights)],
          )
          + balance * balance * np.multiply(
            (x_sol[14, 7:length(x_sol[14])] - x_sol[14, 1:-7] - deaths_data_fit[7:length(deaths_data_fit)] + deaths_data_fit[1:-7]) ** 2,
            weights[7:length(weights)],
          )
      )
    }
    else {
      stop("Optimizer not in 'tnc', 'trust-constr' or 'annealing' so not supported")
    }
    
    residuals_value
}

get_mape_data_fitting <- get_mape_data_fitting(cases_data_fit, deaths_data_fit, x_sol_final) # -> float:
{
    #Computes MAPE on cases & deaths (averaged) either on last 15 days of historical data (if there are more than 15)
    #or exactly the number of days in the historical data (if less than 15)
    #:param cases_data_fit: list, contains data used to fit on number of cases
    #:param deaths_data_fit: list, contains data used to fit on number of deaths
    #:param x_sol_final: numpy array, contains the predicted solution by the DELPHI model for all 16 states
    #:return: a float corresponding to the average MAPE on cases and deaths on a given period of time (15 days is default
    #but otherwise the number of days available in the historical data)
    
    cases_data_fit <- unlist(cases_data_fit)
    deaths_data_fit <- unlist(deaths_data_fit)
  
    if (length(cases_data_fit) > 15) {  # In which case we can compute MAPE on last 15 days
      mape_data <- (
        compute_mape(
          #cases_data_fit[-15:],
          #x_sol_final[15, len(cases_data_fit) - 15: len(cases_data_fit)],
          tail(cases_data_fit, 15), x_sol_final[16, length(cases_data_fit) - 15 : length(cases_data_fit) ]
        ) + compute_mape(
          #deaths_data_fit[-15:],  x_sol_final[14, len(cases_data_fit) - 15: len(deaths_data_fit)],
          tail(deaths_data_fit, 15), x_sol_final[15, len(cases_data_fit) - 15: len(deaths_data_fit)]
        )
      ) / 2
    }
    else 
    {  # We take MAPE on all available previous days (less than 15)
      mape_data <- (
        #compute_mape(cases_data_fit, x_sol_final[15, : len(cases_data_fit)])  + compute_mape(deaths_data_fit, x_sol_final[14, : len(deaths_data_fit)])
        compute_mape(cases_data_fit, x_sol_final[16, 1: length(cases_data_fit)]) +
          + compute_mape(deaths_data_fit, x_sol_final[15, 1: length(deaths_data_fit)])
        ) / 2
    }
    mape_data
}

compute_sign_mape <- function(y_true, y_pred)
{
    #Compute the sign of the Mean Percentage Error, mainly to know if we're constantly over or undershooting
    #:param y_true: list of true historical values
    #:param y_pred: list of predicted values
    #:return: a float, +1 or -1 for the sign of the MPE
  
  # Mean Percentage Error, without the absolute value
    #y_true, y_pred = np.array(y_true), np.array(y_pred)
    
    y_true <- unlist(y_true)
    y_pred <- unlist(y_pred)
    
    mpe <- mean((y_true - y_pred)[y_true > 0] / y_true[y_true > 0]) * 100
    sign(mpe)
}

compute_mape_daily_delta_since_last_train <- function( true_last_train, pred_last_train, y_true, y_pred) 
{
        
  #Computed the Mean Absolute Percentage Error between the daily differences of prediction between a previous train
  #      and a current train true/predicted values
  #      :param true_last_train: list of true historical values for the previous train considered
  #      :param pred_last_train: list of predicted values for the previous train considered
  #      :param y_true: list of true historical values for the current train considered
  #      :param y_pred: list of predicted values for the current train considered
  #      :return: a float corresponding between the MAPE between the daily differences of prediction/truth between 2
  #      different training processes
   
    #delta_true = np.array([y_true_i - true_last_train for y_true_i in y_true])
    
    delta_true <- c()
    for ( y_true_i in y_true ) {
      delta_true <- c(delta_true, y_true_i - true_last_train)
    }
    
    delta_pred <- c()
    #delta_pred = np.array([y_pred_i - pred_last_train for y_pred_i in y_pred])
    for ( y_pred_i in y_pred ) {
      delta_pred <- c(delta_pred, y_pred_i - pred_last_train)
    }
    
    mape_daily_delta <- (
      mean(
        abs(delta_true - delta_pred)[delta_true > 0] / delta_true[delta_true > 0]
      )
      * 100
    )
    mape_daily_delta
}

compute_mse <- function(y_true, y_pred) # -> float:
{
    #Compute the Mean Squared Error between two lists
    #:param y_true: list of true historical values
    #:param y_pred: list of predicted values
    #:return: a float, corresponding to the MSE
    
  #y_true, y_pred = np.array(y_true), np.array(y_pred)
  y_true <- unlist(y_true)
  y_pred <- unlist(y_pred)
  mse = mean((y_true - y_pred) ** 2)
  float(mse)
}

compute_mae_and_mape <- function(y_true, y_pred)
{
    #Compute the Mean Absolute Error (MAE) and Mean Absolute Percentage Error (MAPE) between two lists of values
    #:param y_true: list of true historical values
    #:param y_pred: list of predicted values
    #:return: a tuple of floats, corresponding to (MAE, MAPE)
    
    #y_true, y_pred = np.array(y_true), np.array(y_pred)
    y_true <- unlist(y_true)
    y_pred <- unlist(y_pred)
    mae <- mean(np.abs((y_true - y_pred)))
    mape <- mean(np.abs((y_true - y_pred)[y_true > 0] / y_true[y_true > 0])) * 100
    list(mae, mape)
}


compute_mape <- functio(y_true, y_pred) # -> float:
{
    #Compute the Mean Absolute Percentage Error (MAPE) between two lists of values
    #:param y_true: list of true historical values
    #:param y_pred: list of predicted values
    #:return: a float corresponding to the MAPE
    
  #y_true, y_pred = np.array(y_true), np.array(y_pred)
  
  y_true <- unlist(y_true)
  y_pred <- unlist(y_pred)
  
  mape <- mean(np.abs((y_true - y_pred)[y_true > 0] / y_true[y_true > 0])) * 100
  mape
}
  




