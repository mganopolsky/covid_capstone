## Importing packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(hexbin)) install.packages("hexbin", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")
if(!require(formatR)) install.packages("formatR", repos = "http://cran.us.r-project.org")
if(!require(archivist)) install.packages("archivist", repos = "http://cran.us.r-project.org")
if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
if(!require(splines)) install.packages("splines", repos = "http://cran.us.r-project.org")
if(!require(yaml)) install.packages("yaml", repos = "http://cran.us.r-project.org")
if(!require(argparser)) install.packages("argparser", repos = "http://cran.us.r-project.org")
if(!require(itertools)) install.packages("itertools", repos = "http://cran.us.r-project.org")
if(!require(deSolve)) install.packages("deSolve", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(Rtnmin)) install.packages("Rtnmin", repos = "http://cran.us.r-project.org")
if(!require(trust)) install.packages("trust")

library(splines)
library(yaml)
library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)
library(ggrepel)
library(ggplot2)
library(cowplot)
library(dplyr)
library(here)
library(formatR)
library(archivist)
library(devtools)
library(futile.logger)
library(sets)
library(argparser)
library(iterators)
library(itertools)
library(deSolve)
library(Rtnmin)
library(trust)

## Reading in file

#dir <- "../data"
#files <- list.files(dir)
#files
#file_name <- paste(dir, "covid_19_clean_complete.csv", sep = "/")
#all_data <- read_csv(file_name)

#source("params.r")
#source("capstone_utils.R")
source("covid_classes.R")



solve_and_predict_area <- function(
  tuple_area_,  yesterday_, past_parameters_, popcountries )
{
    #Parallelizable version of the fitting & solving process for DELPHI V3, this function is called with multiprocessing
    #:param tuple_area_: tuple corresponding to (continent, country, province)
    #:param yesterday_: string corresponding to the date from which the model will read the previous parameters. The
    #format has to be 'YYYYMMDD'
    #:param past_parameters_: Parameters from yesterday_ used as a starting point for the fitting process
    #:return: either None if can't optimize (either less than 100 cases or less than 7 days with 100 cases) or a tuple
    #with 3 dataframes related to that tuple_area_ (parameters df, predictions since yesterday_+1, predictions since
    #first day with 100 cases) and a scipy.optimize object (OptimizeResult) that contains the predictions for all
    #16 states of the model (and some other information that isn't used)
    
    time_entering <- Sys.time()
    continent <- tuple_area_[[1]]
    country <- tuple_area_[[2]] 
    province <- tuple_area_[[3]]
    country_sub <- str_replace_all( country, " ", "_")
    province_sub <- str_replace_all( provinc, " ", "_") #province.replace(" ", "_")
    if (dir.exists(paste(PATH_TO_FOLDER_DANGER_MAP , "processed/Global/Cases_" , country_sub, "_", province_sub, ".csv", sep = "")))
    {
      totalcases <- read_csv( paste(PATH_TO_FOLDER_DANGER_MAP , "processed/Global/Cases_", country_sub, "_", province_sub, ".csv", sep = ""))
      
      if  ( max(totalcases$day_since100) < 0 ) {
          flog.warn(paste("Not enough cases (less than 100) for Continent=", continent, " Country=", country, " and Province= ", province, sep = ""))
          return (NULL)
      }
    

      if ( !is.null(  past_parameters_)) 
      { # is not None:
            
        #parameter_list_total <- past_parameters_[
        #  (past_parameters_.Country == country)   & (past_parameters_.Province == province)
        #  ].reset_index(drop=True)
        
        parameter_list_total <- past_parameters_ %>% filter(Country == country & Province == province)
        
        if (length(parameter_list_total) > 0 )
        {
          #parameter_list_line = parameter_list_total.iloc[-1, :].values.tolist()
          
          parameter_list_line <- parameter_list_total.iloc %>% slice_tail(1) %>% list() 
          
          #parameter_list = parameter_list_line[5:]
          
          #must check out the parameter list here  - do we check index 5 or 6?
          parameter_list <- parameter_list_line[[1]][6:length(parameter_list_line[[1]])]
          
          bounds_params <- get_bounds_params_from_pastparams(
            optimizer=OPTIMIZER,
            parameter_list=parameter_list,
            dict_default_reinit_parameters=dict_default_reinit_parameters,
            percentage_drift_lower_bound=percentage_drift_lower_bound,
            default_lower_bound=default_lower_bound,
            dict_default_reinit_lower_bounds=dict_default_reinit_lower_bounds,
            percentage_drift_upper_bound=percentage_drift_upper_bound,
            default_upper_bound=default_upper_bound,
            dict_default_reinit_upper_bounds=dict_default_reinit_upper_bounds,
            percentage_drift_lower_bound_annealing=percentage_drift_lower_bound_annealing,
            default_lower_bound_annealing=default_lower_bound_annealing,
            percentage_drift_upper_bound_annealing=percentage_drift_upper_bound_annealing,
            default_upper_bound_annealing=default_upper_bound_annealing,
            default_lower_bound_jump=default_lower_bound_jump,
            default_upper_bound_jump=default_upper_bound_jump,
            default_lower_bound_std_normal=default_lower_bound_std_normal,
            default_upper_bound_std_normal=default_upper_bound_std_normal,
          )
          
          #date_day_since100 = pd.to_datetime(parameter_list_line[3])
          
          date_day_since100 = as.Date(parameter_list_line[[1]][3])
          
          
          #validcases = totalcases[  (totalcases.day_since100 >= 0)  & (totalcases.date <= str((pd.to_datetime(yesterday_) + timedelta(days=1)).date()))
          #  ][["day_since100", "case_cnt", "death_cnt"]].reset_index(drop=True)
          
          validcases <- totalcases %>% filter(day_since100 >= 0 & date <= (as.date(yesterday_ + 1))) %>%
            select(day_since100, case_cnt, death_cnt)
          
          #bounds_params = list(bounds_params)
        }
        else  # Otherwise use established lower/upper bounds
        {
          parameter_list <- default_parameter_list
          bounds_params <- default_bounds_params
          #date_day_since100 = pd.to_datetime(totalcases.loc[totalcases.day_since100 == 0, "date"].iloc[-1])
          date_day_since100 <- totalcases %>% filter(day_since100 == 0) %>% select(date) %>% 
                    slice_tail(1) %>% as.Date()
          #validcases = totalcases[ (totalcases.day_since100 >= 0) & 
          #    (totalcases.date <= str((pd.to_datetime(yesterday_) + timedelta(days=1)).date()))
            #][["day_since100", "case_cnt", "death_cnt"]].reset_index(drop=True)
          
          validcases <- totalcases %>% filter(day_since100 >= 0 & date <= (as.date(yesterday_) + 1)) %>%
            select("day_since100", "case_cnt", "death_cnt")
          
        } 
      }
      else  # Otherwise use established lower/upper bounds
      {
       
        parameter_list <- default_parameter_list
        bounds_params <- default_bounds_params
        date_day_since100_row <- totakcases %>% filter(day_since100 == 0) %>% select(date) %>% slice_tail(1) 
        date_day_since100 <- as.Date(date_day_since100_row$date)
         
        validcases <- totalcases %>% filter(day_since100 >= 0 & date <= (as.Date(yesterday_) + 1)) %>%
          select("day_since100", "case_cnt", "death_cnt")
      }
      
      # Now we start the modeling part:
      if (length(validcases) <= validcases_threshold) {
            flog.warn(  paste("Not enough historical data (less than a week)",
               "for Continent=", continent," Country=", country, " and Province=", province, sep = ""  ))
        return (NULL)
      }
      else
      {
        #PopulationT = popcountries[
        #  (popcountries.Country == country) & (popcountries.Province == province)
        #  ].pop2016.iloc[-1]
        
        PopulationT <- popcountries %>% filter(Country == country & Province == province) %>% select(pop2016) %>%
          slice_tail[1]
        N <- PopulationT$pop2016
        
        #N = PopulationT
        PopulationI = validcases.loc[0, "case_cnt"]
        PopulationR = validcases.loc[0, "death_cnt"] * 5
        PopulationD = validcases.loc[0, "death_cnt"]
        PopulationCI = PopulationI - PopulationD - PopulationR
        if (PopulationCI <= 0 )
        {
            flog.error(paste("PopulationCI value is negative (",  PopulationCI, "), need to check why", sep = ""))
            stop(paste("PopulationCI value is negative (", PopulationCI, "), need to check why", sep = ""))
        }
                  #  Fixed Parameters based on meta-analysis:
                  #  p_h: Hospitalization Percentage
                  #  RecoverHD: Average Days until Recovery
                  #  VentilationD: Number of Days on Ventilation for Ventilated Patients
                  #  maxT: Maximum # of Days Modeled
                  #  p_d: Percentage of True Cases Detected
                  #  p_v: Percentage of Hospitalized Patients Ventilated,
                  #  balance: Regularization coefficient between cases and deaths 
                   
        #maxT <- diff.Date(default_maxT - date_day_since100).days + 1
        maxT <- as.numeric(difftime(default_maxT, date_day_since100, units="days")) + 1
        #t_cases <- validcases["day_since100"].tolist() - validcases.loc[0, "day_since100"]
        t_cases <- validcases["day_since100"][2:length(validcases)]
        
        data_list <- create_fitting_data_from_validcases(validcases)
        balance <- data_list[[1]]
        cases_data_fit <- data_list[[2]] 
        deaths_data_fit  <- data_list[[3]] 
          
          
        GLOBAL_PARAMS_FIXED <- list(N, PopulationCI, PopulationR, PopulationD, PopulationI, p_d, p_h, p_v)
  
  
        model_covid <- #function(t, alpha, days, r_s, r_dth, p_dth, r_dthdecay, k1, k2, jump, t_jump, std_normal) 
           function(y , times,  params, t)
        {
            with(as.list(params))
            {
                #SEIR based model with 16 distinct states, taking into account undetected, deaths, hospitalized and
                #recovered, and using an ArcTan government response curve, corrected with a Gaussian jump in case of
                #a resurgence in cases
                #:param t: time step
                #:param y: set of all the states in the model (here, 16 of them)
                #:param alpha: Infection rate
                #:param days: Median day of action (used in the arctan governmental response)
                #:param r_s: Median rate of action (used in the arctan governmental response)
                #:param r_dth: Rate of death
                #:param p_dth: Initial mortality percentage
                #:param r_dthdecay: Rate of decay of mortality percentage
                #:param k1: Internal parameter 1 (used for initial conditions)
                #:param k2: Internal parameter 2 (used for initial conditions)
                #:param jump: Amplitude of the Gaussian jump modeling the resurgence in cases
                #:param t_jump: Time where the Gaussian jump will reach its maximum value
                #:param std_normal: Standard Deviation of the Gaussian jump (~ time span of the resurgence in cases)
                #:return: predictions for all 16 states, which are the following
                #[1 S, 2 E, 3 I, 4 UR, 5 DHR, 6 DQR, 7 UD, 8 DHD, 9 DQD, 10 R, 11 D, 12 TH, 13 DVR,14 DVD, 15 DD, 16 DT]
                
              
                r_i <- log(2) / IncubeD  # Rate of infection leaving incubation phase
                r_d <- log(2) / DetectD  # Rate of detection
                r_ri <- log(2) / RecoverID  # Rate of recovery not under infection
                r_rh <- log(2) / RecoverHD  # Rate of recovery under hospitalization
                r_rv <- log(2) / VentilatedD  # Rate of recovery under ventilation
                gamma_t <- (
                  (2 / pi) * arctan(-(t - days) / 20 * r_s) + 1 + jump * exp(-(t - t_jump) ^ 2 / (2 * std_normal ^ 2))
                )
                p_dth_mod <- (2 / pi) * (p_dth - 0.01) * (arctan(-t / 20 * r_dthdecay) + pi / 2) + 0.01
                
                if (length(y) == 16) {
                  stop(paste("Too many input variables, got ", length(y), ", expected 16", sep = ""))
                }
                
                #S, E, I, AR, DHR, DQR, AD, DHD, DQD, R, D, TH, DVR, DVD, DD, DT = y
                S <- y[1]
                E <- y[2] 
                I <- y[3]
                AR<- y[4]
                DHR <- y[5]
                DQR <- y[6] 
                AD <- y[7] 
                DHD <- y[8]
                DQD <- y[9] 
                R<- y[10]
                D <- y[11]
                TH <- y[12]
                DVR <- y[13]
                DVD <- y[14]
                DD <- y[15]
                DT <- y[16]
                
                # Equations on main variables
                dSdt <- -alpha * gamma_t * S * I / N
                dEdt  <-  alpha * gamma_t * S * I / N - r_i * E
                dIdt  <-  r_i * E - r_d * I
                dARdt  <-  r_d * (1 - p_dth_mod) * (1 - p_d) * I - r_ri * AR
                dDHRdt  <-  r_d * (1 - p_dth_mod) * p_d * p_h * I - r_rh * DHR
                dDQRdt  <-  r_d * (1 - p_dth_mod) * p_d * (1 - p_h) * I - r_ri * DQR
                dADdt  <-  r_d * p_dth_mod * (1 - p_d) * I - r_dth * AD
                dDHDdt  <-  r_d * p_dth_mod * p_d * p_h * I - r_dth * DHD
                dDQDdt  <-  r_d * p_dth_mod * p_d * (1 - p_h) * I - r_dth * DQD
                dRdt  <-  r_ri * (AR + DQR) + r_rh * DHR
                dDdt  <-  r_dth * (AD + DQD + DHD)
                # Helper states (usually important for some kind of output)
                dTHdt  <-  r_d * p_d * p_h * I
                dDVRdt  <-  r_d * (1 - p_dth_mod) * p_d * p_h * p_v * I - r_rv * DVR
                dDVDdt  <-  r_d * p_dth_mod * p_d * p_h * p_v * I - r_dth * DVD
                dDDdt  <-  r_dth * (DHD + DQD)
                dDTdt  <-  r_d * p_d * I
            }
            return (list(
              dSdt, dEdt, dIdt, dARdt, dDHRdt, dDQRdt, dADdt, dDHDdt,
              dDQDdt, dRdt, dDdt, dTHdt, dDVRdt, dDVDdt, dDDdt, dDTdt
            ))
        }
        
        residuals_totalcases <- function(params) 
        {
         
         #       Function that makes sure the parameters are in the right range during the fitting process and computes
        #        the loss function depending on the optimizer that has been chosen for this run as a global variable
        #        :param params: currently fitted values of the parameters during the fitting process
        #        :return: the value of the loss function as a float that is optimized against (in our case, minimized)
         
        # Variables Initialization for the ODE system
          #alpha, days, r_s, r_dth, p_dth, r_dthdecay, k1, k2, jump, t_jump, std_normal = params
          alpha <- params[1]
          days <- params[2]
          r_s <- params[3]
          r_dth <- params[4]
          p_dth <- params[5] 
          r_dthdecay <- params[6] 
          k1 <- params[7] 
          k2 <- params[8] 
          jump <- params[9] 
          t_jump <- params[10] 
          std_normal <- params[11]
          # Force params values to stay in a certain range during the optimization process with re-initializations
          c_params <- c()
          c_params <- c(c_params, alpha = max(alpha, dict_default_reinit_parameters["alpha"]))
          c_params <- c(c_params, days = days)
          c_params <- c(c_params, r_s = max(r_s, dict_default_reinit_parameters["r_s"]))
          c_params <- c(c_params, r_dth = max(min(r_dth, 1), dict_default_reinit_parameters["r_dth"]))
          c_params <- c(c_params, p_dth = max(min(p_dth, 1), dict_default_reinit_parameters["p_dth"]))
          c_params <- c(c_params, r_dthdecay =  max(r_dthdecay, dict_default_reinit_parameters["r_dthdecay"]))
          c_params <- c(c_params, k1 = max(k1, dict_default_reinit_parameters["k1"]))
          c_params <- c(c_params, k2 = max(k2, dict_default_reinit_parameters["k2"]))
          c_params <- c(c_params, jump = max(jump, dict_default_reinit_parameters["jump"]))
          c_params <- c(c_params, t_jump = max(t_jump, dict_default_reinit_parameters["t_jump"]))
          c_params <- c(c_params, std_normal = max(std_normal, dict_default_reinit_parameters["std_normal"]))
          
          #params[1] <- max(alpha, dict_default_reinit_parameters["alpha"])
          #params[2] <-  days
          #params[3] <-  max(r_s, dict_default_reinit_parameters["r_s"])
          #params[4] <-   max(min(r_dth, 1), dict_default_reinit_parameters["r_dth"])
          #params[5] <-   max(min(p_dth, 1), dict_default_reinit_parameters["p_dth"])
          #params[6] <-   max(r_dthdecay, dict_default_reinit_parameters["r_dthdecay"])
          #params[7] <-  max(k1, dict_default_reinit_parameters["k1"])
          #params[8] <-   max(k2, dict_default_reinit_parameters["k2"])
          #params[9] <-  max(jump, dict_default_reinit_parameters["jump"])
          #params[10] <-  max(t_jump, dict_default_reinit_parameters["t_jump"])
          #params[11] <-   max(std_normal, dict_default_reinit_parameters["std_normal"])
          
          x_0_cases <- get_initial_conditions(
            params_fitted=params, global_params_fixed=GLOBAL_PARAMS_FIXED
          )
          
          #x_sol <- solve_ivp(
          #  fun=model_covid,
          #    y0=x_0_cases,
          #  t_span=t_cases,    #[t_cases[0], t_cases[-1]],
          #  t_eval=t_cases,
          #  args=tuple(params),
          #).y
          
          
          x_sol <- deSolve::ode(
            y = x_0_cases,
            times = c(1:t_predictions),
            func = model_covid,
            parms = params, # vector of parameter values
            method = "ode45",
            t = t_predictions
          )[ , -1] 
          
          weights = c(range(1, len(cases_data_fit) + 1))
          
          #residuals_value = get_residuals_value(optimizer=OPTIMIZER, balance=balance, x_sol=x_sol,
          #  cases_data_fit=cases_data_fit, deaths_data_fit=deaths_data_fit, weights=weights)
          
          residuals_value <- get_residuals_value(optimizer=OPTIMIZER, balance=balance, x_sol=x_sol,
                                  cases_data_fit=cases_data_fit, deaths_data_fit=deaths_data_fit, weights=weights)
          
          return (residuals_value)
        }
        
        if (OPTIMIZER == "tnc") #truncated newton constrained
        {
          #need to figure this out
          output <- tnbc(x, fgfun, lower, upper, trace=FALSE)
        }
        else if (OPTIMIZER == "trust-constr")
        {
          
          ### need to figure this out!!!!
          #TODO:
          output <- trust(objfun=residuals_totalcases, parainit=parameter_list, iterlim=max_iter, )
          
        } else if ( OPTIMIZER == "annealing") 
        {
          output = dual_annealing( residuals_totalcases, x0=parameter_list, bounds=bounds_params )
        }
        else {
          stop("Optimizer not in 'tnc', 'trust-constr' or 'annealing' so not supported")
        }
    
        best_params <- output$x
        #going to ignore t_predictions in favor of using actual values
        #t_predictions = [i for i in range(maxT)]
        
        solve_best_params_and_predict <- function(optimal_params)
        {
          # Variables Initialization for the ODE system
            if (OPTIMIZER %in% c("tnc" , "trust-constr"))
            {
                alpha <- optimal_params[1] 
                days <- optimal_params[2] 
                r_s <- optimal_params[3] 
                r_dth <- optimal_params[4] 
                p_dth <- optimal_params[5] 
                r_dthdecay <- optimal_params[6]
                k1 <- optimal_params[7]
                k2 <- optimal_params[8] 
                jump <- optimal_params[9] 
                t_jump <- optimal_params[10] 
                std_normal <- optimal_params[11]
                optimal_params <- c(
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
            }
            x_0_cases <- get_initial_conditions(
              params_fitted=optimal_params,
              global_params_fixed=GLOBAL_PARAMS_FIXED,
            )
  
            x_sol_best <- deSolve::ode(
              y = x_0_cases,
              times = c(1:t_predictions),
              func = model_covid,
              parms = params, # vector of parameter values
              method = "ode45",
              t = t_predictions
            )[ , -1] 
            
            return (x_sol_best)
        }
        
        x_sol_final <- solve_best_params_and_predict(best_params)
        
        data_creator <- new( "DELPHIDataCreator",
          x_sol_final=x_sol_final,
          date_day_since100=date_day_since100,
          best_params=best_params,
          continent=continent,
          country=country,
          province=province,
          testing_data_included=False
        )
        
        mape_data <- get_mape_data_fitting(
          cases_data_fit=cases_data_fit, deaths_data_fit=deaths_data_fit, x_sol_final=x_sol_final
        )
        
        #logging.info(f"In-Sample MAPE Last 15 Days {country, province}: {round(mape_data, 3)} %")
        
        flog.info("In-Sample MAPE Last 15 Days %s, %s:, %d %", country, province, round(mape_data, 3))
        #logging.debug(f"Best fitted parameters for {country, province}: {best_params}")
        flog.debug("Best fitted parameters for %s, %s: %s", country, province, best_params)
        
        
        df_parameters_area <- data_creator@create_dataset_parameters(mape_data)
        # Creating the datasets for predictions of this area
        if (GET_CONFIDENCE_INTERVALS) {
          output <-   data_creator@create_datasets_with_confidence_intervals(
              cases_data_fit, deaths_data_fit,
              past_prediction_file=PATH_TO_FOLDER_DANGER_MAP + paste("predicted/Global_V2_", format( as.Date(past_prediction_date), "%Y%m%d"), ".csv", sep = ""),
              past_prediction_date= past_prediction_date)
        }
        else {
          output <- data_creator@create_datasets_predictions()
        }
        
        df_predictions_since_today_area <- output[[1]]
        df_predictions_since_100_area <- output[[2]]
        
        flog.info("Finished predicting for Continent=%s, Country=%s and Province=%s in %d seconds)", 
                  continent, country, province, round(time.time() - time_entering, 2))
        flog.info("--------------------------------------------------------------------------------------------")
        return ( list(
          df_parameters_area,
          df_predictions_since_today_area,
          df_predictions_since_100_area,
          output)
        )
      }
      
    }
    else 
    {
      flog.info("Skipping Continent=%s, Country=%s and Province=%s as no processed file available",
                continent, country, province)
      return (NULL)   
    }
}
  
  #main code to be executed
  
main <- function() {
  
  cfg <- read_yaml("config.yml")
  CONFIG_FILEPATHS = unlist(cfg$filepaths)
  
  time_beginning <- Sys.time()
  yesterday <- format(Sys.Date() - 1, "%Y%m%d")
  
  yesterday_logs_filename <- paste(
    yesterday , "_", format(Sys.time(), "%H%M", sep = "")
  )
  
  parser <- arg_parser("Delphi")
  
  parser <- add_argument(parser, "--optimizer", type="character", help= paste(
    "Which optimizer among 'tnc', 'trust-constr' or 'annealing' would you like to use ? " ,
    "Note that 'tnc' and 'trust-constr' lead to local optima, while 'annealing' is a " ,
    "method for global optimization: ",  sep = "")
    , default="tnc", short='-o')
  
  parser <- add_argument(parser, "--confidence_intervals", type="numeric", 
                         help= "Generate Confidence Intervals? Reply 0 or 1 for False or True.", 
                         default=1, short='-ci')
  
  parser <- add_argument(parser, "--since100case", type="numeric", 
                         help= "Save all history (since 100 cases)? Reply 0 or 1 for False or True.", 
                         default=1, short='-s100')
  
  arguments <- parse_args(parser)
  
  
  OPTIMZER <- arguments$optimizer
  GET_CONFIDENCE_INTERVALS <- as.logical (arguments$confidence_intervals)
  SAVE_SINCE100_CASES <- as.logical(arguments$since100case)
  PATH_TO_FOLDER_DANGER_MAP = CONFIG_FILEPATHS["danger_map"]
  
  #past_prediction_date <- format(Sys.Date() - 14, "%Y%m%d")
  past_prediction_date - Sys.Date() - 14 # last prediction date of 14 days ago


  if (! dir.exists(paste( CONFIG_FILEPATHS["logs"] + "model_fitting/", sep = "" )))
    dir.create(paste( CONFIG_FILEPATHS["logs"] + "model_fitting/", sep = "" ))
  
  logger_filename <- paste(CONFIG_FILEPATHS["logs"], "model_fitting/delphi_model_V3_", yesterday_logs_filename, "_" , OPTIMIZER, ".log", sep = "")
  logger_name <- "covid_capstone"
  
  flog.logger(logger_name, threshold=DEBUG, appender=appender.file(logger_filename))
  
  flog.info("The chosen optimizer for this run was %s and 'generation of Confidence Intervals' flag is %s", 
            OPTIMIZER, GET_CONFIDENCE_INTERVALS)
  
  popcountries <- read.csv(paste(PATH_TO_FOLDER_DANGER_MAP , "processed/Global/Population_Global.csv", sep = ""))
  
  popcountries["tuple_area"] <- #itertools::izip(Continent = popcountries.Continent, Country = popcountries.Country, Province = popcountries.Province)
      data.frame(Continent = popcountries.Continent, Country = popcountries.Country, Province = popcountries.Province)
  
  tryCatch ({
    past_parameters <- read.csv(paste(   PATH_TO_FOLDER_DANGER_MAP, "predicted/Parameters_Global_V2_", yesterday, ".csv", sep = ""))
  }, finally = {
    past_parameters <- NULL          
  })
          
  list_df_global_predictions_since_today <- list()
  list_df_global_predictions_since_100_cases <- list()
  list_df_global_parameters <- list()
  obj_value = 0
  
  list_tuples <- as.list(popcountries$tuple_area)
  
  flog.info("Number of areas to be fitted in this run: %d", length(list_tuples))
        
  
  for(tuple in list_tuples) {
    result_area <- solve_and_predict_area( tuple,  yesterday, past_parameters, popcountries = popcountries)
    if ( is.null(  result_area ))
    {
      
      df_parameters_area <- result_area[[1]]
      df_predictions_since_today_area <- result_area[[2]]
      df_predictions_since_100_area <- result_area[[3]]
      output <- result_area[[4]]
      
      obj_value <- obj_value + output$fun
      # Then we add it to the list of df to be concatenated to update the tracking df
      list_df_global_parameters.append(df_parameters_area)
      list_df_global_predictions_since_today.append(df_predictions_since_today_area)
      list_df_global_predictions_since_100_cases.append(df_predictions_since_100_area)
    }
  }
  flogging.info("Finished the processing for all areas")   
        
  today_date_str <-  format(Sys.Date(), "%Y%m%d")
  df_global_parameters = data.frame()
  for (item in list_df_global_parameters) 
    df_global_parameters <- bind_rows( df_global_parameters, item) 
  df_global_parameters <- df_global_parameters %>% arrange("Country", "Province")
  
  df_global_predictions_since_today = data.frame()
  for (item in list_df_global_predictions_since_today) 
    df_global_predictions_since_today <- bind_rows( df_global_predictions_since_today, item) 
  
  df_global_predictions_since_today <- DELPHIAggregations$append_all_aggregations(
    df_global_predictions_since_today
  )
  
  df_global_predictions_since_100_cases = data.frame()
  for (item in list_df_global_predictions_since_100_cases) 
    df_global_predictions_since_100_cases <- bind_rows( df_global_predictions_since_100_cases, item) 
  
  if (GET_CONFIDENCE_INTERVALS) 
  {
    output <- DELPHIAggregations.append_all_aggregations_cf(
      df_global_predictions_since_100_cases,
      past_prediction_file= paste(PATH_TO_FOLDER_DANGER_MAP , "predicted/Global_V2_", format(past_prediction_date, "%Y%m%d") , ".csv", sep = ""),
      past_prediction_date=past_prediction_date
    )
    
    df_global_predictions_since_today <- output[[1]]
    df_global_predictions_since_100_cases <- output[[2]]
    
  } else { 
    df_global_predictions_since_100_cases = DELPHIAggregations$append_all_aggregations( df_global_predictions_since_100_cases )
  }
  
  delphi_data_saver <- new("DELPHIDataSaver",
    path_to_folder_danger_map=PATH_TO_FOLDER_DANGER_MAP,
    path_to_website_predicted=PATH_TO_WEBSITE_PREDICTED,
    df_global_parameters=df_global_parameters,
    df_global_predictions_since_today=df_global_predictions_since_today,
    df_global_predictions_since_100_cases=df_global_predictions_since_100_cases,
  )
  delphi_data_saver@save_all_datasets(optimizer=OPTIMIZER, save_since_100_cases=SAVE_SINCE100_CASES, website=SAVE_TO_WEBSITE)
  
  flogger.info("Exported all 3 datasets to website & danger_map repositories, total runtime was %d minutes", 
               round((time.time() - time_beginning)/60, 2))

}


