    
    
    
    ################################################
    #### Author: Nafis Sadat
    #### Purpose: Running an ARIMA model across countries in TMB
    #### Created: 2017/02/12
    ################################################
    
    
    rm(list=ls())
    library(AFModel)
    
    ################################################
    ## (1) Get model information
    ################################################
    
      ## Model and task has 1:1 mapping in means
      model = Sys.getenv('SGE_TASK_ID')
      if(model == "" | is.na(model)) {
        model = 385
      }
      
      ## Parse the args and get model info

      ## Open the parser
        parser <- ArgumentParser()

        ## Specify version and description
        parser$add_argument("--variable", required = T, type = "character", help = "Variable")
        parser$add_argument("--date", required = T, type = "integer", help = "Date")
        parser$add_argument("--comment", required = T, type = "character", help = "Comment")
        parser$add_argument("--oos", required = T, type = "integer", help = "OOS")

        argss <- parser$parse_args()
        variable <- argss$variable; date <- argss$date; comment <- argss$comment; oos_years <- argss$oos
        

      print("Get grid")
      load(paste0('/share/resource_tracking/forecasting/', variable, '/', date, '_', comment, '/summary_files/mean_array.Rdata'))
      
      print("Getting model info")
      model_info <- get_model_specs(GDPs_full_grid, model, draws = F)
      print(paste0("Running model number ", model, " with the following specifications: "))
      print(model_info)
    
    
    
    ################################################
    ## (2) Prep all data with covariates
    ################################################
    
    ## Bring in full data
    input_data <-  data.table(read.csv(paste0('/share/resource_tracking/forecasting/', variable, '/input_data/RT_2018_GDP_use.csv')))[year<=metadata_list$end_FC]
    input_data <- copy(input_data)
     
    
    ## Keep only the columns we need
    # input_data <- input_data[,.SD, .SDcols = c('iso3', 'year', depvar, covars)]
    input_data <- input_data[year >= eval(metadata_list$start_year - 1)]
    input_data[, year_id:= as.numeric(year) - min(year)  ]
    
    ## Years of OOS
    # oos_years = 10
    
    ## Create scaled maxxer for convergence
    input_data[, conv_scaler:= max(get(gsub('ln\\_|logit\\_', '', model_info$yvar)), na.rm=T)]
    input_data[, Y_scaled_conv:= shift(get(gsub('ln\\_|logit\\_', '', model_info$yvar)) / conv_scaler), by = 'iso3']
    
    
    ################################################
    ## (3) Prepping for TMB
    ################################################
    
    ## Test and break convergence
    # input_data$ln_pop = .2
    # input_data$logit_pop15 = -.1
    
    data_params <- make_data_param(input_data = input_data, 
                                  yvar = model_info$yvar, 
                                  xvar = model_info$xvar, 
                                  REs= 'iso3', 
                                  global_int= model_info$global_int,
                                  country_int = model_info$country_int, 
                                  country_int_dist= model_info$country_int_dist, 
                                  re_vars = model_info$re_coef, 
                                  ar_mod = model_info$ar_mod, ar=model_info$ar, 
                                  ma_mod = model_info$ma_mod, ma=model_info$ma, 
                                  fd = model_info$fdiff, 
                                  weight_decay = model_info$weight_decay,
                                  conv= model_info$conv,
                                  scaled_lev_conv = model_info$scaled_lev_conv,
                                  start_year = metadata_list$start_year, 
                                  end_fit = metadata_list$end_fit, 
                                  end_FC = metadata_list$end_FC, 
                                  chaos = metadata_list$chaos,
                                  ar_constrain = model_info$ar_constrain)
    
    
    
    ################################################
    #### Full Sample Model Run ###
    ################################################
    
        
        
        ################################################
        ## (3.1) Compile, optimize and fit
        ################################################
        
        ## Run the TMB model
      
        system.time(tryCatch( output_TMB <-  run_model(data_params = data_params, 
                                                       model = 'AFModel', 
                                                       verbose = T),
          error = function(e) {
            # print(e)
            print(paste0("Model couldn't be run: ", e ));
            record_model_and_stop(model, variable = 'gdp', comment, date  , type = 'failed')
        }))
        
       
        ## Stop if false convergence but FC_model still evaluated, or we have NaNs in SEs
        if(output_TMB$opt$convergence == 1  | output_TMB$FC_model$pdHess == F | any(is.nan(summary(output_TMB$FC_model)[,2])))  {
          print("False convergence, or evaluation limit reached, or NaNs in SEs in main regression. Stopping script")
          record_model_and_stop(model, variable = 'gdp', comment, date  , type = 'failed')
        }
          
          
          
        
        ################################################
        ## (3.2) Prediction block 
        ################################################
        
        
        ### Make a dataset for mean estimation data
        mean_est_data <- prep_data_for_forecast(tmb_obj = output_TMB$obj, tmb_data_param = data_params )
        
        ### Forecast
        mean_forecast <- make_forecast(mean_est_data_obj = mean_est_data, tmb_output_object = output_TMB, tmb_data_param = data_params, add_residual = F, transform = 'exp')
        
        print("Mean forecast predicted")
        
        # i='USA'; plot(mean_forecast[iso3==i, .(year, yvar)], type = 'l', col='red');
        
        ################################################
        ## (3.3) Exclusion criteria test block
        ################################################
        
        ## Test whether the coefficients are stat sig
        stat_sig_test <- test_sig(output_TMB, rigorous = T)
        
        ## Test for growth rate bounds
          ## Run SFA models first
          sfa_model <- sfa_bounds(input_data, model_info$yvar, transform = 'log', panelvar = 'iso3', yearvar='year')
          
          ## Test bounds
          bounds_test <- test_bounds(data = mean_forecast, 
                                     depvar = 'yvar',  
                                     rev_trans = 'exp',
                                     predict_years = c(metadata_list$end_fit, metadata_list$end_FC), 
                                     panelvar = 'iso3', 
                                     yearvar = 'year', 
                                     sfa_output = sfa_model)
        
          
        ## Test for empirical priors (will be done in ensemble collector)
          
        
          ## If all of our bounds failed, stop model
          if(sum(stat_sig_test, bounds_test) != 2) {
            print("This model did not pass all of the exclusion criteria. You're a failure in life.")
            record_model_and_stop(model, variable = 'gdp', comment, date, type = 'failed')
          }
          
        
          ################################################
          ## (3.4) Create residuals
          ################################################
          
          residuals <- create_residuals(tmb_output_obj = output_TMB, tmb_data_param = data_params)  
          
          ## Get the statistics we usually care about from the resids (MAD, median, mean, SD, etc)
          level_resid_stats <- create_stats(data = residuals, 'iso3', variable = "level_resid")
          colnames(level_resid_stats) <- c('iso3', paste0('level_', c('mean', 'sd', 'median', 'mad')))
          diff_resid_stats <- create_stats(data = residuals, 'iso3', variable = "diff_resid")
          colnames(diff_resid_stats) <- c('iso3', paste0('diff_', c('mean', 'sd', 'median', 'mad')))
          
          ## Merge the two
          resid_info <- merge(level_resid_stats, diff_resid_stats, 'iso3')
          
    
    ######################################################
    #### Out of Sample Model Run and Finalizing Output ###
    ######################################################
    
    
          ################################################
          ## (4.1) Compile, optimize and fit
          ################################################
          
          ## Prep data for OOS
          data_params_OOS = make_data_param(input_data = input_data, 
                                            yvar = model_info$yvar, 
                                            xvar = model_info$xvar, 
                                            REs= 'iso3', 
                                            global_int= model_info$global_int,
                                            country_int = model_info$country_int, 
                                            country_int_dist= model_info$country_int_dist, 
                                            re_vars = NULL, 
                                            ar_mod = model_info$ar_mod, ar=model_info$ar, 
                                            ma_mod = model_info$ma_mod, ma=model_info$ma, 
                                            fd = model_info$fdiff, 
                                            weight_decay = model_info$weight_decay,
                                            conv= model_info$conv,
                                            scaled_lev_conv = model_info$scaled_lev_conv,
                                            start_year = metadata_list$start_year, 
                                            end_fit = eval(metadata_list$end_fit - oos_years), 
                                            end_FC = metadata_list$end_fit,
                                            chaos = metadata_list$chaos,
                                            ar_constrain = model_info$ar_constrain)
          
          
          
          system.time(tryCatch(output_TMB_OOS <-  run_model(data_params = data_params_OOS, 
                                                        model = 'AFModel', 
                                                        verbose = F),
                               error = function(e) {
                                 # print(e)
                                 print(paste0("Model couldn't be run:", e ));
                                 record_model_and_stop(model, variable = 'gdp', comment, date  , type = 'failed')
                                 
                               }))
          
          
          ## Stop if false convergence but FC_model still evaluated
          if(output_TMB_OOS$opt$convergence == 1  | output_TMB_OOS$FC_model$pdHess == F) {
            print("False convergence, or evaluation limit reached, or NaNs in SEs in OOS regression. Stopping script")
            record_model_and_stop(model, variable = 'gdp', comment, date  , type = 'failed')
          }
          
          
          
          
          ################################################
          ## (4.2) Prediction block 
          ################################################
          
          
          ### Make a dataset for mean estimation data
          mean_est_data_OOS <- prep_data_for_forecast(tmb_obj = output_TMB_OOS$obj, tmb_data_param = data_params_OOS )
          
          ### Forecast
          mean_forecast_OOS <- make_forecast(mean_est_data_obj = mean_est_data_OOS, 
                                             tmb_output_object = output_TMB_OOS, 
                                             tmb_data_param = data_params_OOS, 
                                             add_residual = F,
                                             transform = 'exp')
          
          print("Mean forecast for OOS run predicted")
          
          # i='USA'; plot(mean_forecast[iso3==i, .(year, yvar)], type = 'l', col='red'); lines(mean_forecast_OOS[iso3==i, .(year, yvar )], type = 'l', col='blue')
          
          
          
          ################################################
          ## (4.3) Generate RMSE by regions
          ################################################
          
          loc_DT_regs <- unique(input_data[,.(iso3, region_name, super_region_name)])
          
          
          ##################################################
          ########## Computing RMSEs (maybe Chaos, maybe not!)
          ##################################################
          
          
          if(metadata_list$chaos) {
            RMSE_data <- RMSE_generator_2(mean_data = mean_forecast, 
                                          OOS_data = mean_forecast_OOS,
                                          panelvar = 'iso3', 
                                          yearvar = 'year',
                                          depvar_dt = 'yvar',
                                          oos_start = eval(metadata_list$end_fit - oos_years + 1) ,
                                          in_sample_end = metadata_list$end_fit,
                                          region_metrics = T,
                                          region_data = loc_DT_regs)[, model_number:= model]  
          } else {
            RMSE_data <- RMSE_generator(mean_data = mean_forecast, 
                                          OOS_data = mean_forecast_OOS,
                                          panelvar = 'iso3', 
                                          yearvar = 'year',
                                          depvar_dt = 'yvar',
                                          oos_start = eval(metadata_list$end_fit - oos_years + 1) ,
                                          in_sample_end = metadata_list$end_fit,
                                          region_metrics = T,
                                          region_data = loc_DT_regs)[, model_number:= model]  
          } 
          
          
          
          ## Combine the two forecasts
          colnames(mean_forecast_OOS) <- c('iso3', 'year', 'yvar_OOS', 'ydiff_OOS')
          mean_forecast <- merge(mean_forecast, mean_forecast_OOS, c('iso3', 'year'), all.x=T)
          
          ################################################
          ## (4.4) Save out model info
          ################################################
          
          
          ## Merge postfile with a shell iso3 file (this is so that we can have country with NAs)
          country_list_DT <- loc_DT_regs[,.(iso3, model_number = model)]
          RMSE_data <- merge(RMSE_data, country_list_DT, c('model_number', 'iso3'), all.y=T)
          
          
          ## Record postfiles
          postfile <- postfile_maker(tmb_output_obj = output_TMB, tmb_data_param = data_params, model_number = model)
          
          ## Merge in RMSE (outer merge)
          postfile_iso <- merge(postfile, RMSE_data, "model_number", all.y=T)
          
          ## Outer merge the country residual info
          postfile_iso <- merge(postfile_iso, resid_info, 'iso3', all.x=T)
          
          ## Save the essentials to load in later
          save(list = c("postfile_iso", "output_TMB",  "mean_forecast", "data_params"),
               file = paste0('/share/resource_tracking/forecasting/', variable, '/', date, '_', comment, '/means/', model, '.Rdata'))
          
          ## Save just the postfile info in a csv
          fwrite(postfile_iso, 
                 paste0('/share/resource_tracking/forecasting/', variable, '/', date, '_', comment, '/means/', model, '.csv') )
          
          
          
          ## If we survived so far, save the model text file in passed!
          record_model_and_stop(model, variable = 'gdp', comment, date  , type = 'passed')
          
          q('no')
          
          
          
    
    
    
    
    
    