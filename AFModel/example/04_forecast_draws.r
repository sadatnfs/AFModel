    
    
    
    ################################################
    #### Author: Nafis Sadat
    #### Purpose: Create draws from a TMB run
    #### Created: 2018/05/22
    ################################################
    
    
    rm(list=ls())
    

    source('~/forecasting/AFModel/AFModel_funks.r')
    source('~/forecasting/AFModel/AFModel_helpers.r')
    
    
    ################################################
    ## (1) Get model information
    ################################################
    
      ## Get the Task ID
      id = Sys.getenv('SGE_TASK_ID')
      if(id == "" | is.na(id)) {
        id = 1
      }
      
      ## Parse the args and get model info
       
        ## Open the parser
        parser <- ArgumentParser()
        
        ## Specify version and description
        parser$add_argument("--variable", required = T, type = "character", help = "Variable")
        parser$add_argument("--date", required = T, type = "integer", help = "Date")
        parser$add_argument("--comment", required = T, type = "character", help = "Comment")
        
  
        argss <- parser$parse_args()
        variable <- argss$variable; date <- argss$date; comment <- argss$comment 
        
      ## Get mean grid
      load(paste0('/share/resource_tracking/forecasting/', variable, '/', date, '_', comment, '/summary_files/draws_array.Rdata'))
      
      ### Getting model info
      model_info <- get_model_specs(data = rmse_distro, model = id, draws = T)
      print(paste0("Running model number ", model_info$model, " with the following specifications and ", model_info$draws, " draws"))
      print(model_info)
    
    
    
    ################################################
    ## (2) Prep all data with covariates
    ################################################
    
    ## Bring in full data
    input_data <- data.table(read.csv('/home/j/Project/IRH/Forecasting/poverty/data_new/RT_2018_Povs_use.csv'))[year<=metadata_list$end_FC]
    input_data <- copy(input_data)
    input_data <- input_data[year >= eval(metadata_list$start_year - 1)]
    input_data[, year_id:= year]
    
    
    
    
    ################################################
    ## (3) Prepare base data
    ################################################
    
    ## Test and break convergence
    # input_data$ln_pop = .2
    # input_data$logit_pop15 = -.1
    
    data_params = make_data_param(input_data = input_data, 
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
                                  start_year = metadata_list$start_year, 
                                  end_fit = metadata_list$end_fit, 
                                  end_FC = metadata_list$end_FC)
    
    
    
        ################################################
        ## (4.1) Load previously run TMB model
        ################################################
        
        load(paste0('/share/resource_tracking/forecasting/', variable, '/', date, '_', comment, '/means/', model_info$model, '.Rdata'))
    
          
        
        ################################################
        ## (4.2) Prep draws of covariates and/or Y
        ################################################
        
        if(model_info$draws > 0) {
    
          ### Covariates ###
            
            N_draws = model_info$draws
      
            # if('edu' %in% model_info$xvar) {
            # ##### Education draws (melt and array it) [NOT USE BECAUSE BAD]
            #   edu_draws <- data.table(read_feather('/home/j/Project/IRH/Forecasting/FBD_edu_20171128_ref_draws.feather',
            #                                        columns = c('location_id','year_id','scenario', paste0('draw_', c(0:eval(model_info$draws - 1))) )))
            #   edu_draws <- edu_draws[year_id %in% c(eval(metadata_list$start_year + 1):metadata_list$end_FC) & scenario == 0][, year:= year_id]
            #   edu_draws <- edu_draws[, .SD, .SDcols = c('location_id', 'year', paste0('draw_', c(0:eval(model_info$draws - 1))) )]
            #   edu_draws <- melt(edu_draws, c('location_id', 'year'), variable.name = 'draw', value.name = 'edu')
            #   edu_draws[, draw:= as.numeric(gsub('draw_', '', draw))]
            # 
            #   ## Merge on iso3
            #   edu_level <- merge(edu_draws, unique(input_data[,.(iso3, location_id)]), 'location_id')
            # 
            #   ## Make diff
            #   edu_diff <- copy(edu_level)
            #   setkeyv(edu_diff, c('iso3', 'year'))
            #   
            #   ## Update draw index
            #   edu_diff[, draw:= draw+1]
            #   
            #   edu_diff[, edu:= edu - shift(edu), by = c('iso3')]
            #   edu_diff <- edu_diff[year>= metadata_list$start_year]
            # 
            #   edu_level <- edu_level[year>= metadata_list$start_year]
            # 
            # }
      
            if(!is.null(data_params$specifications$xvar) ) {
      
              #### Get the x_full (levels)
              X_level <- data.table(dcast(melt(data_params$data$x_full), Var2 + Var3 ~ Var1, value.var = 'value'))
              colnames(X_level)[1:2] <- c('iso3', 'year')
      
              #### Get the x_diff (diffs)
              X_diff <- data.table(dcast(melt(data_params$data$x_diff), Var2 + Var3 ~ Var1, value.var = 'value'))
              colnames(X_diff)[1:2] <- c('iso3', 'year')
      
      
              # if('edu' %in% model_info$xvar) {
              # 
              #   print("Prepping level data with education draws")
              #   ## Outer merge on edu draws
              #   ## First, drop edu if it exists
              #   X_level[, edu:= NULL]
              #   X_level <- merge(X_level, edu_level, c('iso3', 'year'), all.y=T)
              #   X_level[, location_id:= NULL]
              # 
              # 
              #   print("Prepping diff data with education draws")
              # 
              #   ## Outer merge on edu draws
              #   ## First, drop edu if it exists
              #   X_diff[, edu:= NULL]
              #   X_diff <- merge(X_diff, edu_diff, c('iso3', 'year'), all.y=T)
              #   X_diff[, location_id:= NULL]
              # }
      
            } else {
              X_diff = NULL
              X_level = NULL
            }
    
          ### Dependent Variable Draws ###
            
            # ## Sample N_draws 
            sample_draws <- sample(c(0:999), size = model_info$draws)

            ## Get draws of poverty p190
            pov_draws <- data.table(read_feather('/home/j/Project/IRH/POVERTY/DATA/New_Beemus/poverty_complete_gpr_output_m44536_p190_20180810.feather',
                                    columns = c('iso3', 'year_id', paste0('draw_', sample_draws))))

            ## Clean up draw names
            colnames(pov_draws) <- c('iso3', 'year', paste0('draw_', c(1:model_info$draws)))

            ## Keep specific locs
            pov_draws <- pov_draws[iso3 %in% unique(input_data$iso3) ]

            ## Melt long on draws
            pov_draws <- melt(pov_draws, c('iso3', 'year'), variable.name = 'draw', value.name = 'pov')
            pov_draws[, yvar:= pov]

            ## Create first differences (and logit yvar of course)
            pov_draws[, pov:=NULL]
            pov_draws[, yvar:= logit_trans(yvar)]
            pov_draws[, ydiff:= yvar - shift(yvar), by = c('iso3', 'draw')]

            ## Fix draws and make pretty
            pov_draws[, draw:= as.numeric(gsub('draw_', '', draw))]

            ## Prep final datasets
            Y_diff_lev <- pov_draws[year >= metadata_list$start_year]
            
            
        }
    
      
    
        
      
        ################################################
        ## (4.3) Create forecasts draws!
        ################################################
        
        ### Simulate parameter draws
        param_draws <- rmvnorm_prec(mu = output_TMB$mean_params,
                                    prec = output_TMB$FC_model$jointPrecision,
                                    n.sims = N_draws)


        ### Create predictions

        # registerDoParallel(cores=2)
        
        system.time(draws <- rbindlist(foreach(d=1:N_draws) %do% {

          if(d %% 10 == 0) print(paste0('Draw = ', d))
          
          ### Make a dataset for estimation data with the draw
          est_data <- prep_data_for_forecast(tmb_obj = output_TMB$obj,
                                             tmb_data_param = data_params,
                                             tmb_params = param_draws[d,],
                                             draw_Xdata =  prepping_X_for_draw(X_diff, X_level,
                                                                               tmb_data_param = data_params, d = d),
                                             draw_Ydata = Y_diff_lev[draw == d]
                                             # draw_Ydata = NULL
                                             )

          ### Forecast with RW errors
          draw_forecast <- make_forecast(mean_est_data_obj = est_data, 
                                         tmb_output_object = output_TMB,
                                         tmb_data_param = data_params,
                                         add_residual = T, 
                                         var_metric='mad', 
                                         cap_variance=T,
                                         var_threshold = if(data_params$specifications$fd == 1) {
                                           rmse_distro$diff_mad_cap
                                         }  else {
                                           rmse_distro$level_mad_cap
                                         } )[,draw:= d]

          return(draw_forecast)
        }, use.names = T))



        ggplot(draws[iso3=="ARE"]) +
          # geom_line(aes(x = year, y = ydiff, group = draw, color = draw)) +
          geom_line(aes(x = year, y = yvar, group = draw, color = draw)) +
          theme(legend.position = 'none')

        # stopImplicitCluster()


        
      
      ################################################
      ## (5) Output draws
      ################################################         
          

      ## Cast wide on draw, tag model number and save out Rdata
      out_draws <- prep_draw_output(draws, model = model_info$model)
          
        
      ## Save the draws
      # save(list = c("out_draws"),
      #      file = paste0('/share/resource_tracking/forecasting/', variable, '/', date, '_', comment, '/draws/', model_info$model, '.Rdata'))
      write_feather(out_draws,
           paste0('/share/resource_tracking/forecasting/', variable, '/', date, '_', comment, '/draws/', model_info$model, '.feather'))
      
        
      q('no')
          
        
    
    
    
    
    
    
    