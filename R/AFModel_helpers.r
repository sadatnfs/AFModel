    
    ###################################################
    ### Author:  Nafis Sadat
    ### Purpose: Helper functions to version and run forecasting stuff
    ###################################################
    
    suppressMessages(pacman::p_load(MASS, RMySQL, data.table, parallel, feather, argparse, foreach, doParallel, compiler, ggplot2, arm))

    ###################################################
    ## Job stuff
    ###################################################
    
    
    ### a job holding function to keep sleeping until certain jobs are done (from Patty Liu's cluster tools)
    job_hold <- function(job_name, sleeper = 10) {
      
      ## Give it a sec to launch
      # Sys.sleep(5)
      
      ## Start timer
      start.time <- proc.time()
      
      ## Wait for job to finish
      flag <-  0
      while (flag == 0) {
        ## Check if job is done
        if (system(paste0("qstat -r | grep ", job_name, "|wc -l"), intern=T) == 0) {
          ## If so, set flag to 1
          flag <- 1
        } else {
          Sys.sleep(sleeper)
        }
      }
      
      ## End Timer
      job.runtime <- proc.time() - start.time
      job.runtime <- job.runtime[3]
      
      ## Give it another sec
      # Sys.sleep(10)
      
      ## Complete
      print(paste0("Job ", job_name, " has completed. Time elapsed: ", job.runtime))
    }
    
    
    
    
    ###################################################
    ## DB
    ###################################################
    
    
    ## Function to query and get entity info
    get_entity_info <- function(variable = NA) {
      
      dbconn = dbConnect(MySQL(), user='readonly', password='justlooking', 
                         dbname='forecasting', host='dex-modelingdb-d01.ihme.washington.edu')
      
      if(is.na(variable)) {
        out = dbSendQuery(dbconn, paste0('select * from entity_metadata'))
      } else {
        out = dbSendQuery(dbconn, paste0('select * from entity_metadata where entity_name = "', variable, '"'))
      }
      
      out_2 = data.table(fetch(out, n=-1))
      on.exit(dbDisconnect(dbconn))
      
      return(out_2)
      
    }
    
    ## Function to query and get model info
    get_model_info <- function(entity_id = NA) {
      
      dbconn = dbConnect(MySQL(), user='readonly', password='justlooking', 
                         dbname='forecasting', host='dex-modelingdb-d01.ihme.washington.edu')
      
      if(is.na(entity_id)) {
        out = dbSendQuery(dbconn, paste0('select * from model_versions '))
      } else {
        out = dbSendQuery(dbconn, paste0('select * from model_versions where entity = "', entity_id, '"'))
      }
      out_2 = data.table(fetch(out, n=-1))
      on.exit(dbDisconnect(dbconn))
      
      return(out_2)
      
    }
    
    
    
    ## Function to query and get output info for given entity and model
    get_oots <- function(entity_id, model_id) {
      
      dbconn = dbConnect(MySQL(), user='readonly', password='justlooking', 
                         dbname='forecasting', host='dex-modelingdb-d01.ihme.washington.edu')
      
      out = dbSendQuery(dbconn, paste0('select * from outputs where entity = "', entity_id, '"',
                                       ' and model_id = ', model_id))
      out_2 = data.table(fetch(out, n=-1))
      on.exit(dbDisconnect(dbconn))
      
      return(out_2)
      
    }
    
    ## Locs
    get_lox <- function() {
      dbconn = dbConnect(MySQL(), user='readonly', password='justlooking', 
                         dbname='forecasting', host='dex-modelingdb-d01.ihme.washington.edu')
      
      out = dbSendQuery(dbconn, paste0('select * from location_metadata'))
      out_2 = data.table(fetch(out, n=-1))
      on.exit(dbDisconnect(dbconn))
      
      return(out_2)
      
    }
    
    
    
    ## Prep output data with the entity and model info
    prep_for_db_uploading <- function(root_fold, metadata_list, entity_info, model_num) {
      #' @param root_fold the model folder info which will pull in 
      #' a data.table with the following columns: 
      #' location_id, year_id, mean, lower, upper, scenario
      #' usually comes from Scenario output.
      #' If no scenario columns exist, make scenario == 0 column
      #' @param metadata_list
      #' @param entity_info
      #' @param model_num
      #' Outputs include a list with:
      #' \itemize{
      #'  \item \code{model_info} A dataframe to be uploaded to model_versions
      #'  \item \code{outputs} A dataframe to be uploaded to outputs 
      #' }
      
      
      ## Get data from root_fold:
        ## First check whether scenario data exists
        tryCatch(
          {
            try(data <- fread(paste0(root_fold, '/summary_files/scenario_stats.csv')))
            data <- fread(paste0(root_fold, '/summary_files/correlated_stats.csv'))
          },
          error = function(e) {
            stop("Neither correlated nor scenario stats found")
          }, 
          warning = function(w) {
            stop("No scenarios")
          }
        )
      
      ## Check whether iso3, year, mean, upper and lower are in our dataset
      if(!any(colnames(data) %in% c('iso3', 'year', 'mean', 'upper' , 'lower'))) {
        stop("Not all of the following columns are in the dataframe: iso3, year, mean, upper, lower")
      }
      
      
      ## Add scenario column if scenario not in data
      if(!'scenario' %in% colnames(data)) {
        data[, scenario:= 0]
      }
      
      
      ## If location_id is not in data, we merge it on
      if(!'location_id' %in% colnames(data)) {
        
        ## Get location_set
        dbconn = dbConnect(MySQL(), user='readonly', password='justlooking', 
                           dbname='forecasting', host='dex-modelingdb-d01.ihme.washington.edu')
        
        lox <- get_lox()[level == 3, .(iso3, location_id)]
        on.exit(dbDisconnect(dbconn))
        
        data <- merge(data, lox, 'iso3', all.x=T) 
      }
      
      ## Prep data in the format we desire
      data_oot <- data[,.(entity = entity_info$entity, 
                          model_id = model_num, location_id, year_id = year, mean, upper, lower, scenario)]
      
      
      ## Get location of draws (depending on whether we ran scenarios or not)
      if(file.exists(paste0(root_fold, '/summary_files/scenario_draws.feather'))) {
        draws <- paste0(root_fold, '/summary_files/scenario_draws.feather')
      } else if(file.exists(paste0(root_fold, '/summary_files/correlated_draws.feather'))) {
        draws <- paste0(root_fold, '/summary_files/correlated_draws.feather')
      } else {
        draws <- 'NO DRAW FILES FOUND'
      }
      
      ## Create a data.table with the model and entity info
      model_info <- data.table(model_id = model_num, 
                               date = metadata_list$date,
                               entity = entity_info$entity,
                               comment = metadata_list$comment,
                               comment_long = metadata_list$comment_long,
                               draws = draws)
      
      list_out <- list()
      list_out[['outputs']] <- data_oot
      list_out[['model_info']] <- model_info
      return(list_out)
      
    }
    
    
    ### Uploader
    
    upload_data_to_db <- function(upload_prepz) {
      #' @param upload_prepz The list output from prep_for_db_uploading
      
      ## Open connection
      dbconn = dbConnect(MySQL(), user='rt_uploader', password='rt_uploader100', 
                         dbname='forecasting', host='dex-modelingdb-d01.ihme.washington.edu')
      
      
      ## Make sure that we are good on the model numbering
      model_info <- get_model_info(entity_info$entity)
      if(nrow(model_info) == 0) {
        model_num = 1
      } else {
        model_num = max(model_info$model_id)+1
      }
      
      if(model_num > upload_prepz$model_info$model_id) {
        print(paste0("Model ID ", model_num, " have already been uploaded! Increment by 1"))
        upload_prepz$model_info$model_id = upload_prepz$model_info$model_id + 1
        upload_prepz$outputs$model_id = upload_prepz$outputs$model_id + 1
      }
      
      ## Upload model_info first
      print("Uploading model info")
      dbWriteTable(dbconn, 
                   value = upload_prepz$model_info, 
                   row.names=F,
                   name = "model_versions", append = TRUE ) 
      
      ## Upload data
      print("Uploading data to outputs table")
      
      ## Save out tempfile of data
      tmpfile <- fwrite(upload_prepz$outputs[,.(entity, model_id, location_id, year_id, mean, upper, lower, scenario)], 
                        paste0('/share/resource_tracking/temp_dump/', 
                               upload_prepz$model_info$comment, '_output_rt1.csv'))
      
      ## Build query
      query  <- paste0("load data infile ",
                       " '/share/resource_tracking/temp_dump/", upload_prepz$model_info$comment, "_output_rt1.csv'",
                       " into table forecasting.outputs ",
                       "fields terminated by ',' ",
                       "lines terminated by '\n' ignore 1 lines;")
      
      on.exit(file.remove(paste0('/share/resource_tracking/temp_dump/', 
                                 upload_prepz$model_info$comment, '_output_rt1.csv')))
      
      
      # Submit the update query and dc
      dbGetQuery(dbconn, query)
      on.exit(dbDisconnect(dbconn))
      
      print("Upload completed")
      return(0)
      
    }
    
    
    ###################################################
    ## File structuring
    ###################################################
    
    
    ## Function to prep for a run (makes folder structure)
    prep_model <- function(variable, comment, date = gsub('-','', Sys.Date()), erase=F) {
      
      folder_path <- '/share/resource_tracking/forecasting/'
      
      ## Make root folder
      dir.create(paste0(folder_path, variable, '/', date, '_', comment))
      
      ## Make all the necessary sub folders
      dir.create(paste0(folder_path, variable, '/', date, '_', comment, '/passed'))
      dir.create(paste0(folder_path, variable, '/', date, '_', comment, '/failed'))
      dir.create(paste0(folder_path, variable, '/', date, '_', comment, '/means'))
      dir.create(paste0(folder_path, variable, '/', date, '_', comment, '/draws'))
      dir.create(paste0(folder_path, variable, '/', date, '_', comment, '/summary_files'))
      dir.create(paste0(folder_path, variable, '/', date, '_', comment, '/summary_files_scenarios'))
      dir.create(paste0(folder_path, variable, '/', date, '_', comment, '/draws_scenarios'))
      dir.create(paste0(folder_path, variable, '/', date, '_', comment, '/draws_scenarios/worse'))
      dir.create(paste0(folder_path, variable, '/', date, '_', comment, '/draws_scenarios/better'))
      
      if(erase) {
        
        CONFIRM <- readline("Are you sure you want to erase all of the innards? \n")
        
        if(substr(CONFIRM, 1, 1) == "y" | 
           substr(CONFIRM, 1, 1) == "Y" | 
           substr(CONFIRM, 1, 1) == "yes" | 
           substr(CONFIRM, 1, 1) == "Yes") {
          
          ## If the folders already exist, clean up the innards
          system(paste0('rm ', paste0(folder_path, variable, '/', date, '_', comment, '/passed/*')))
          system(paste0('rm ', paste0(folder_path, variable, '/', date, '_', comment, '/failed/*')))
          system(paste0('rm ', paste0(folder_path, variable, '/', date, '_', comment, '/means/*')))
          system(paste0('rm ', paste0(folder_path, variable, '/', date, '_', comment, '/draws/*')))
          system(paste0('rm ', paste0(folder_path, variable, '/', date, '_', comment, '/summary_files/*')))
          system(paste0('rm ', paste0(folder_path, variable, '/', date, '_', comment, '/summary_files/*')))
          system(paste0('rm ', paste0(folder_path, variable, '/', date, '_', comment, '/summary_files_scenarios/*')))
          system(paste0('rm ', paste0(folder_path, variable, '/', date, '_', comment, '/draws_scenarios/worse/*')))
          system(paste0('rm ', paste0(folder_path, variable, '/', date, '_', comment, '/draws_scenarios/worse/*')))
          
          
        } else {
          stop("Stopping now.")
        }
      
      }
      
      
      ## Return root folder of run 
      return(paste0(folder_path, variable, '/', date, '_', comment))
      
    }
    
    ## A simple function to record a fail/pass and stop running code
    record_model_and_stop <- function(model, variable, comment, date = gsub('-','', Sys.Date()), type) {
      
      if(!(type %in% c('passed', 'failed'))) {
        stop("Really?")
      }
      
      ## Just echo a blank file in failed folder
      folder_path <- '/share/resource_tracking/forecasting/'
      paste0(folder_path, variable, '/', date, '_', comment, '/', type)
      system(paste0("echo ", model, " >> ",   folder_path, variable, '/', date, '_', comment, '/', type, '/', model, '.txt'))
      
      if(type == 'passed') {
        print(paste0("Model run sucessfull! Saved out empty file in : ", folder_path, variable, '/', date, '_', comment, '/', type, '/', model, '.txt' ) )
        q('no')
      } else if (type == 'failed') {
        stop(paste0("Model run failed. Saved out empty file in : ", folder_path, variable, '/', date, '_', comment, '/', type, '/', model, '.txt'))
      }
      
    }
    
    
    
    ###################################################
    ## Ensemble Architecture
    ###################################################
    
    
    
    ## A small function to duplicate and return a data frame with switches
    duplicate_switch <- function(data, param_name, param_domain) {
      if(length(param_domain) > 0 & !is.null(param_domain)) {
        tmpcol <- colnames(data)
        data <- data[rep(1:.N, length(param_domain))][, (param_name):= param_domain, by = tmpcol]
      } else {
        data[, (param_name):=0]
      }
      
      return(data)
    }
    
    
    ### Create grid of covariates and specification
    create_ensemble_grid <- function(yvar, 
                                     xvar, 
                                     re_coef = NULL,
                                     ar = c(0), ar_mod = c(2), 
                                     ma = c(0), ma_mod = c(2), 
                                     weight_decays = c(0),
                                     global_int = 1, 
                                     country_int = 1, 
                                     country_int_dist = 1,
                                     fdiff = c(1), conv = c(0,1)) {
      
      
      ### Some checks to break
        
        ## Break if ar_mod is not null where as AR is
        if(!is.null(ar_mod) & is.null(ar)) {
          stop("'ar' can't be null while 'ar_mod' is specified")
        }
      
        ## Same with MA
        if(!is.null(ma_mod) & is.null(ma)) {
          stop("'ma' can't be null while 'ma_mod' is specified")
        }
        
        ## Convergence term can't be true if fdiff does not have 1
        if(!(1 %in% fdiff) & (1 %in% conv)) {
          stop("'conv' can't be true if fdiff models aren't in")
        } 

        
        ## Break if the random coefficient is not also included as a fixed effect
        if( any(paste0('FE_', re_coef) %in% xvar == F) & !is.null(re_coef) ) {
          stop("Random coefficients MUST have a counterpart fixed effect")
        }
      
        ## Random coefs?
        if(!is.null(re_coef)) {
          
          ## Create grid of xvars
          tmp <- (rep(list(0:2), eval( length(xvar)   ) ))
          regMat <- data.table(do.call(expand.grid, tmp))
          colnames(regMat) <- c(xvar)
          
          ## Drop where the non REs are equal to 2
          for(bads in setdiff(xvar, paste0('FE_', re_coef))) {
            regMat <- regMat[ get(bads) != 2]
          }
          
          
        } else {
          
          ## Create grid of xvars
          tmp <- (rep(list(0:1), eval( length(xvar)  ) ))
          regMat <- data.table(do.call(expand.grid, tmp))
          colnames(regMat) <- c(xvar)
          
        }
      

      
        ## Cycle over AR terms?
        regMat <- duplicate_switch(data = regMat, param_name = 'ar', param_domain = c(0:ar))
        
          ## Which types of AR specification are we using?
          regMat <- duplicate_switch(data = regMat, param_name = 'ar_mod', param_domain = c(0, ar_mod))
        
          ## Drop if AR == 0 and ar_mod != 0, or if AR != 0 and ar_mod == 0
          regMat <- regMat[!(ar == 0 & ar_mod >0)]
          regMat <- regMat[!(ar != 0 & ar_mod ==0)]
          
        ## Cycle over MA terms?
        regMat <- duplicate_switch(data = regMat, param_name = 'ma', param_domain = c(0:ma))
        
          ## Which types of AR specification are we using?
          regMat <- duplicate_switch(data = regMat, param_name = 'ma_mod', param_domain = c(0, ma_mod))
        
          ## Drop if MA == 0 and ma_mod != 0, or if MA != 0 and ma_mod == 0
          regMat <- regMat[!(ma == 0 & ma_mod >0)]
          regMat <- regMat[!(ma != 0 & ma_mod ==0)]
          
        ## Cycle over time weights?
        regMat <- duplicate_switch(data = regMat, param_name = 'weight_decay', param_domain = weight_decays)
        
        ## Cycle over global intercept?
        regMat <- duplicate_switch(data = regMat, param_name = 'global_int', param_domain = global_int)
        
        ## Cycle over country intercept?
        regMat <- duplicate_switch(data = regMat, param_name = 'country_int', param_domain = country_int)
        
          ## Random or fixed country intercept?
          if(country_int_dist) {
            regMat[, country_int_dist:= 1]
          } else {
            regMat[, country_int_dist:= 0]
          }
        
        ## Cycle over fixed difference and levels?
        regMat <- duplicate_switch(data = regMat, param_name = 'fdiff', param_domain = fdiff)
        
        ## Cycle over convergence term, but ONLY set conv=T if fdiff==1
        regMat <- duplicate_switch(data = regMat, param_name = 'conv', param_domain = conv)
        regMat <- regMat[!(fdiff == 0 & conv == 1)]
        
        
        ## Finally, create yvar grid 
        regMat <- duplicate_switch(data = regMat, param_name = 'yvar', param_domain = yvar)
        
        ## Tag model numbers
        regMat[, id:= .I]
        
        return(regMat)
      
    }
    
    
    
    ######################################################################
    ### Ensemble Collectors
    ######################################################################
    
    ## Checking whether all mean models have finished running
    
    mean_run_check <- function(root_fold, mean_array_grid, return_fail = F) {
    
      ## Get a list of models that passed and failed
      list_of_models_passed <- data.table(model_number = as.numeric(strsplit(list.files(paste0(root_fold, "/passed")), ".txt")))
      list_of_models_failed <- data.table(model_number = as.numeric(strsplit(list.files(paste0(root_fold, "/failed")), ".txt")))
      
      print(paste0(nrow(list_of_models_passed)  + nrow(list_of_models_failed), 
                   ' models out of ', nrow(mean_array_grid)  , 
                   ' have finished running. Remaining: ', 
                   nrow(mean_array_grid) - (nrow(list_of_models_passed)  + nrow(list_of_models_failed)) ))
      print(paste0('with ', nrow(list_of_models_passed) , ' passed and ', nrow(list_of_models_failed), ' failed'))
      
      ## Make sure that we have fail and passes for ALL the models we're supposed to run
      if(eval(nrow(list_of_models_passed) + nrow(list_of_models_failed)) != nrow(mean_array_grid) ) {
        # print("All of the models did not finish running. Check the following models!")
        
        print("Models missing from run are:")
        merg <- merge(rbind(list_of_models_passed, list_of_models_failed)[order(model_number), done:=T], 
                      data.table(model_number = mean_array_grid$id),
                      "model_number", all.y=T)[is.na(done)]
        print(merg)       
        
        if(return_fail) {
          return(merg[, model_number])
        } else {
            stop("DIE DIE DIE DIE DIE. Seriously.")
        }
      } else {
        print("All models have finished running")
        
      }
      
      return(list_of_models_passed[, model_number])
    
    }
    
    ## Checking whether all draw models have finished running
    
    draw_run_check <- function(root_fold, array_grid) {
      
      ## Get list of draws already there
      list_of_draws <- data.table(model_number = as.numeric(strsplit(list.files(paste0(root_fold, "/draws")), ".feather")))[, file:= model_number]
      merged_mods <- merge(array_grid, list_of_draws, "model_number", all.x=T) 
      
      
      print(paste0(nrow(list_of_draws), ' models out of ', nrow(merged_mods)  , ' have finished running'))

      if(length(merged_mods[is.na(file), model_number]) > 0) {
        print("The following models are missing:")
        print(merged_mods[is.na(file)][order(id)])
        stop("Not all the draws have been run. Stopping.")
      } 
    } 
    
    ## Bring in mean postfile outputs
    mean_postfiles_compile <- function(root_fold, passed_models, mc.cores) {
      
      registerDoParallel(cores = mc.cores)
      
      ## Load the passed models stuff on the number of rows in all models passed
      system.time(data_models_passed_list <- rbindlist( foreach(model = passed_models) %dopar% { 
        
        ## Load the output meanz data
        return(fread(paste0(root_fold, "/means/", model, '.csv')))
        
      }  , use.names = T, fill = T))
      
      on.exit(stopImplicitCluster())
      
      
      ## Exception for Chaos
      if(!('oos' %in% colnames(data_models_passed_list) )) {
        setkeyv(data_models_passed_list, c('model_number', 'iso3'))
        data_models_passed_list[, oos:= NA]
      } else {
        setkeyv(data_models_passed_list, c('model_number', 'iso3', 'oos'))
      }
      return(data_models_passed_list)
      
    }
    
    ## Checking whether all the Chaos collectors have finished
    chaos_collector_check <- function(root_fold, oos_years, chaos, scenario = F) {
    
      
      ## Get list of files
      if(!scenario) {
        list_of_chaos <- list.files(paste0(root_fold, "/summary_files"), pattern = ".rds")
      } else if (scenario) {
        list_of_chaos <- list.files(paste0(root_fold, "/summary_files_scenarios"), pattern = ".rds")
        oos_years = oos_years*2
      }
      
      ## Keep only Chaos rds
      list_of_chaos <- list_of_chaos[grep('chaos', list_of_chaos)]
      
      if(chaos) {
        
        if(length(list_of_chaos) == oos_years) {
          print("All Chaos collectors have finished running")
        } else {
          stop(paste0(eval(oos_years - length(list_of_chaos)), ' out of ', oos_years, ' have not finished.'))
        }  
      } else {
        
        if(length(list_of_chaos) == 1) {
          print("Draw collector have finished running")
        } else {
          stop('Nope')
        }
      }
      
      
      
    }
    
    
    ## Bring in mean forecast estimates
    ## Bring in mean postfile outputs
    mean_estimates_compile <- function(root_fold, passed_models, mc.cores) {
      
      registerDoParallel(cores = mc.cores)
      
      ## Load the passed models stuff on the number of rows in all models passed
      system.time(data_models_passed_list <- rbindlist( foreach(model = passed_models) %dopar% { 
        
        ## Load the output meanz data
        load(paste0(root_fold, "/means/", model, '.Rdata'))
        
        ## Only return mean_forecast
        return(mean_forecast[, model_number:= model])
        
      }  , use.names = T, fill = T))
      
      on.exit(stopImplicitCluster())
      return(data_models_passed_list)
    }
    
    
    ######################################################################
    ### Post Means stuff
    ######################################################################
    
    ## This function will take in a list where the name of the members are named of the FEs with
    ## the values being -1,0,1, where:
    ## -1 : negative coefficient
    ##  0 : no prior
    ##  1 : positive coefficient
    prior_coefs_mean <- function(data, coef_prior_list) {
      
      # data = copy(data_models_passed)
      # coef_prior_list = gdp_prior
      
      ## First, check that the names of the items in coef_prior_list are in colnames of data
      # if( length( intersect(names(coef_prior_list) , colnames(data)) )  != length(coef_prior_list) ) {
      #   stop("Column names of data and coef_prior_list aren't aligned")
      # }
      
      ## Apply the priors
      mod_num <- list()
      for(i in names(coef_prior_list)) {
        
        if(i %in% colnames(data)) {
        
          ## Check for which model numbers are defying our priors
          if(coef_prior_list[[i]] == -1)  {
            mod_num[[i]] <- unique(data[!is.na( get(i) ) & get(i) > 0 , model_number])
          } else if (coef_prior_list[[i]] == 1)  {
            mod_num[[i]] <- unique(data[!is.na( get(i) ) & get(i) < 0 , model_number])
          }
          
        }
      }
      ## Drop the unique model numbers from data
      data <- data[!(model_number %in% unique(unlist(mod_num))) ]
      
      data
    }
    
    
    DoubleMAD <- function(x, zero.mad.action="warn"){
      # The zero.mad.action determines the action in the event of an MAD of zero.
      # Possible values: "stop", "warn", "na" and "warn and na".
      x         <- x[!is.na(x)]
      m         <- median(x)
      abs.dev   <- abs(x - m)
      left.mad  <- median(abs.dev[x<=m])
      right.mad <- median(abs.dev[x>=m])
      if (left.mad == 0 || right.mad == 0){
        if (zero.mad.action == "stop") stop("MAD is 0")
        if (zero.mad.action %in% c("warn", "warn and na")) warning("MAD is 0")
        if (zero.mad.action %in% c(  "na", "warn and na")){
          if (left.mad  == 0) left.mad  <- NA
          if (right.mad == 0) right.mad <- NA
        }
      }
      return(c(left.mad, right.mad))
    }
    
    
    
    ## This function will distribute draws across sub-models by Chaos percent and RMSE, if Chaos. It's dope
    
    chaos_draw_distro <- function(data,  chaos_pc = 10, iso_portion = 1, metadata = ensemble_metadata,
                                  region_portion = 0, super_region_portion = 0, global_portion = 0, 
                                  N_draws = 1000) {
      
      # data = copy(data_models_passed)
      # chaos_pc <- 10
      # iso_portion = 2/3; region_portion = 1/3; super_region_portion = 0; global_portion = 0
      
      ## First, stop if the portion sums are greater than 1
      if(region_portion+iso_portion+super_region_portion+global_portion > 1) {
        stop("RMSE fractions are > 1.")
      }
      
      
      
      print("Computing the weighted RMSE by country-region-SR-global portions")
      data[, rmse_WREG:= (region_portion*rmse_REG) + (iso_portion*rmse_iso) + (super_region_portion*rmse_SRREG) + (global_portion* rmse_global)]
      
      print("Rank each submodel by iso3/region/sr specific by iso3 and OOS year")
      data <- data[!is.na(rmse_WREG), iso_rank := frank(rmse_WREG, ties.method = "first"), by = c("iso3", "oos")]
      data <- data[order(iso3, oos, iso_rank)]
      
      ## Drop if rmse_iso is missing
      data <- data[!is.na(rmse_WREG)]
      
    
      ## What's the highest rank for each? We want the chaos% (and respective draws for each) 
      
      #### NOTE: We use the length as opposed to the max, because that will allow us to filter by criteria
      ##### and therefore, we can get a higher number of draws to use by filtering out 
      
      ## We also wanna drop if RMSE is NA (because those models weren't picked up through virtue of missing vals)
      
      ## Find the cutoff number of models
      ## What's the max number of models per country we got
      data[, max_rank := max(iso_rank, na.rm=T), c( 'iso3')]
      
      ## What's the Chaos% cutoff for each country?
      print(paste0("Keep only the top ", chaos_pc, "% models")) 
      data[, draw_cutoff:= ceiling(chaos_pc/100 *max_rank)]
      
      print(paste0("Countries where draw_cutoff is <=", chaos_pc, ", we bump it up to the minimum of ", chaos_pc,  " or the max_rank"))
      data[draw_cutoff<=chaos_pc, draw_cutoff:= min(chaos_pc, max_rank, na.rm=T),  by = c('iso3')]
      
      ## Cut off the models passed beyond the cutoff value
      data <- data[iso_rank <= draw_cutoff]
      
      
      ### What's the number of draws per country-model ?
      data[, draw_num:= ceiling(N_draws / draw_cutoff)]
      
      #### ORDER FEs in the order of ensemble grid !!!! ####
      if(!is.null(metadata$xvar)[1] ) {
        order_of_FEs <- metadata$xvar
        
        ## Get no FE col names
        data_cols_no_FE <- setdiff(colnames(data), order_of_FEs)
        
        ## Get FE cols in data and drop the ones where the FE did not pass whatsoever
        FE_data_cols <- colnames(data)[grep('^FE_', colnames(data))]
        
        ## Drop from order_of_FEs (use intersect of large with small)
        order_of_FEs <- intersect(order_of_FEs, FE_data_cols)
        
        ## Reorder data
        setcolorder(data, neworder = c(order_of_FEs, data_cols_no_FE))
        
      }
      
      
      print("Making draws:")
      
      ## Let's get the unique models (with the max number of draws) remaining we will create draws over
      models_remaining <- unique(data[, .(draw_num, model_number, yvar)])
      models_remaining[, drawz:= max(draw_num), 'model_number']
      models_remaining <- models_remaining[drawz == draw_num]
      models_remaining[, draw_num:=NULL]
      
      ## Create task ID (1:n) and number of draws
      array_grid <- copy(models_remaining)
      array_grid <- array_grid[, id:=.I]      
      
      print("Compute the MAD cap from ALL the MADs if we want to cap our residual errors")
      # data[, diff_mad_cap:= (qnorm(0.90)/.6745)*mad(diff_mad) + median(diff_mad)]
      data[, diff_mad_cap:= DoubleMAD(diff_mad_cap)[1] ]
      data[ diff_mad > diff_mad_cap, diff_mad:= diff_mad_cap]
      
      # data[, level_mad_cap:= (qnorm(0.90)/.6745)*mad(level_mad) + median(level_mad)]
      data[, level_mad_cap:= DoubleMAD(level_mad_cap)[1] ]
      data[ level_mad > level_mad_cap, level_mad:= level_mad_cap]
      
      
      ## Extract the MAD cap scalars
      diff_mad_cap <- data[1, diff_mad_cap]
      level_mad_cap <- data[1, level_mad_cap]  
      
      return(list(array_grid = array_grid, 
                  data = data, 
                  level_mad_cap = level_mad_cap, diff_mad_cap = diff_mad_cap,
                  N_draws = N_draws))
    }
    
    
    
    ### This function will do the same as above, but uses the Double Mad Cap
    
    chaos_draw_distro_DMC <- function(data, chaos_pc = 10, iso_portion = 1, metadata = ensemble_metadata,
                                  region_portion = 0, super_region_portion = 0, global_portion = 0, 
                                  N_draws = 1000) {
      
      # data = copy(data_models_passed)
      # chaos_pc <- 10
      # iso_portion = 2/3; region_portion = 1/3; super_region_portion = 0; global_portion = 0
      
      ## First, stop if the portion sums are greater than 1
      if(region_portion+iso_portion+super_region_portion+global_portion > 1) {
        stop("RMSE fractions are > 1.")
      }
      
      print("Computing the weighted RMSE by country-region-SR-global portions")
      data[, rmse_WREG:= (region_portion*rmse_REG) + (iso_portion*rmse_iso) + (super_region_portion*rmse_SRREG) + (global_portion* rmse_global)]
      
      print("Rank each submodel by iso3/region/sr specific by iso3 and OOS year")
      data <- data[!is.na(rmse_WREG), iso_rank := frank(rmse_WREG, ties.method = "first"), by = c("iso3", "oos")]
      data <- data[order(iso3, oos, iso_rank)]
      
      ## Drop if rmse_iso is missing
      data <- data[!is.na(rmse_WREG)]
      
      ## What's the highest rank for each? We want the chaos% (and respective draws for each) 
      
      #### NOTE: We use the length as opposed to the max, because that will allow us to filter by criteria
      ##### and therefore, we can get a higher number of draws to use by filtering out 
      
      ## We also wanna drop if RMSE is NA (because those models weren't picked up through virtue of missing vals)
      
      ## Find the cutoff number of models
      ## What's the max number of models per country we got
      data[, max_rank := max(iso_rank, na.rm=T), c( 'iso3')]
      
      ## What's the Chaos% cutoff for each country?
      
      print(paste0("Keep only the top ", chaos_pc, "% models")) 
      data[, draw_cutoff:= ceiling(chaos_pc/100 *max_rank)]
      
      print(paste0("Countries where draw_cutoff is <=", chaos_pc, ", we bump it up to the minimum of ", chaos_pc,  "or the max_rank"))
      data[draw_cutoff<=chaos_pc, draw_cutoff:= min(chaos_pc, max_rank, na.rm=T),  by = c('iso3')]      
      
      ## Cut off the models passed beyond the cutoff value
      data <- data[iso_rank <= draw_cutoff]
      
      
      ### What's the number of draws per country-model ?
      data[, draw_num:= ceiling(N_draws / draw_cutoff)]
      
      #### ORDER FEs in the order of ensemble grid !!!! ####
      if(!is.null(metadata$xvar)[1] ) {
        order_of_FEs <- metadata$xvar
        
        ## Get no FE col names
        data_cols_no_FE <- setdiff(colnames(data), order_of_FEs)
        
        ## Get FE cols in data and drop the ones where the FE did not pass whatsoever
        FE_data_cols <- colnames(data)[grep('^FE_', colnames(data))]
        
        ## Drop from order_of_FEs (use intersect of large with small)
        order_of_FEs <- intersect(order_of_FEs, FE_data_cols)
        
        ## Reorder data
        setcolorder(data, neworder = c(order_of_FEs, data_cols_no_FE))
         
      }
      
      
      
      print("Making draws:")
      
      ## Let's get the unique models (with the max number of draws) remaining we will create draws over
      models_remaining <- unique(data[, .(draw_num, model_number, yvar)])
      models_remaining[, drawz:= max(draw_num), 'model_number']
      models_remaining <- models_remaining[drawz == draw_num]
      models_remaining[, draw_num:=NULL]
      
      ## Create task ID (1:n) and number of draws
      array_grid <- copy(models_remaining)
      array_grid <- array_grid[, id:=.I]      
      
      print("Compute the MAD cap from ALL the MADs if we want to cap our residual errors")
      data[, diff_mad_cap:= (qnorm(0.90)/.6745)*mad(diff_mad) + median(diff_mad)]
      data[ diff_mad > diff_mad_cap, diff_mad:= diff_mad_cap]
      
      
      data[, level_mad_cap:= (qnorm(0.90)/.6745)*mad(level_mad) + median(level_mad)]
      data[ level_mad > level_mad_cap, level_mad:= level_mad_cap]
      
      
      ## Extract the MAD cap scalars
      diff_mad_cap <- data[1, diff_mad_cap]
      level_mad_cap <- data[1, level_mad_cap]  
      
      return(list(array_grid = array_grid, 
                  data = data, 
                  level_mad_cap = level_mad_cap, diff_mad_cap = diff_mad_cap,
                  N_draws = N_draws))
    }
    
    
    
    
    
    
    
    
    
    
    ### Cumsumming Chaos (or not) POST DRAWS
    final_cum_sum <- function(root_fold, chaos, scenario = F, oos_years, N_draws, rev_trans, pop_data = NULL, pop_action = NULL, hack_drop_NAs = F) {
      
      if(chaos) {
        if(!scenario) {
          print("Bring all the Chaos compiles into memory")
          chaos_compiles <- rbindlist(lapply(c(1:oos_years), function(f) readRDS(paste0(root_fold, "/summary_files/chaos_oos_", f, ".rds") )))  
          
          
        } else {
          print("Bring all the Chaos compiles into memory")
          chaos_compiles_worse <- rbindlist(lapply(c(1:oos_years), function(f) readRDS(paste0(root_fold, "/summary_files_scenarios/worse_chaos_oos_", f, ".rds") )))[, scenario:= -1]
          chaos_compiles_better <- rbindlist(lapply(c(1:oos_years), function(f) readRDS(paste0(root_fold, "/summary_files_scenarios/better_chaos_oos_", f, ".rds") )))[, scenario:= 1]    
          chaos_compiles <- rbindlist(list(chaos_compiles_better, chaos_compiles_worse))
         }
        
      } else {
        if(!scenario) {
          print("Bring all the draws into memory")
          chaos_compiles <- readRDS(paste0(root_fold, "/summary_files/chaos_oos.rds") ) 
        } else {
          print("Bring all the draws into memory")
          chaos_compiles_worse <- readRDS(paste0(root_fold, "/summary_files_scenarios/worse_chaos_oos.rds") )[, scenario:= -1]
          chaos_compiles_better <- readRDS(paste0(root_fold, "/summary_files_scenarios/better_chaos_oos.rds") )[, scenario:= 1]
          chaos_compiles <- rbindlist(list(chaos_compiles_better, chaos_compiles_worse))
        }
      
      }
      
      if(!scenario) {
        ## Set iso3 and year index
        setkeyv(chaos_compiles, c('iso3', 'year'))
        
        print("Cumsum all draws for each country, and reverse transform")
        chaos_compiles[, paste0('draw_', c(1:N_draws)):= lapply(paste0('draw_', c(1:N_draws)), function(x) cumsum(get(paste0(x))) ),  by = c('iso3' )]
        
        
        ## Reserve transform; rev_trans = 'exp'
        chaos_compiles[, paste0('draw_', c(1:N_draws)):= lapply(paste0('draw_', c(1:N_draws)), function(x) get(rev_trans)(get(paste0(x))) ) ]
        
        
        if(!is.null(pop_data)) {
          ## Merge in pops
          chaos_compiles <- merge(chaos_compiles, pop_data, c('iso3', 'year'))
          
          if(pop_action == 'div') {
            chaos_compiles[, paste0('draw_', c(1:N_draws)):= lapply(paste0('draw_', c(1:N_draws)), function(x) get(paste0(x)) / total_pop ) ]
          } else if(pop_action == 'multiply') {
            chaos_compiles[, paste0('draw_', c(1:N_draws)):= lapply(paste0('draw_', c(1:N_draws)), function(x) get(paste0(x)) * total_pop ) ]
          }
          
          chaos_compiles[, total_pop:=NULL]
        }
        
        ## Create stats
        if(hack_drop_NAs) {
          chaos_compiles <- chaos_compiles[!is.na(draw_1)]
          chaos_stats <- stat_maker(data = chaos_compiles,  melt = T, merge = F)  
        } else {
          chaos_stats <- stat_maker(data = chaos_compiles,  melt = T, merge = F)  
        }
        
      } else {
        
        ## Set iso3 and year index
        setkeyv(chaos_compiles, c('iso3', 'scenario', 'year'))
        
        print("Cumsum all draws for each country, and reverse transform")
        chaos_compiles[, paste0('draw_', c(1:N_draws)):= lapply(paste0('draw_', c(1:N_draws)), function(x) cumsum(get(paste0(x))) ),  by = c('iso3', 'scenario' )]
        
        
        ## Reserve transform; rev_trans = 'exp'
        chaos_compiles[, paste0('draw_', c(1:N_draws)):= lapply(paste0('draw_', c(1:N_draws)), function(x) get(rev_trans)(get(paste0(x))) ) ]
        
        
        if(!is.null(pop_data)) {
          ## Merge in pops
          chaos_compiles <- merge(chaos_compiles, pop_data, c('iso3', 'year'))
          
          if(pop_action == 'div') {
            chaos_compiles[, paste0('draw_', c(1:N_draws)):= lapply(paste0('draw_', c(1:N_draws)), function(x) get(paste0(x)) / total_pop ) ]
          } else if(pop_action == 'multiply') {
            chaos_compiles[, paste0('draw_', c(1:N_draws)):= lapply(paste0('draw_', c(1:N_draws)), function(x) get(paste0(x)) * total_pop ) ]
          }
          
          chaos_compiles[, total_pop:=NULL]
        }
        
        ## Create stats
        if(hack_drop_NAs) {
          chaos_compiles <- chaos_compiles[!is.na(draw_1)]
          chaos_stats <- stat_maker(data = chaos_compiles, idvar = c('iso3', 'scenario', 'year'), melt = T, merge = F)  
          
        } else {
          chaos_stats <- stat_maker(data = chaos_compiles, idvar = c('iso3', 'scenario', 'year'), melt = T, merge = F)  
        }
        
      }
      
      return(list(draws = chaos_compiles, stats = chaos_stats))
      
    }
    
    ######################################################################
    ### Math functions
    ######################################################################
    
    ## Lemon squeeze logit transformer
    logit_trans <- function(x) {
      return( log(((x*999/1000) + (0.5/1000))/(1 - ((x*999/1000) + (0.5/1000)))) )
    }
    
    ## Inv Lemon squeeze logit transformer
    inv_logit_trans <- function(x) {
      return( ((exp(x)/(1 +exp(x))) - (0.5/1000))*(1000/999)  )
    }
    
    
    ### Stat making function across draws
    stat_maker <- cmpfun(function(data, melt = F, merge = T, idvar = c('iso3', 'year')) {
      
      ## Melt
      if(melt == T) {
        ## Melt super long
        print("Melting data")
        data2 <- melt(data, idvar, value.name = "data", variable.name = "draws")
      } else {
        data2 <- data
      }
      
      
      
      ## Get stats
      print("Make stats")
      data_stats <- data2[, as.list(c(mean(data ), 
                                      quantile(data, c(0.025, 0.975) ))),  by = idvar]
      
      colnames(data_stats) <- c(idvar, "mean", "lower", "upper")
      setkeyv(data_stats, idvar)
      
      if(merge) {
        ## Merge with data
        print("Merge with data")
        outdata <- merge(data_stats, data, idvar)
        setkeyv(outdata, idvar)
        return(outdata)
      }
      
      else {
        return(data_stats)
      }
      
      
      
    })
    
    
    
    
    ######################################################################
    ### Copula functions
    ######################################################################
    
    
    
    ## Get stats dataframe and get past correlation across time
    get_past_corr <- function(data_stats, metadata_list) {
      input_corr <- data_stats[year %in% c(eval(metadata_list$end_fit + metadata_list$oos_years):metadata_list$end_FC),
                                .(corrs = cor(mean, year)), 'iso3']
      return(input_corr)
    }
    
    ## Prep draw data
    prep_array_for_copula <- function(data_draws, metadata_list) {
      draws_melt <- melt(data_draws[year >= metadata_list$end_fit],
                         id.vars = c("iso3", "year"), value.name = "data", variable.name = "draw")
      draws_array <- reshape2::acast(draws_melt, iso3 ~ draw ~ year, value.var = "data")
      return(draws_array)
    } 
    
    
    
    
    ### Copula functions
    draw1Dcopula <- cmpfun(function(X, corr_mat, print=FALSE){
      mvdat <- t(mvrnorm(n=dim(X)[1], mu=0 * 1:dim(X)[2], Sigma=corr_mat))
      ranks <- t(apply(mvdat, 1, rank, ties.method="first"))
      sorted_X <- apply(X, 2, sort)
      sapply(1:dim(X)[2], function(x) sorted_X[,x][as.vector(ranks[x,])])
    })
    
    draw2Dcopula <- cmpfun(function(X, cor_mat){
      L <- dim(X)[2]
      D <- dim(X)[3]
      Xsum <- apply(X, c(2, 3), sum)
      mvdat <- mvrnorm(n=D, mu=0 * 1:L, Sigma=cor_mat, empirical=TRUE)
      ranks <- apply(mvdat, 2, rank, ties.method="first")
      sortedXsim <- apply(Xsum, 1, function(x) sort(x, index.return=TRUE)$ix)
      sortedX <- Xtad
      for(i in 1:L){
        sortedX[,i,] <- Xtad[,i,sortedXsim[,i]]
      }
      Xcorr <- sortedX
      for(i in 1:L){
        Xcorr[,i,] <- sortedX[,i,ranks[,i]]
      }
      Xcorr
    })
    
    
    ## Run copula for each country
    copula_by_country <- function(draws_array, iso, input_corr, metadata_list, AR1=T) {
      
      # Create a Correlation matrix (if using AR(1))
      if(AR1) {
        corr_mat <- (input_corr[iso3==paste0(iso), corrs])**abs(outer(0:(metadata_list$end_FC - eval(metadata_list$end_fit)), 
                                                                      0:(metadata_list$end_FC - eval(metadata_list$end_fit)), "-"))
      } else {
        ## Make square matrix
        corr_mat <- matrix(rep(input_corr[iso3==paste0(iso), corrs], (metadata_list$end_FC - eval(metadata_list$end_fit) + 1)**2), 
                           nrow = metadata_list$end_FC - eval(metadata_list$end_fit) + 1)
        
        ## Set diagnoals to 1
        diag(corr_mat) <- 1
      }
      
      
      ## Correlate the array for each country
      draws_corr <- data.table(t(draw1Dcopula(X = draws_array[paste0(iso),,], corr_mat = corr_mat)))
      
      ## Make it nice and pretty to be returned
      colnames(draws_corr) <- paste0("draw_", c(1:metadata_list$N_draws))
      draws_corr[, year:= metadata_list$end_fit:metadata_list$end_FC]
      draws_corr[, iso3:= paste0(iso)]
      
      setcolorder(draws_corr, c("iso3", "year", paste0("draw_", c(1:metadata_list$N_draws))))
      
      return(draws_corr)
    } 
    
    
    
    ## Take copulated output for all countries, bind to past and make stats
    finalize_copula_draws <- function(data_draws, copula_draws, metadata_list) {
      
      ## Keep only observed past
      data_draws <- data_draws[year < metadata_list$end_fit]
      
      ## Bind with copula draws
      data_draws <- rbindlist(list(data_draws, copula_draws), use.names = T)
      setkeyv(data_draws, c('iso3', 'year'))
      
      return(data_draws)
      
    }
    
    
    ### Run all copula sequence
    correlate_draws_1D <- function(draws = chaos_draws, stats = chaos_stats, metadata_list = metadata_list, AR1 = T) {
      
      print("Get correlations")
      input_corr <- get_past_corr(stats, metadata_list)
      
      print("Prepping draws")
      draws_array <- prep_array_for_copula(draws, metadata_list)
      
      print("Run copula by country")
      copula_draws <- rbindlist(foreach(iso = dimnames(draws_array)[[1]]) %do% 
                                  copula_by_country(draws_array = draws_array, iso = iso, input_corr = input_corr, metadata_list, AR1 = AR1)
                                )
      
      print("Finalize draws")
      cop_draws <- finalize_copula_draws(draws, copula_draws, metadata_list)
      cop_stats <- stat_maker(data = cop_draws, melt = T, merge = F, idvar = c('iso3','year'))
      
      return(list(draws = cop_draws, stats = cop_stats))
    }
    
    
    
    
    ######################################################################
    ### Scenarios
    ######################################################################
    
    
    ## Create the mean scenario trajectories, based on the convergence regressions
    mean_scenario_model <- function(data, metadata_list, aroc_years, transform = 'log') {
      
      #### Input: a dataframe with iso3, year, and mean columns
      ### metadata list : for year info
      ### aroc_years : how many years to use for growth regression
      
      ## Error out if we are trying to AROC more years than what exists in the observed data
      if(aroc_years > metadata_list$end_fit - metadata_list$start_year) {
        stop(paste0("Not enough years of observed data to compute ", aroc_years, " years worth of AROC."))
      }
      
      ## First, take out the values of mean, and keep only the years to be used in the conv reg
      first_yr_scen <- metadata_list$end_fit - aroc_years
      second_yr_scen <- metadata_list$end_fit
      third_yr_scen <- metadata_list$end_fit + aroc_years
      
      scen_data <- data[year %in% c(first_yr_scen, second_yr_scen, third_yr_scen), 
                        .(year, gr_rate = get(transform)(mean) - shift(get(transform)(mean)), conv_future = get(transform)(mean)), by = 'iso3']
      
      ## Anchor everything at the second_yr_scen point
      scen_data[, conv_past:= shift(conv_future)] 
      scen_data <- scen_data[year == second_yr_scen]
      
      
      ## Run regression
      summary(scenario_reg <- lm(data = scen_data, formula = gr_rate ~ 1 + conv_past))
      
      ## Record the intercept, coefficient, and the worse/better quantiles
      int_val <- coef(scenario_reg)[1]
      conv_coef_val <- coef(scenario_reg)[2]
      resid_worse <- quantile(residuals(scenario_reg),0.15)
      resid_better <- quantile(residuals(scenario_reg),0.85)
      
      
      ## Start by making the baseline 'constant' values
      better_const = exp(int_val  + resid_better)**(1/aroc_years)
      worse_const = exp(int_val  + resid_worse)**(1/aroc_years)
      
      
      ## Prep our output dataset
      output_data <- data[, .(iso3, year, reference = (mean))]
      
      ## Make placeholders for better and worse logged
      output_data[year <= metadata_list$end_fit, better:= reference]
      output_data[year <= metadata_list$end_fit, worse:= reference]
      
      
      ## Loop over each year from t+1 ... T, and make predictions where:
      ## Y_t = Y_t-1 * constant *  (Y_t-1**beta)
      for(yrs in c(eval(metadata_list$end_fit+1):metadata_list$end_FC)) {
        
        ## Create placeholder variables for better and worse values with a lag
        output_data[, better_lag:= shift(better), by = 'iso3']
        output_data[, worse_lag:= shift(worse), by = 'iso3']
        
        ## Create B/W forecasts at 'yrs'
        output_data[year == yrs, better:= (better_lag * better_const * (better_lag**conv_coef_val)**(1/aroc_years))]
        output_data[year == yrs, worse:= (worse_lag * worse_const * (worse_lag**conv_coef_val)**(1/aroc_years))]
        
        output_data[, better_lag := NULL]
        output_data[, worse_lag := NULL]
        
      }
      
      ## Cap if better<reference or worse>reference
      output_data[worse>reference, worse:= reference]
      output_data[better<reference, better:= reference]
      
      return(output_data) 
      
    }
    
    
    #### Propagate uncertainty in transformed space
    scenario_uncertainty <- function(mean_data, draws_data, metadata_list, transform = 'log', reverse_scenario = F) {
      
      
      ## Check column requirements
      if(any(!colnames(mean_data) %in% c('iso3', 'year', 'reference', 'better', 'worse'))) {
        stop("Column names for mean_data must have iso3, year, reference, better, worse")
      }
      if(any(!colnames(draws_data) %in% c('iso3', 'year',  paste0('draw_', c(1:metadata_list$N_draws))) )) {
        stop("Column names must have iso3, year, drawz")
      }
      
      ## Set inverse transform calls
      if(transform == 'log') rev_trans = 'exp'
      if(transform == 'logit') rev_trans = 'invlogit'
      if(transform == 'level') rev_trans = function(x) x*1
      
      if(any(!transform %in% c('log', 'logit', 'level')) ) {
        stop("Transformation must be one of: log, logit, level")
      }
      
      print("Take the draws dataset, log things, get the rowMeans and create a deviation matrix")
      
      # draws_data = copy(correlated_output$draws)
      
      ### NOTE: We do this extra line of subset to not have data.table modify the draws inplace
      draws_data <- draws_data[,.SD,  .SDcols = c('iso3', 'year', paste0('draw_', c(1:metadata_list$N_draws)))]
      draws_data[, log_ref_mean:= log(rowMeans(.SD)), .SDcols = paste0('draw_', c(1:metadata_list$N_draws))]
      draws_data[,  paste0('draw_', c(1:metadata_list$N_draws)):= lapply( paste0('draw_', c(1:metadata_list$N_draws)), function(x) get(transform)(get(x)))]
      draws_data[,  paste0('dev_', c(1:metadata_list$N_draws)):= lapply( paste0('draw_', c(1:metadata_list$N_draws)), 
                                                                                       function(x) ( get(paste0(x)) - log_ref_mean )  ) ]
      
      
      
      print("Replicate base data to make better and worse")
      dev_data <- draws_data[, .SD, .SDcols = c('iso3', 'year', paste0('dev_', c(1:metadata_list$N_draws)))]
      dev_data <- dev_data[rep(1:.N, 3)][, scenario:= c('reference', 'better', 'worse'), by = c('iso3', 'year')]
      # dev_data <- dev_data[, .SD, .SDcols = c('iso3', 'year', 'scenario', paste0('dev_', c(1:metadata_list$N_draws)))]
      
      print("Now, take the mean_data DT, melt by scenario, and merge on the deviation")
      draws_scenario <- melt(mean_data, c('iso3', 'year'), variable.name = 'scenario', value.name = 'data')
      draws_scenario[, log_data:= log(data)]
      draws_scenario <- merge(dev_data, draws_scenario, c('iso3', 'scenario', 'year'), all.x=T)
      
      ## Let's encode scenarios to FBD esque style
      draws_scenario[, scen:= 0]
      
      ### Reverse scenario if asked
      if(reverse_scenario) {
        draws_scenario[scenario == 'worse', scen:= 1]
        draws_scenario[scenario == 'better', scen:= -1]
      } else {
        draws_scenario[scenario == 'worse', scen:= -1]
        draws_scenario[scenario == 'better', scen:= 1]
      }

      draws_scenario[, scenario:= NULL]
      setnames(draws_scenario, 'scen', 'scenario')
      
      print("Apply deviation to all draws")
      draws_scenario[, paste0('draw_', c(1:metadata_list$N_draws)):= lapply(c(1:metadata_list$N_draws), function(x) get(rev_trans)(get(paste0('dev_', x)) + log_data) ) ]
      
      draws_scenario <- draws_scenario[, .SD, .SDcols = c('iso3', 'year', 'scenario', paste0('draw_', c(1:metadata_list$N_draws)))]
      

      print("Make stats")
      scenario_stats <- stat_maker(data = draws_scenario, melt = T, merge = F, idvar = c('iso3', 'scenario', 'year'))
      
      
      ## Return stuff
      list_out <- list()
      list_out[['draws']] = draws_scenario
      list_out[['stats']] = scenario_stats
      return(list_out)
      
    }
    
    
    
    