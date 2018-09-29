    
    ###################################################
    ### Author:  Nafis Sadat
    ### Purpose: Central script to hold and submit forecasting jobs
    ###################################################
    
    
    rm(list=ls())
    
    ## Source the job helpers
    source('~/forecasting/AFModel/AFModel_helpers.r')
    
    ########################################
    #### (1) Run and variable information
    ########################################
    
    ## Set up run info
    variable = 'poverty'
    comment = 'p190_v9'
    
    date = '20180810'
    
    chaos <- T
    
    if(chaos) {
      cha = 'Add Chaos'
    } else {
      cha = 'No Chaos'
    }
    
    comment_long = paste0(date, '; ', cha, '; NEW DATA TODAY; force GDPpc; crank down upw') 
    
    
    
    
    ## Prep empty folders and return root
    root_fold <- prep_model(variable, comment, date, erase = F)
    
    
    ## Set forecasting year cutoff info
    start_year <- 1980
    end_fit <- 2016
    end_FC <- 2050
    oos_years <- 10
    N_draws <- 1000
    
    ## Pack all the metadata in a list
    metadata_list <- list(variable = variable, 
                          date = date,
                          comment = comment, 
                          comment_long = comment_long, 
                          start_year = start_year, 
                          end_fit = end_fit, 
                          end_FC = end_FC, 
                          oos_years = oos_years,
                          chaos = chaos,
                          N_draws = 1000)
    
    
    ########################################
    #### (2) Ensemble information
    ########################################
    
    ## Create a metadata for the ensemble grid innards
    ensemble_metadata <- list(yvar = c('logit_pov_p190'),
                              xvar = paste0('FE_', c("ln_gdppc", "edu",
                                                     "ln_TFR", "logit_gge_gdp" )),
                              re_coef = NULL,
                              ar = 3, ar_mod = c(1,3), ma = 0, 
                              weight_decays = c(0,0.1,0.25,0.5),
                              global_int = 1, country_int = 1, country_int_dist = 1,
                              fdiff = 1, conv = c(0,1))
    
    
    ## Create ensemble model grid for GDP and GDPpc
    full_grid <- do.call(create_ensemble_grid, ensemble_metadata)
    
    ## List our coefficient priors
    coef_priors <- list('conv' = -1, 'FE_ln_gdppc' = -1, 'FE_edu' = -1, 'FE_ln_TFR' = 1)
    
    
    #### Force GDPpc AND REMAKE ID
    full_grid <- full_grid[FE_ln_gdppc == 1]
    full_grid[, id:= .I]
    
    
    ## Save them in root
    save(list = c('metadata_list',  'full_grid', 'ensemble_metadata', 'coef_priors'), file = paste0(root_fold, '/summary_files/mean_array.Rdata'))
    
    
    
    ########################################
    #### (3) Submit mean jobs
    ########################################
    
    
    ## Create qsub command
    proj <- ' -P proj_fin_forecast '
    slots <- ' -pe multi_slot 1'
    arrays <- paste0('1:',nrow(full_grid)) 
    # arrays = '1:2'
    qsub_mean <- paste0('qsub -N pov_p190_mean_', comment, ' ',
                        proj,
                        slots,
                        ' -t ' , arrays,
                        ' -j y -o /share/temp/sgeoutput/sadatnfs/output ',
                        ' /share/singularity-images/health_fin/forecasting/shells/health_fin_forecasting_shell_mkl_singularity.sh ',
                        ' 1 ~/forecasting/poverty/p190/03_mean_forecast_model.r ',
                        ' --variable ', variable, ' --date ', date, ' --comment ', comment, ' --oos ', oos_years)
    
    
    ## Submit!
    system(qsub_mean)
    
    
    ## Keep sleeping until donesies
    job_hold(paste0('pov_p190_mean_', comment))
    
    
    
    
    ########################################
    #### (4) Check and collect ensemble
    ########################################
    
    # Did all the models finish running? (and return models)
    # failed_models <- mean_run_check(root_fold = root_fold, mean_array_grid = full_grid, return_fail = T)
    # 
    # ## Qsubbing for failed jobs
    # arrays_failed <- failed_models
    # proj <- ' -P proj_fin_forecast '
    # slots <- ' -pe multi_slot 1'
    # 
    # lapply(failed_models, function(i) {
    #   system(paste0('qsub -N pov_p190_mean_', comment, ' ',
    #          proj,
    #          slots,
    #          ' -t ' , i,
    #          ' -j y -o /share/temp/sgeoutput/sadatnfs/output ',
    #          ' /share/singularity-images/health_fin/forecasting/shells/health_fin_forecasting_shell_mkl_singularity.sh ',
    #          ' 1 ~/forecasting/poverty/p190/03_mean_forecast_model.r ',
    #          ' --variable ', variable, ' --date ', date, ' --comment ', comment, ' --oos ', oos_years))
    #   return(0)
    # })
    
    
    ## Quick load:
    # load(paste0(root_fold, '/summary_files/mean_array.Rdata'))
    
    
    ## Get list of models passed
    passed_models <- mean_run_check(root_fold = root_fold, 
                                    mean_array_grid = full_grid, 
                                    return_fail = F)
    
    
    ## Bring in all the postfiles
    data_models_passed <- mean_postfiles_compile(root_fold, passed_models, mc.cores = 15)
    save(list = 'data_models_passed', file = paste0(root_fold, '/summary_files/data_models_passed.Rdata') )
    
    
    ## Apply coefficient direction priors
    data_models_passed <- prior_coefs_mean(data = data_models_passed,
                                           coef_prior_list =  coef_priors)
    
    
        
    ## Distribute draws and chaos RMSE and SRREG and all that. This works regardless of Chaos flag
    rmse_distro <- chaos_draw_distro_DMC(data = data_models_passed, chaos_pc = 10, 
                                         metadata = ensemble_metadata,
                                       iso_portion = 2/3, 
                                       region_portion = 1/3, super_region_portion = 0, 
                                       N_draws = N_draws)
    
    
    ## Fix order of the covariates in RMSE_DISTRO (VERY IMPORTANT TODO THIS INTERNALLY)
    

    
    
    ## Save out metadata and distro
    save(list=c('rmse_distro', 'metadata_list'), file = paste0(root_fold, '/summary_files/draws_array.Rdata'))
    
    
    
    ## Quick load:
    # load(paste0(root_fold, '/summary_files/draws_array.Rdata'))
    
    ########################################
    #### (5) Submit draws
    ########################################
    
    
    ## Job environment
    slotz <- 1; mem <- slotz*2
    
    proj <- ' -P proj_fin_forecast '
    slots <- paste0(' -pe multi_slot ', slotz, ' ')
    arrays <- paste0('1:',nrow(rmse_distro$array_grid))
    
    shell_file = " /share/singularity-images/health_fin/forecasting/shells/health_fin_forecasting_shell_singularity.sh"
    
    ## Empty out draws: # system(paste0('rm ', root_fold, '/draws/*'))
    
    ## Make qsub line and submit
    qsub_draws <- paste0('qsub -N pov_p190_draws_', comment, ' ',
                         proj,
                         slots,
                         ' -t ' , arrays,
                         ' -j y -o /share/temp/sgeoutput/sadatnfs/output ',
                         ' /share/singularity-images/health_fin/forecasting/shells/health_fin_forecasting_shell_mkl_singularity.sh ',
                         ' 1 ~/forecasting/poverty/p190/04_forecast_draws.r ',
                         ' --variable ', variable, ' --date ', date, ' --comment ', comment)
    
    ## Submit!
    system(paste0(qsub_draws))
    
    ## Keep sleeping until donesies
    job_hold(paste0('pov_p190_draws_', comment))
    
    
    
    
    ########################################
    #### (6) Check and collect draws (Chaos or not)
    ########################################
    
    ## Quick load:
    # load(paste0(root_fold, '/summary_files/draws_array.Rdata'))
    
    
    ## Check if draws have finished running successfully for all the models
    draw_run_check(root_fold = root_fold, array_grid = rmse_distro$array_grid)
    
    ## Submit Chaos code for each OOS year
    slotz <- 2
    proj <- ' -P proj_fin_forecast '
    slots <- paste0(' -pe multi_slot ', slotz, ' ')
    
    if(metadata_list$chaos) { 
      chaos_car = 'T' 
    } else {
      chaos_car = 'F'
    }
    
    chaos_qsubber <- function(oos, chaos_car) { 

        paste0('qsub -N ', paste0('p190_', chaos_car, '_', comment, '_', oos ),
               proj,
               slots,
               ' -j y -o /share/temp/sgeoutput/sadatnfs/output ',
               ' /share/singularity-images/health_fin/forecasting/shells/health_fin_forecasting_shell_mkl_singularity.sh ',
               ' 10 ~/forecasting/poverty/p190/05_chaos_compile.r ',
               ' --variable ', variable, ' --date ', date, ' --comment ', comment, ' --oos ', oos, ' --chaos ', chaos_car)
      }  

    
    
    ## Submit for each OOS year (if Chaos)
    if(chaos) {
      for(oos in c(1:metadata_list$oos_years)) system(chaos_qsubber(oos, chaos_car))
    } else {
      system(chaos_qsubber(1, chaos))
    }
    
    
    ## Keep sleeping until donesies
    job_hold(paste0('p190_', chaos_car, '_', comment))
    
    ## Check if finished
    chaos_collector_check(root_fold = root_fold, oos_years = metadata_list$oos_years, chaos = metadata_list$chaos)
    
    
    #### ONLY FOR POVERTY: 
    ###### Fix past draw data because missingness and alignment with GPR
    
    
    
    lox <- get_lox()[level == 3  , location_id]
    
    ## Get draws
      pov_p190_draws <- data.table(read_feather('/home/j/Project/IRH/POVERTY/DATA/New_Beemus/poverty_complete_gpr_output_m44536_p190_20180810.feather', 
                                                columns = c('location_id', 'iso3', 'year_id', paste0('draw_', c(0:999)))))
      pov_p190_draws <- pov_p190_draws[(year_id %in% c(1980:eval(metadata_list$end_fit ) )) & (location_id %in% lox)]
      pov_p190_draws[, location_id:= NULL]
      colnames(pov_p190_draws) <- c('iso3', 'year', paste0('draw_', c(1:metadata_list$N_draws)))
      
    ## Diff
      pov_p190_draws[, paste0('draw_', c(1:metadata_list$N_draws)):= lapply(paste0('draw_', c(1:metadata_list$N_draws)), 
                                                                            function(v) logit_trans(get(v)) )]
      pov_p190_draws[, paste0('diff_draw_', c(1:metadata_list$N_draws)):= lapply(paste0('draw_', c(1:metadata_list$N_draws)), 
                                                                                 function(v) get(v) - shift(get(v)) ), by = 'iso3']
      pov_p190_draws[year > 1980, paste0('draw_', c(1:metadata_list$N_draws)):= lapply( paste0('diff_draw_', c(1:metadata_list$N_draws)),
                                                                                        function(v) (get(v)))]
      pov_p190_draws[, paste0('diff_draw_', c(1:metadata_list$N_draws)):= NULL]
    
      print("Make past draw corrections")  
      
    if(chaos) {
      chaos_output <- readRDS(paste0(root_fold, "/summary_files/TMP_chaos_oos_1.rds"))
      chaos_output <- chaos_output[year >= 2017]
      chaos_output <- rbindlist(list(chaos_output, pov_p190_draws), use.names=T)
      setkeyv(chaos_output, c('iso3', 'year'))
      
      saveRDS(chaos_output, paste0(root_fold, "/summary_files/chaos_oos_1.rds"))
    } else {
      chaos_output <- readRDS(paste0(root_fold, "/summary_files/TMP_chaos_oos.rds"))
      chaos_output <- chaos_output[year >= 2017]
      chaos_output <- rbindlist(list(chaos_output, pov_p190_draws), use.names=T)
      setkeyv(chaos_output, c('iso3', 'year'))
      
      saveRDS(chaos_output, paste0(root_fold, "/summary_files/chaos_oos.rds"))
    }
    
    
    ## Cumsum all the data and convert to GDPpc
    chaos_list <- final_cum_sum(root_fold = root_fold, 
                                chaos = metadata_list$chaos, 
                                oos_years = metadata_list$oos_years, 
                                N_draws = metadata_list$N_draws, 
                                rev_trans = 'inv_logit_trans')

    
    
    
    
    ########################################
    #### (7) Copulate draws
    ########################################
    
    ## Correlated draws with AR1 covariance
    correlated_output <- correlate_draws_1D(draws = chaos_list[['draws']], stats = chaos_list[['stats']], metadata_list = metadata_list, AR1=T)
    
    ## Save out
    write_feather(correlated_output[['draws']], paste0(root_fold, '/summary_files/correlated_draws.feather'))
    fwrite(correlated_output[['stats']], paste0(root_fold, '/summary_files/correlated_stats.csv'))
    
    
    
    
    ########################################
    #### (9) Upload stats to DB
    ########################################
    
    ### REDACTED ###

    q('no')
    
    
    
    
    
    
    
    