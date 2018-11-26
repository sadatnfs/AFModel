## Bring in mean forecast estimates
## Bring in mean postfile outputs
mean_estimates_compile <- function(root_fold, passed_models, mc.cores) {
  registerDoParallel(cores = mc.cores)

  ## Load the passed models stuff on the number of rows in all models passed
  system.time(data_models_passed_list <- rbindlist(foreach(model = passed_models) %dopar% {

    ## Load the output meanz data
    load(paste0(root_fold, "/means/", model, ".Rdata"))

    ## Only return mean_forecast
    return(mean_forecast[, model_number := model])
  }, use.names = T, fill = T))

  on.exit(stopImplicitCluster())
  return(data_models_passed_list)
}
