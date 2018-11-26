## Bring in mean postfile outputs
mean_postfiles_compile <- function(root_fold, passed_models, mc.cores) {
  registerDoParallel(cores = mc.cores)

  ## Load the passed models stuff on the number of rows in all models passed
  system.time(data_models_passed_list <- rbindlist(foreach(model = passed_models) %dopar% {

    ## Load the output meanz data
    return(fread(paste0(root_fold, "/means/", model, ".csv")))
  }, use.names = T, fill = T))

  on.exit(stopImplicitCluster())


  ## Exception for Chaos
  if (!("oos" %in% colnames(data_models_passed_list))) {
    setkeyv(data_models_passed_list, c("model_number", "iso3"))
    data_models_passed_list[, oos := NA]
  } else {
    setkeyv(data_models_passed_list, c("model_number", "iso3", "oos"))
  }
  return(data_models_passed_list)
}
