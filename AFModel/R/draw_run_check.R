## Checking whether all draw models have finished running
draw_run_check <- function(root_fold, array_grid) {

  ## Get list of draws already there
  list_of_draws <- data.table(model_number = as.numeric(strsplit(list.files(paste0(root_fold, "/draws")), ".feather")))[, file := model_number]
  merged_mods <- merge(array_grid, list_of_draws, "model_number", all.x = T)


  print(paste0(nrow(list_of_draws), " models out of ", nrow(merged_mods), " have finished running"))

  if (length(merged_mods[is.na(file), model_number]) > 0) {
    print("The following models are missing:")
    print(merged_mods[is.na(file)][order(id)])
    stop("Not all the draws have been run. Stopping.")
  }
}
