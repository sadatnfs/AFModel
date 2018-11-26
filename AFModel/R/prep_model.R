###################################################
## File structuring
###################################################
## Function to prep for a run (makes folder structure)
prep_model <- function(variable, comment, date = gsub("-", "", Sys.Date()), erase = F) {
  folder_path <- "/share/resource_tracking/forecasting/"

  ## Make root folder
  dir.create(paste0(folder_path, variable, "/", date, "_", comment))

  ## Make all the necessary sub folders
  dir.create(paste0(folder_path, variable, "/", date, "_", comment, "/passed"))
  dir.create(paste0(folder_path, variable, "/", date, "_", comment, "/failed"))
  dir.create(paste0(folder_path, variable, "/", date, "_", comment, "/means"))
  dir.create(paste0(folder_path, variable, "/", date, "_", comment, "/draws"))
  dir.create(paste0(folder_path, variable, "/", date, "_", comment, "/summary_files"))
  dir.create(paste0(folder_path, variable, "/", date, "_", comment, "/summary_files_scenarios"))
  dir.create(paste0(folder_path, variable, "/", date, "_", comment, "/draws_scenarios"))
  dir.create(paste0(folder_path, variable, "/", date, "_", comment, "/draws_scenarios/worse"))
  dir.create(paste0(folder_path, variable, "/", date, "_", comment, "/draws_scenarios/better"))

  if (erase) {
    CONFIRM <- readline("Are you sure you want to erase all of the innards? \n")

    if (substr(CONFIRM, 1, 1) == "y" |
      substr(CONFIRM, 1, 1) == "Y" |
      substr(CONFIRM, 1, 1) == "yes" |
      substr(CONFIRM, 1, 1) == "Yes") {

      ## If the folders already exist, clean up the innards
      system(paste0("rm ", paste0(folder_path, variable, "/", date, "_", comment, "/passed/*")))
      system(paste0("rm ", paste0(folder_path, variable, "/", date, "_", comment, "/failed/*")))
      system(paste0("rm ", paste0(folder_path, variable, "/", date, "_", comment, "/means/*")))
      system(paste0("rm ", paste0(folder_path, variable, "/", date, "_", comment, "/draws/*")))
      system(paste0("rm ", paste0(folder_path, variable, "/", date, "_", comment, "/summary_files/*")))
      system(paste0("rm ", paste0(folder_path, variable, "/", date, "_", comment, "/summary_files/*")))
      system(paste0("rm ", paste0(folder_path, variable, "/", date, "_", comment, "/summary_files_scenarios/*")))
      system(paste0("rm ", paste0(folder_path, variable, "/", date, "_", comment, "/draws_scenarios/worse/*")))
      system(paste0("rm ", paste0(folder_path, variable, "/", date, "_", comment, "/draws_scenarios/worse/*")))
    } else {
      stop("Stopping now.")
    }
  }


  ## Return root folder of run
  return(paste0(folder_path, variable, "/", date, "_", comment))
}
