###################################################
## Job stuff
###################################################
### a job holding function to keep sleeping until certain jobs are done (from Patty Liu's cluster tools)
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param job_name PARAM_DESCRIPTION
#' @param sleeper PARAM_DESCRIPTION, Default: 10
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname job_hold
#' @export 
job_hold <- function(job_name, sleeper = 10) {

  ## Give it a sec to launch
  # Sys.sleep(5)

  ## Start timer
  start.time <- proc.time()

  ## Wait for job to finish
  flag <- 0
  while (flag == 0) {
    ## Check if job is done
    if (system(paste0("qstat -r | grep ", job_name, "|wc -l"), intern = T) == 0) {
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
