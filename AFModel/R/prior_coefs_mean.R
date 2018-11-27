######################################################################
### Post Means stuff
######################################################################
## This function will take in a list where the name of the members are named of the FEs with
## the values being -1,0,1, where:
## -1 : negative coefficient
##  0 : no prior
##  1 : positive coefficient
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param coef_prior_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname prior_coefs_mean
#' @export 
prior_coefs_mean <- function(data, coef_prior_list) {

  # data = copy(data_models_passed)
  # coef_prior_list = gdp_prior

  ## First, check that the names of the items in coef_prior_list are in colnames of data
  # if( length( intersect(names(coef_prior_list) , colnames(data)) )  != length(coef_prior_list) ) {
  #   stop("Column names of data and coef_prior_list aren't aligned")
  # }

  ## Apply the priors
  mod_num <- list()
  for (i in names(coef_prior_list)) {
    if (i %in% colnames(data)) {

      ## Check for which model numbers are defying our priors
      if (coef_prior_list[[i]] == -1) {
        mod_num[[i]] <- unique(data[!is.na(get(i)) & get(i) > 0, model_number])
      } else if (coef_prior_list[[i]] == 1) {
        mod_num[[i]] <- unique(data[!is.na(get(i)) & get(i) < 0, model_number])
      }
    }
  }
  ## Drop the unique model numbers from data
  data <- data[!(model_number %in% unique(unlist(mod_num))) ]

  data
}
