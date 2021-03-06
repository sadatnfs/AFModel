### Create grid of covariates and specification
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param yvar PARAM_DESCRIPTION
#' @param xvar PARAM_DESCRIPTION
#' @param re_coef PARAM_DESCRIPTION, Default: NULL
#' @param ar PARAM_DESCRIPTION, Default: c(0)
#' @param ar_mod PARAM_DESCRIPTION, Default: c(2)
#' @param ma PARAM_DESCRIPTION, Default: c(0)
#' @param ma_mod PARAM_DESCRIPTION, Default: c(2)
#' @param weight_decays PARAM_DESCRIPTION, Default: c(0)
#' @param global_int PARAM_DESCRIPTION, Default: 1
#' @param country_int PARAM_DESCRIPTION, Default: 1
#' @param country_int_dist PARAM_DESCRIPTION, Default: 1
#' @param fdiff PARAM_DESCRIPTION, Default: c(1)
#' @param conv PARAM_DESCRIPTION, Default: c(0, 1)
#' @param scaled_lev_conv PARAM_DESCRIPTION, Default: c(0)
#' @param ar_constrain PARAM_DESCRIPTION, Default: 0
#' @param int_decay PARAM_DESCRIPTION, Default: 0
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname create_ensemble_grid
#' @export
create_ensemble_grid <- function(yvar,
                                 xvar,
                                 re_coef = NULL,
                                 ar = c(0), ar_mod = c(2),
                                 ma = c(0), ma_mod = c(2),
                                 weight_decays = c(0),
                                 global_int = 1,
                                 country_int = 1,
                                 country_int_dist = 1,
                                 fdiff = c(1),
                                 conv = c(0, 1),
                                 scaled_lev_conv = c(0),
                                 ar_constrain = 0,
                                 int_decay = 0) {


  ### Some checks to break

  ## Break if ar_mod is not null where as AR is
  if (!is.null(ar_mod) & is.null(ar)) {
    stop("'ar' can't be null while 'ar_mod' is specified")
  }

  ## Same with MA
  if (!is.null(ma_mod) & is.null(ma)) {
    stop("'ma' can't be null while 'ma_mod' is specified")
  }

  ## Convergence term can't be true if fdiff does not have 1
  if (!(1 %in% fdiff) & (1 %in% conv)) {
    stop("'conv' can't be true if fdiff models aren't in")
  }


  ## Break if the random coefficient is not also included as a fixed effect
  if (any(paste0("FE_", re_coef) %in% xvar == F) & !is.null(re_coef)) {
    stop("Random coefficients MUST have a counterpart fixed effect")
  }



  ## Add fixed effects
  if (is.null(xvar)) {
    tmp <- (rep(list(0), 1))
    regMat <- data.table(do.call(expand.grid, tmp))
  } else {
    ## Random coefs?
    if (!is.null(re_coef)) {

      ## Create grid of xvars
      tmp <- (rep(list(0:2), eval(length(xvar))))
      regMat <- data.table(do.call(expand.grid, tmp))
      colnames(regMat) <- c(xvar)

      ## Drop where the non REs are equal to 2
      for (bads in setdiff(xvar, paste0("FE_", re_coef))) {
        regMat <- regMat[ get(bads) != 2]
      }
    } else {

      ## Create grid of xvars
      tmp <- (rep(list(0:1), eval(length(xvar))))
      regMat <- data.table(do.call(expand.grid, tmp))
      colnames(regMat) <- c(xvar)
    }
  }



  ## Cycle over AR terms?
  regMat <- duplicate_switch(data = regMat, param_name = "ar", param_domain = ar)

  ## Which types of AR specification are we using?
  regMat <- duplicate_switch(data = regMat, param_name = "ar_mod", param_domain = c(0, ar_mod))

  ## Drop if AR == 0 and ar_mod != 0, or if AR != 0 and ar_mod == 0
  regMat <- regMat[!(ar == 0 & ar_mod > 0)]
  regMat <- regMat[!(ar != 0 & ar_mod == 0)]


  ## Constrain AR terms?
  regMat <- duplicate_switch(data = regMat, param_name = "ar_constrain", param_domain = ar_constrain)

  ## Set ar_constrain to 0 if ar == 0
  regMat <- regMat[ar == 0, ar_constrain := 0]

  ## Make sure there's no duplication
  regMat <- unique(regMat)


  ## Cycle over MA terms?
  regMat <- duplicate_switch(data = regMat, param_name = "ma", param_domain = ma)

  ## Which types of AR specification are we using?
  regMat <- duplicate_switch(data = regMat, param_name = "ma_mod", param_domain = c(0, ma_mod))

  ## Drop if MA == 0 and ma_mod != 0, or if MA != 0 and ma_mod == 0
  regMat <- regMat[!(ma == 0 & ma_mod > 0)]
  regMat <- regMat[!(ma != 0 & ma_mod == 0)]


  ## Cycle over time weights?
  regMat <- duplicate_switch(data = regMat, param_name = "weight_decay", param_domain = weight_decays)

  ## Cycle over global intercept?
  regMat <- duplicate_switch(data = regMat, param_name = "global_int", param_domain = global_int)

  ## Cycle over country intercept?
  regMat <- duplicate_switch(data = regMat, param_name = "country_int", param_domain = country_int)

  ## Random or fixed country intercept?
  if (country_int_dist) {
    regMat[, country_int_dist := 1]
  } else {
    regMat[, country_int_dist := 0]
  }

  ## Cycle over fixed difference and levels?
  regMat <- duplicate_switch(data = regMat, param_name = "fdiff", param_domain = fdiff)

  ## Cycle over convergence term, but ONLY set conv=T if fdiff==1
  regMat <- duplicate_switch(data = regMat, param_name = "conv", param_domain = conv)
  regMat <- regMat[!(fdiff == 0 & conv == 1)]

  ## Cycle over scaled convergence term, but ONLY set T if fdiff==1
  regMat <- duplicate_switch(data = regMat, param_name = "scaled_lev_conv", param_domain = scaled_lev_conv)
  regMat <- regMat[!(fdiff == 0 & scaled_lev_conv == 1)]

  ## Random effect decay (only if country_int == 1)
  regMat <- duplicate_switch(data = regMat, param_name = "int_decay", param_domain = int_decay)
  regMat <- regMat[!(country_int == 0 & int_decay == 1)]

  ## Finally, create yvar grid
  regMat <- duplicate_switch(data = regMat, param_name = "yvar", param_domain = yvar)

  ## Drop V1 if xvar is NULL
  if (is.null(xvar)) {
    regMat[, Var1 := NULL]
  }

  ## Make sure there's no duplication
  regMat <- unique(regMat)

  ## Tag model numbers
  regMat[, id := .I]

  return(regMat)
}
