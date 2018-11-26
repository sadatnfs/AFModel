#' Get model specifications from an ensemble grid
#'
#' @description ## This function will take in the full array grid and return a list with all the specs
#' @usage get_model_specs(data, model, draws = F)
#'
#' @param data data.table, an ensemble grid of model specification, created from `create_ensemble_grid`
#' @param model int > 0, model IF
#' @param draws int > 0, number of draws to assign to this model
#'
#' @return a list with the following keys:
#' xvar, yvar, re_coef,
#' ar, ar_mod, ma, ma_mod, weight_decay,
#' global_int, country_int, country_int_dist,
#' fdiff, conv, scaled_lev_conv,
#' draws, model, ar_constrain
#'
#'
#' @examples
#' library("data.table")
#' \dontrun{
#' # Create the ensemble grid
#' ens_grid <- create_ensemble_grid(yvar = 'Y', xvar = c('X1', 'X2'), ar = 1)
#'
#' # Run function
#' get_model_specs(ens_grid, 1)
#' }
#'
#'
#' @export
get_model_specs <- function(data, model, draws = F) {

  # data = copy(GDPs_full_grid)
  # data = rmse_distro

  if (draws) {
    draws <- data$array_grid[id == model, drawz]
    model <- data$array_grid[id == model, model_number]
    array_grid <- data$data[model_number == model ][1, ]
  } else {
    array_grid <- data[id == model]
    draws <- 0
  }




  ## Get into on FEs and REs

  ## If mean estimations:
  if (draws == F) {
    ## FEs
    dtset <- array_grid[, .SD, .SDcols = colnames(array_grid)[grep("FE_", colnames(array_grid))]]

    if (nrow(dtset) == 0) {
      covars <- NULL
    } else {
      covars <- gsub("FE_", "", data.table(t(dtset), keep.rownames = T)[V1 == 1, rn])
      if (identical(covars, character(0))) {
        covars <- NULL
      }
    }

    ## RE coefs?
    if (nrow(dtset) == 0) {
      re_coefs <- NULL
    } else {
      re_coefs <- gsub("FE_", "", data.table(t(dtset), keep.rownames = T)[V1 == 2, rn])

      ## Add re_coefs to FE if covars is not NULL
      if (!is.null(covars)) {
        covars <- c(covars, re_coefs)
      }

      if (identical(re_coefs, character(0))) {
        re_coefs <- NULL
      }
    }
  } else {

    ## If draws:

    ## FEs
    dtset <- array_grid[, .SD, .SDcols = colnames(array_grid)[grep("FE_", colnames(array_grid))]]
    if (nrow(dtset) == 0) {
      covars <- NULL
    } else {
      covars <- gsub("FE_", "", data.table(t(dtset), keep.rownames = T)[!is.na(V1), rn])
      if (identical(covars, character(0))) {
        covars <- NULL
      }
    }

    ## RE coefs?
    dtset <- array_grid[, .SD, .SDcols = colnames(array_grid)[grep("RE_", colnames(array_grid))]]
    if (nrow(dtset) == 0) {
      re_coefs <- NULL
    } else {
      re_coefs <- gsub("RE_", "", data.table(t(dtset), keep.rownames = T)[!is.na(V1), rn])
      if (identical(re_coefs, character(0))) {
        re_coefs <- NULL
      }
    }
  }


  ## yvar
  yvar <- array_grid[, yvar]

  ## AR
  ar <- array_grid[, ar]
  ar_mod <- array_grid[, ar_mod]

  ## MA
  ma <- array_grid[, ma]
  ma_mod <- array_grid[, ma_mod]

  ## Weight decay
  weight_decay <- ifelse(draws, array_grid[, weights], array_grid[, weight_decay])

  ## Intercept stuff
  global_int <- array_grid[, global_int]
  country_int <- array_grid[, country_int]
  country_int_dist <- array_grid[, country_int_dist]

  ## Fdiff?
  fdiff <- array_grid[, fdiff]

  ## AR constrain?
  ar_constrain <- array_grid[, ar_constrain]

  ## Conv?
  if (draws == F) {
    conv <- array_grid[, conv]
  } else if (draws) {
    conv <- ifelse(!is.na(array_grid$conv), T, F)
  }

  ## Scaled conv?
  if (draws == F) {
    scaled_lev_conv <- array_grid[, scaled_lev_conv]
  } else if (draws) {
    scaled_lev_conv <- ifelse(!is.na(array_grid$scaled_c), T, F)
  }

  ## Outlist
  return(list(
    xvar = covars, yvar = yvar, re_coef = re_coefs,
    ar = ar, ar_mod = ar_mod, ma = ma, ma_mod = ma_mod, weight_decay = weight_decay,
    global_int = global_int, country_int = country_int, country_int_dist = country_int_dist,
    fdiff = fdiff, conv = conv, scaled_lev_conv = scaled_lev_conv,
    draws = draws, model = model, ar_constrain = ar_constrain
  ))
}
