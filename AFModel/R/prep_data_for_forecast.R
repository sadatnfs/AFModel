### Prepping the data for forecasting
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param tmb_obj PARAM_DESCRIPTION
#' @param tmb_data_param PARAM_DESCRIPTION
#' @param tmb_params PARAM_DESCRIPTION, Default: NULL
#' @param draw_Xdata PARAM_DESCRIPTION, Default: NULL
#' @param draw_Ydata PARAM_DESCRIPTION, Default: NULL
#' @param verbose PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname prep_data_for_forecast
#' @export
prep_data_for_forecast <- function(tmb_obj,
                                   tmb_data_param,
                                   tmb_params = NULL,
                                   draw_Xdata = NULL,
                                   draw_Ydata = NULL,
                                   verbose = F) {

  # tmb_data_param = copy(data_params)
  # tmb_obj = copy(output_TMB$obj)

  ## Get the model specifications
  specs <- tmb_data_param$specifications

  ## Get the parameters estimates
  # mean_params <- data.table(as.matrix(tmb_obj$env$last.par.best), keep.rownames = T)[, inc1:=1]

  if (is.null(tmb_params)) {
    if (verbose) print("Using mean parameter estimates")
    tmb_params <- data.table(as.matrix(tmb_obj$env$last.par.best), keep.rownames = T)[, inc1 := 1]
  } else {
    tmb_params <- data.table(as.matrix(tmb_params), keep.rownames = T)[, inc1 := 1]
  }


  ## Draws or mean?
  if (is.null(draw_Xdata)) {
    if (verbose) print("Using mean X data from data_params object")
  }

  if (is.null(draw_Ydata)) {
    if (verbose) print("Using mean Y data from data_params object")
  }


  ### Prep data ###

  if (is.null(draw_Ydata)) {
    Y_data <- data.table(melt(tmb_data_param$data$Y_input))
    colnames(Y_data) <- c("iso3", "year", "yvar")
    Y_data[, ydiff := yvar - shift(yvar), "iso3"]
  } else {
    if (verbose) print("Using draws of Ydata")
    Y_data <- draw_Ydata[, .(iso3, year, yvar, ydiff)]
  }

  min_year <- min(Y_data$year)

  ## Create first difference of x_input if the model has FDs
  if (specs$fd) {

    ## Prep the xvars
    if (!is.null(specs$xvar)) {
      if (is.null(draw_Xdata)) {
        X_data <- data.table(dcast(melt(tmb_data_param$data$x_diff), Var2 + Var3 ~ Var1, value.var = "value"))
        colnames(X_data)[1:2] <- c("iso3", "year")
        X_data <- X_data[year >= min_year]
      } else {
        if (verbose) print("Using draws of Xdata")
        X_data <- draw_Xdata[year >= min_year, .SD, .SDcols = c("iso3", "year", dimnames(tmb_data_param$data$x_input)[[1]])]
      }

      setkeyv(X_data, c("iso3", "year"))
    }
  } else {


    ## Prep the xvars
    if (!is.null(specs$xvar)) {
      if (is.null(draw_Xdata)) {
        X_data <- data.table(dcast(melt(tmb_data_param$data$x_full), Var2 + Var3 ~ Var1, value.var = "value"))
        colnames(X_data)[1:2] <- c("iso3", "year")
      } else {
        X_data <- draw_Xdata[year >= min_year, .SD, .SDcols = c("iso3", "year", dimnames(tmb_data_param$data$x_input)[[1]])]
      }

      setkeyv(X_data, c("iso3", "year"))
    }
  }

  ## Sort Y_data
  setkeyv(Y_data, c("iso3", "year"))


  ## If we have X_data, then merge Y_data onto X_data, otherwise expand Y_data
  if (!("X_data" %in% ls())) {
    pred_data <- data.table(expand.grid(iso3 = unique(Y_data$iso3), year = c(min_year:specs$end_FC)))
    pred_data <- merge(pred_data, Y_data, c("iso3", "year"), all.x = T)
  } else {
    pred_data <- merge(X_data, Y_data, c("iso3", "year"), all.x = T)
  }




  ##### Prep params and bind on to the prediction dataset #####

  ### Fixed Effects ###

  ## Add global intercept
  if (specs$a) {
    pred_data[, glob_int := ifelse(specs$a, tmb_params[rn == "a", V1], 0)]
  }


  ## Add covariates
  if (!is.null(specs$xvar) | length(specs$xvar) > 0) {
    pred_data[, paste0("b_", dimnames(tmb_data_param$data$x_input)[[1]]) := lapply(
      c(1:length(tmb_params[rn == "b", V1])),
      function(x) tmb_params[rn == "b", V1][x]
    )]
  }

  ## Add convergence term
  if (specs$c) {
    pred_data[, conv := ifelse(specs$c, tmb_params[rn == "c", V1], 0)]
  }

  ## Add scaled convergence term, along with the scaler
  if (specs$scaled_c) {
    pred_data[, scaled_conv := ifelse(specs$scaled_c, tmb_params[rn == "scaled_c", V1], 0)]
    pred_data[, conv_scaler := tmb_data_param$data$conv_scaler]
  }


  ### Random Effects ###

  ## First, check we have any country specific components to begin with that we'll use for prediction
  ## Note that MAs are just part of the estimation, not needed for prediction
  if (specs$z == T | specs$z_coef == 1 | specs$rho >= 1) {


    ## Get the random intercepts
    if (specs$z) {

      ## Create dataset with the random intercepts and associated iso3 from the y_input matrix
      ## and merge on to pred_data
      pred_data <- merge(pred_data, data.table(iso3 = rownames(tmb_data_param$data$Y_input), z_int = tmb_params[rn == "z", V1]), "iso3", all.x = T)
    }

    ## Get the random coefficients
    if (specs$z_coef == 1) {

      ## Create dataset with the random coefficients and associated iso3 from the y_input matrix
      ## Do this in two steps, since there could be multiple random coeffs

      z_coef_out <- data.table(
        iso3 = rownames(tmb_data_param$data$Y_input),
        z_coef = as.data.table(matrix(tmb_params[rn == "z_coef", V1], ncol = length(tmb_data_param$specifications$re_vars)))
      )
      colnames(z_coef_out) <- c("iso3", paste0("z_", tmb_data_param$specifications$re_vars))

      ## Merge on to pred_data
      pred_data <- merge(pred_data, z_coef_out, "iso3", all.x = T)
    }


    ## Get the AR terms (global or country specific)
    if (specs$rho >= 1) {

      ## If we have constrained the AR terms, then we need to invlogit them ##

      ## If country specific AR, then do this:
      if (specs$ar_mod %in% c(2, 3)) {

        ## Create dataset with the AR and associated iso3 from the y_input matrix
        ## Do this in two steps, since there could be multiple AR terms
        ar_out <- data.table(
          iso3 = rownames(tmb_data_param$data$Y_input),
          arterms = data.table(matrix(tmb_params[rn == "rho_country", V1], ncol = tmb_data_param$data$ar))
        )
        colnames(ar_out) <- c("iso3", paste0("ar_country_", c(1:specs$rho)))

        ## Merge on to pred_data
        pred_data <- merge(pred_data, ar_out, "iso3", all.x = T)

        ## invlogit on constrain
        if (specs$ar_constrain) {
          pred_data[, paste0("ar_country_", c(1:specs$rho)) := lapply(
            paste0("ar_country_", c(1:specs$rho)),
            function(r) invlogit(get(r))
          )]
        }
      }

      ## If global AR, then just get the values and paste them on
      if (specs$ar_mod %in% c(1, 3)) {
        pred_data[, paste0("ar_global_", c(1:specs$rho)) := lapply(c(1:specs$rho), function(r) matrix(tmb_params[rn == "rho_global", V1])[r])]

        ## invlogit on constrain
        if (specs$ar_constrain) {
          pred_data[, paste0("ar_global_", c(1:specs$rho)) := lapply(
            paste0("ar_global_", c(1:specs$rho)),
            function(r) invlogit(get(r))
          )]
        }
      }
    }
  }

  return(pred_data)
}
