

################################################
#### Author: Nafis Sadat
#### Purpose: Gathering all of the TMB functions needed for forecasting
#### Created: 2018/05/11
################################################


suppressMessages(pacman::p_load(TMB, data.table, ggplot2,
                                Matrix, feather, parallel,
                                compiler, argparse))



################################################
## Prepping stuff
################################################


## A nice function to argparse with variable, date and comment

forecast_args_parser <- function() {

  ## Open the parser
  parser <- ArgumentParser()

  ## Specify version and description
  parser$add_argument("--variable", required = T, type = "character", help = "Variable")
  parser$add_argument("--date", required = T, type = "integer", help = "Date")
  parser$add_argument("--comment", required = T, type = "character", help = "Comment")

  args <- parser$parse_args()
  print(args)

  return(list(variable = args$variable, date = args$date, comment = args$comment))
}


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


###### Make data and parameter set
make_data_param <- function(input_data,
                            yvar,
                            xvar, REs,
                            global_int = T,
                            country_int = T, country_int_dist = 1,
                            re_vars = NULL,
                            ar_mod = 2, ar = 0,
                            ma_mod = 2, ma = 0,
                            start_year = 1970, end_fit = 2017, end_FC = 2040,
                            weight_decay = 0.,
                            chaos = T,
                            fd = F,
                            conv = F,
                            scaled_lev_conv = F,
                            ar_constrain = F) {


  ## Convergence cannot be true without first diff
  if (!fd & conv) {
    stop("Convergence term cannot be included without a first difference model")
  }



  ## Prep the dependent variable: cast out and create a matrix (long on country, wide on years)
  Y_input <- dcast(input_data[!is.na(get(yvar)) & year >= start_year & year <= end_fit, .(iso3, year, get(yvar))], iso3 ~ year, value.var = "V3")

  Y_input_rows <- Y_input$iso3
  Y_input_cols <- colnames(Y_input)

  Y_input <- as.matrix(Y_input[, iso3 := NULL])
  colnames(Y_input) <- Y_input_cols
  rownames(Y_input) <- Y_input_rows


  ## Create matrix of diffs of Y
  Y_diff <- Y_input[, 2:ncol(Y_input)] - Y_input[, 1:ncol(Y_input) - 1]

  ## Create lagged Y_input for scaled lev conv
  Y_scaled_conv <- dcast(input_data[!is.na(Y_scaled_conv) & year >= start_year & year <= end_fit, .(iso3, year, Y_scaled_conv)], iso3 ~ year, value.var = "Y_scaled_conv")

  Y_scaled_conv_input_rows <- Y_scaled_conv$iso3
  Y_scaled_conv_input_cols <- colnames(Y_scaled_conv)

  Y_scaled_conv <- as.matrix(Y_scaled_conv[, iso3 := NULL])
  colnames(Y_scaled_conv) <- Y_scaled_conv_input_cols
  rownames(Y_scaled_conv) <- Y_scaled_conv_input_rows

  ## Strip the first entry of the convergence matrix (first time period)
  Y_scaled_conv <- Y_scaled_conv[, 2:ncol(Y_scaled_conv)]

  ## Get the global scalar
  conv_scaler <- input_data$conv_scaler[1]





  ## Create array of covariates
  if (!is.null(xvar)) {
    fe <- 1

    x_input <- reshape2::acast(data.table::melt(input_data[!is.na(get(yvar)) & year >= start_year & year <= end_fit,
      .SD,
      .SDcols = c("iso3", "year", xvar)
    ],
    c("iso3", "year"),
    variable.name = "xvar"
    ), xvar ~ iso3 ~ year)

    ## Create array of full set of covariates (level space)
    x_full <- reshape2::acast(data.table::melt(input_data[ year >= start_year & year <= end_FC,
      .SD,
      .SDcols = c("iso3", "year", xvar)
    ],
    c("iso3", "year"),
    variable.name = "xvar"
    ), xvar ~ iso3 ~ year)

    ## Create array of full set of covariates in diff space for our prediction purposes (all years)
    temp_input_data <- copy(input_data)
    temp_input_data[, c(yvar, xvar) := lapply(c(yvar, xvar), function(x) get(x) - shift(get(x))), "iso3"]

    ### VERY IMPORTANT: if we have time as an FE/RE, then we MUST leave it undifferenced
    if ("year_id" %in% xvar) {
      temp_input_data[is.na(year_id), year_id := 0]
      temp_input_data[, year_id := cumsum(year_id), by = "iso3"]
    }

    x_diff <- reshape2::acast(data.table::melt(temp_input_data[!is.na(get(xvar[1])) & year >= start_year & year <= end_FC,
      .SD,
      .SDcols = c("iso3", "year", xvar)
    ],
    c("iso3", "year"),
    variable.name = "xvar"
    ), xvar ~ iso3 ~ year)
  } else {
    fe <- 0
    x_input <- NA
    x_full <- NA
    x_diff <- NA
  }



  ## Create array of random coefficients
  if (!is.null(re_vars)) {
    re_coef <- 1
    re_coef_input <- reshape2::acast(data.table::melt(input_data[!is.na(get(yvar)) & year >= start_year & year <= end_fit,
      .SD,
      .SDcols = c("iso3", "year", re_vars)
    ],
    c("iso3", "year"),
    variable.name = "re_vars"
    ), re_vars ~ iso3 ~ year)
  } else {
    re_coef_input <- NA
    re_coef <- 0
  }




  ## Create our list of data and params
  data <- list(
    Y_input = Y_input,
    Y_scaled_conv = Y_scaled_conv,
    conv_scaler = conv_scaler,
    x_input = x_input,
    fe = fe,
    re_coef_input = re_coef_input,
    global_int = global_int * 1,
    country_int = country_int * 1,
    country_int_dist = country_int_dist,
    re_coef = re_coef * 1,
    ar_mod = ar_mod,
    ma_mod = ma_mod,
    ar = ar,
    ma = ma,
    fd = fd * 1,
    convergence_term = conv * 1,
    scaled_convergence_term = scaled_lev_conv * 1,
    x_full = x_full,
    x_diff = x_diff,
    Y_diff = Y_diff,
    weight_decay = weight_decay,
    ar_constrain = ar_constrain * 1
  )


  ## Initialize parameters conditional on the model type

  ### NOTE : This parameter order is SUPER important ###

  parameters <- list(
    logSigma = 0,
    b = if (fe == 1) {
      rep(0, dim(data$x_input)[1])
    } else {
      0
    },
    a = if (global_int == T) {
      0
    } else {
      NA
    },

    z = if (country_int == T) {
      rep(0, length(unique(input_data[!is.na(get(yvar)) & year >= start_year & year <= end_fit]$iso3)))
    } else {
      NA
    },

    loggroup = if (country_int_dist >= 1) {
      0
    } else {
      NA
    },

    z_ar1 = if (country_int_dist == 2) {
      0
    } else {
      NA
    },

    z_coef = if (!is.null(re_vars)) {
      matrix(0, nrow = dim(data$re_coef_input)[2], ncol = dim(data$re_coef_input)[1])
    } else {
      NA
    },

    loggrcoef = if (!is.null(re_vars)) {
      rep(0, dim(data$re_coef_input)[1])
    } else {
      NA
    },

    c = if (conv == T) {
      -2
    } else {
      NA
    },

    scaled_c = if (scaled_lev_conv == T) {
      -2
    } else {
      NA
    },

    rho_global = if (ar > 0 & (ar_mod %in% c(1, 3))) {
      rep(0, data$ar)
    } else {
      NA
    },

    rho_country = if (ar > 0 & (ar_mod %in% c(2, 3))) {
      array(0, c(length(unique(input_data[!is.na(get(yvar)) & year >= start_year & year <= end_fit]$iso3)), data$ar))
    } else {
      NA
    },

    logSigma_rho_country = if (ar > 0 & ar_mod == 3) {
      rep(0, data$ar)
    } else {
      NA
    },

    theta_global = if (ma > 0 & (ma_mod %in% c(1, 3))) {
      rep(0, data$ma)
    } else {
      NA
    },

    var_theta_global = if (ma > 0 & (ma_mod %in% c(1, 3))) {
      rep(0, data$ma)
    } else {
      NA
    },

    theta_country = if (ma > 0 & (ma_mod %in% c(2, 3))) {
      array(0, c(length(unique(input_data[!is.na(get(yvar)) & year >= start_year & year <= end_fit]$iso3)), data$ma))
    } else {
      NA
    },

    var_theta_country = if (ma > 0 & (ma_mod %in% c(2, 3))) {
      rep(0, data$ma)
    } else {
      NA
    }
  )

  outlist <- list()
  outlist[["raw_data"]] <- input_data
  outlist[["data"]] <- data
  outlist[["parameters"]] <- parameters
  outlist[["specifications"]] <- list(
    yvar = yvar, xvar = xvar,
    c = conv, scaled_c = scaled_lev_conv,
    a = global_int, z = country_int, z_ran = country_int_dist,
    z_coef = ifelse(is.null(re_vars), F, min(re_vars != "")),
    re_vars = re_vars,
    ar_mod = ar_mod, ma_mod = ma_mod,
    rho = ar, theta = ma, fd = fd,
    start_year = start_year, end_fit = end_fit, end_FC = end_FC, chaos = chaos,
    ar_constrain = ar_constrain
  )
  return(outlist)
}




#################################################
## Modeling functions
#################################################

# ## Run a simple linear model to initialize values
# run_lm_model <- function(data_params) {
#
#   ## Get both level and diff Ys
#   data_use <- data.table(melt(data_params$data$Y_diff))
#   colnames(data_use) <- c('iso3', 'year', 'yvar')
#
#   data_use_levels <- data.table(melt(data_params$data$Y_input))
#   colnames(data_use_levels) <- c('iso3', 'year', 'ylevel')
#
#   ## Check if FD
#   if(data_params$specifications$fd) {
#     if(!is.null(data_params$specifications$xvar)) {
#       x_use <- data.table(melt(data_params$data$x_diff))
#       colnames(x_use) <- c('covars', 'iso3', 'year', 'x')
#       x_use <- dcast(x_use, iso3 + year ~ covars, value.var = 'x')
#       data_use <- merge(data_use, x_use, c('iso3','year'))
#     }
#   } else {
#     if(!is.null(data_params$specifications$xvar)) {
#       x_use <- data.table(melt(data_params$data$x_input))
#       colnames(x_use) <- c('covars', 'iso3', 'year', 'x')
#       x_use <- dcast(x_use, iso3 + year ~ covars, value.var = 'x')
#       data_use_levels <- merge(data_use, x_use, c('iso3','year'))
#     }
#
#   }
#
#   data_use <- merge(data_use, data_use_levels, c('iso3', 'year'))
#   setkeyv(data_use, c('iso3', 'year'))
#
#
#   ## Conv term?
#   if(data_params$specifications$c) {
#     data_use[, convergence_term:= shift(ylevel), by = 'iso3']
#     conv_form <- 'convergence_term'
#   } else {
#     conv_form <- NULL
#   }
#
#   ## Global intercept?
#   if(data_params$specifications$a) {
#     data_use[, intercept:= 1]
#     inter_form <- 'intercept'
#   } else {
#     inter_form <- NULL
#   }
#
#   ## Prepping 'random' effects as static inputs
#   if(data_params$specifications$z) {
#     re_term <- 'factor(iso3)'
#   } else {
#     re_term <- NULL
#   }
#
#   ## Random coefs
#
#
#   ## Only have a single global AR term as initial values
#   if(data_params$specifications$rho > 0) {
#     if(data_params$specifications$fd) {
#       data_use[, paste0('ar_', c(1:data_params$specifications$rho)):= lapply(c(1:data_params$specifications$rho), function(var) shift(yvar, n = var) ), by = 'iso3']
#     } else {
#       data_use[, paste0('ar_', c(1:data_params$specifications$rho)):= lapply(c(1:data_params$specifications$rho), function(var) shift(ylevel, n = var) ), by = 'iso3']
#     }
#     ar_form <- paste0('ar_', data_params$specifications$rho)
#   } else {
#     ar_form <- NULL
#   }
#
#
#   ## Fit model
#   lm_form <- reformulate(termlabels = c(data_params$specifications$xvar, conv_form, ar_form, inter_form, re_term),
#                          response = ifelse(data_params$specifications$fd, 'yvar', 'ylevel'),
#                          intercept = F)
#
#
#   model <- lm(data = data_use, formula = lm_form)
#
#
#
# }


## Run TMB model
run_model <- function(data_params, model, verbose = F) {

  # arguments <- list(...)

  ### Our output list
  output_list <- list()

  ## Create the ADFun
  random_vec <- NULL
  re_input <- na.omit(c(
    ifelse(data_params$data$country_int > 0 & data_params$data$country_int_dist %in% c(0, 1), "z", NA),
    ifelse(data_params$data$country_int > 0 & data_params$data$country_int_dist == 2, "z_ar1", NA),
    ifelse(data_params$data$re_coef > 0, "z_coef", NA),
    # ifelse(data_params$data$ar>0 & data_params$data$ar_mod %in% c(1,3) , 'rho_global', NA),
    ifelse(data_params$data$ar > 0 & data_params$data$ar_mod %in% c(2, 3), "rho_country", NA),
    # ifelse(data_params$data$ma>0 & data_params$data$ma_mod %in% c(1,3) , 'theta_global', NA),
    ifelse(data_params$data$ma > 0 & data_params$data$ma_mod %in% c(2, 3), "theta_country", NA)
  ))
  if (length(re_input) > 0) {
    random_vec <- re_input
  }

  ## Finally, find the NAs in data_params$parameters, and MAP them out
  map_var <- names(data_params$parameters)[is.na(data_params$parameters)]
  map_var_list <- lapply(map_var, function(x) return(factor(NA)))
  names(map_var_list) <- map_var



  #### Create the objective function as a member of output_list
  output_list[["obj"]] <- MakeADFun(data_params$data,
    data_params$parameters,
    hessian = T,
    DLL = model,
    silent = !verbose,
    random = random_vec,
    checkParameterOrder = T,
    last.par = T
  )

  runSymbolicAnalysis(output_list[["obj"]])

  ## Optimize (put this in a try block)
  output_list[["opt"]] <- nlminb(
    objective = output_list[["obj"]]$fn,
    gradient = output_list[["obj"]]$gr,
    start = output_list[["obj"]]$par
  )



  ### The parameter vector in order of the joint precision can be extracted from obj:
  output_list[["mean_params"]] <- output_list[["obj"]]$env$last.par.best


  ## Get the SEs of the estimates
  output_list[["FC_model"]] <- sdreport(output_list[["obj"]], getJointPrecision = T, getReportCovariance = T, bias.correct = F)




  ### Get the variance-covariance matrix by inverting the joint Precision
  vcov_mat <- Matrix::solve(output_list[["FC_model"]]$jointPrecision)
  rownames(vcov_mat) <- colnames(vcov_mat) <- colnames(output_list[["FC_model"]]$jointPrecision)

  ## Assert that the name of mean_params and vcov_mat and identical (dimension matching test)
  testthat::expect_identical(colnames(vcov_mat), names(output_list[["mean_params"]]))

  ## Finally, record the innards of the object : Y_hat and resid
  output_list[["Y_hat"]] <- output_list[["obj"]]$report()$Y_hat
  output_list[["resid"]] <- output_list[["obj"]]$report()$resid

  # conv <<- 1
  return(output_list)
}



### Prepping the data for forecasting

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



################################################
## Forecasting
################################################

## Prep our input (X) data for draws
prepping_X_for_draw <- function(X_diff, X_level, tmb_data_param = data_params, d) {
  if (!is.null(tmb_data_param$specifications$xvar)) {
    if ("draw" %in% names(X_diff)) {
      if (tmb_data_param$specifications$fd) {
        return(X_diff[draw == d])
      } else {
        return(X_level[draw == d])
      }
    } else {
      if (tmb_data_param$specifications$fd) {
        return(X_diff[, draw := d])
      } else {
        return(X_level[, draw := d])
      }
    }
  } else {
    return(NULL)
  }
}



## Function to forecast
make_forecast <- function(mean_est_data_obj, tmb_output_object, tmb_data_param, add_residual = T, transform = NULL, ...) {

  # mean_est_data_obj = copy(mean_est_data)
  # tmb_output_object = copy(output_TMB)
  # tmb_data_param = copy(data_params)

  ## Extra args
  argss <- list(...)


  ## Get specifications
  specs <- tmb_data_param$specifications


  ## If specs$scaled_c, then we gotta have a back transform:
  if (specs$scaled_c & is.null(transform)) {
    stop("Scaled convergence needs a back transformer")
  }


  ## Loop over each year of forecas
  j <- 0
  for (yeer in c(eval(specs$end_fit):specs$end_FC)) {
    j <- j + 1

    # print(paste0('Predicting year: ', yeer))


    ## Fit all non time variant stuff

    ## global intercept
    mean_est_data_obj[year %in% yeer, pred := glob_int]

    ## Fixed effects
    if (!is.null(specs$xvar)) {
      mean_est_data_obj[year %in% yeer, pred := pred +
        eval(parse(text = paste0("b_", paste0(tmb_data_param$specifications$xvar, "*", tmb_data_param$specifications$xvar), collapse = "+")))]
    }

    ## Add random intercepts
    if (specs$z) {
      mean_est_data_obj[year %in% yeer, pred := pred + z_int]
    }

    ## Add random coefficients
    if (specs$z_coef == 1) {
      mean_est_data_obj[year %in% yeer, pred := pred +
        eval(parse(text = paste0("z_", paste0(tmb_data_param$specifications$re_vars, "*", tmb_data_param$specifications$re_vars), collapse = "+")))]
    }

    ## Add time variant stuff

    ## Make temp lag-level variable
    mean_est_data_obj[year %in% c(yeer - 1, yeer), temp_lev := shift(yvar), by = "iso3"]

    ## Make temp lag-diff variable
    mean_est_data_obj[year %in% c(yeer - 1, yeer), temp_diff := shift(ydiff), by = "iso3"]


    ## Add convergence term
    if (specs$c) {
      mean_est_data_obj[year %in% yeer, pred := pred + conv * temp_lev, by = "iso3"]
    }

    ## Add scaled convergence term
    if (specs$scaled_c) {
      mean_est_data_obj[year %in% yeer, pred := pred + scaled_conv * get(transform)(temp_lev) / conv_scaler, by = "iso3"]
    }


    ## Add AR terms (global or country)
    if (specs$rho >= 1) {
      if (specs$fd) {
        if (specs$ar_mod %in% c(1, 3)) {
          mean_est_data_obj[year %in% yeer, pred := pred + eval(parse(text = paste(paste0("ar_global_", c(1:specs$rho)), "temp_diff", sep = " * "))), by = "iso3"]
        }
        if (specs$ar_mod %in% c(2, 3)) {
          mean_est_data_obj[year %in% yeer, pred := pred + eval(parse(text = paste(paste0("ar_country_", c(1:specs$rho)), "temp_diff", sep = " * "))), by = "iso3"]
        }
      } else {
        if (specs$ar_mod %in% c(1, 3)) {
          mean_est_data_obj[year %in% yeer, pred := pred + eval(parse(text = paste(paste0("ar_global_", c(1:specs$rho)), "temp_lev", sep = " * "))), by = "iso3"]
        }
        if (specs$ar_mod %in% c(2, 3)) {
          mean_est_data_obj[year %in% yeer, pred := pred + eval(parse(text = paste(paste0("ar_country_", c(1:specs$rho)), "temp_lev", sep = " * "))), by = "iso3"]
        }
      }
    }

    ##### End of single year prediction #####



    ### Intercept shifting if we have level space values ###
    ### We will capture the difference in levels in the last year of observed data
    ### which will add on to all of the level projections as intercept shift


    if (j == 1) {
      if (specs$fd == F) {
        ## Get the intercept shift difference
        mean_est_data_obj[, int_shift := mean(yvar - pred, na.rm = T), "iso3"]
      }
      mean_est_data_obj[, c("pred", "temp_lev", "temp_diff") := NULL]
    } else {
      ## Update yvar and ydiff according to the model space we used
      if (specs$fd) {
        mean_est_data_obj[year %in% yeer, ydiff := pred]
        mean_est_data_obj[year %in% yeer, yvar := pred + temp_lev]
      } else {

        ## Int shift
        mean_est_data_obj[year %in% yeer, yvar := pred + int_shift]
        mean_est_data_obj[year %in% yeer, ydiff := yvar - temp_lev]
      }

      mean_est_data_obj[, c("temp_lev", "temp_diff") := NULL]
    }
  }

  ## Simulate errors
  if (add_residual) {

    # print("Adding residuals")

    if (!("var_metric" %in% names(argss))) argss$var_metric <- "mad"
    if (!("cap_variance" %in% names(argss))) argss$cap_variance <- F
    if (!("var_threshold" %in% names(argss))) argss$var_threshold <- NULL

    ## Simulate errors
    rw_errors <- simulate_rw_forecast(
      tmb_output_object = tmb_output_object, tmb_data_params = tmb_data_param,
      var_metric = argss$var_metric, cap_variance = argss$cap_variance, var_threshold = argss$var_threshold
    )

    ## Merge on data
    mean_est_data_obj <- merge(mean_est_data_obj, rw_errors, c("iso3", "year"), all.x = T)
    mean_est_data_obj[is.na(rw), rw := 0]

    ## Add to levels
    mean_est_data_obj[, yvar := yvar + rw]

    ## Remake ydiff
    mean_est_data_obj[, ydiff := yvar - shift(yvar), by = "iso3"]
  }



  return(mean_est_data_obj[, .(iso3, year, yvar, ydiff)])
}



#### Simulate Residuals

simulate_rw_forecast <- function(tmb_output_object, tmb_data_params, var_metric = "mad", cap_variance = T, var_threshold = NULL) {

  # tmb_output_object = copy(output_TMB)
  # tmb_data_params = copy(data_params)

  ## Get the Y_hat from the TMB output (in-sample fit)
  y_output <- tmb_output_object$Y_hat

  ## Trim y-output to remove if we have AR/MA terms (first years of preds are naughty)
  y_output <- y_output[, eval(max(tmb_data_params$data$ma, tmb_data_params$data$ar) + 1):ncol(y_output)]


  ## If we are in first-diff space, then we can just get the SD of the first diff fits by country
  if (tmb_data_params$specifications$fd) {

    ## Nothing to do here
  } else {

    ## Get first diffs of y_output
    y_output <- y_output[, 2:ncol(y_output)] - y_output[, 1:eval(ncol(y_output) - 1)]
  }

  ## Get the variance measure of the y-diffs
  if (var_metric == "sd") {
    noise_diffs <- apply(y_output, 1, sd)
  }
  if (var_metric == "mad") {
    noise_diffs <- apply(y_output, 1, mad)
  }



  ## Create a data.table with country and standard diffs
  rw_mat <- data.table(iso3 = rownames(tmb_data_params$data$Y_input), noise = noise_diffs)

  ## Cap noise if needed
  if (cap_variance) {
    if (is.null(var_threshold)) {
      rw_mat[, le_cap := (qnorm(0.9) / .6745) * mad(noise) + median(noise)]
      rw_mat[noise > le_cap, noise := le_cap]
      rw_mat[, le_cap := NULL]
    } else {
      rw_mat[noise > var_threshold, noise := var_threshold]
    }
  }

  ## Create first column of zeroes (end_fit value)
  rw_mat[, paste0("yr", tmb_data_params$specifications$end_fit) := 0]

  ## Simulate normal distribution with 0 mean and sd of sd
  yr_vect <- eval(tmb_data_params$specifications$end_fit + 1):tmb_data_params$specifications$end_FC
  rw_mat[, paste0("yr", yr_vect) := lapply(yr_vect, function(x) rnorm(.N, 0, noise))]

  rw_mat[, noise := NULL]

  ## Melt long on yr stub
  rw_mat <- melt(rw_mat, "iso3", value.name = "rw", measure.vars = patterns("^yr"), variable.name = "year")
  rw_mat[, year := as.numeric(gsub("yr", "", year))]
  rw_mat[, rw := cumsum(rw), by = "iso3"]
  rw_mat <- rw_mat[year >= yr_vect[1]]

  return(rw_mat)
}







#' Take multivariate normal draws given a mean vector and precision matrix
#'
#' @param mu vector of parameter means
#' @param prec joint precision matrix
#' @param n.sims number of draws
#' @param ncores number of cores to thread on
#'
#' @return n.sims by length(mu) matrix of parameter draws
#'
rmvnorm_prec <- function(mu = NULL, prec, n.sims) {
  if (is.null(mu)) {
    mu <- rep(0, dim(prec)[1])
  }
  z <- matrix(MASS::mvrnorm(length(mu) * n.sims, 0, 1), ncol = n.sims)
  L <- Matrix::Cholesky(prec, super = TRUE)
  z <- Matrix::solve(L, z, system = "Lt") ## z = Lt^-1 %*% z
  z <- Matrix::solve(L, z, system = "Pt") ## z = Pt    %*% z
  z <- as.matrix(z)
  outmat <- t(mu + z)
  colnames(outmat) <- colnames(prec)
  return(outmat)
}


### This function will: multiply or divvy by a column from raw data with yvar,
### and regenerate ydiff (helpful for changing denominators)
change_units_of_draws <- function(draws, idvar, input_data, column, op = c("add", "subtract", "multiply", "divide")) {
  if (length(op) != 1) {
    stop("Choose one operation")
  }
  if (!(op %in% c("add", "subtract", "multiply", "divide"))) {
    stop("Choose one op of c('add', 'subtract', 'multiply', 'divide')")
  }

  ## Merge draws with input_data
  draws <- merge(draws, input_data[, .SD, .SDcols = c(idvar, column)], idvar, all.x = T)

  ## Apply the op
  if (op == "add") {
    draws[, yvar := yvar + get(paste0(column))]
  }
  if (op == "subtract") {
    draws[, yvar := yvar - get(paste0(column))]
  }
  if (op == "multiply") {
    draws[, yvar := yvar * get(paste0(column))]
  }
  if (op == "divide") {
    draws[, yvar := yvar / get(paste0(column))]
  }

  ## Remake ydiff
  draws[, ydiff := yvar - shift(yvar), by = c(idvar[1], "draw") ]
  draws[, (column) := NULL]

  return(draws)
}


## Take draws object, cast wide on draws, and tag model number
prep_draw_output <- function(draws, model) {
  return(dcast(draws, iso3 + year ~ draw, value.var = c("yvar", "ydiff"))[, model := model])
}


############################################################
### Testing Inclusion Criteria
############################################################


### Coefficient significance
test_sig <- compiler::cmpfun(function(tmb_output_object, alpha = 0.1, intercept_test = T, rigorous = F) {

  # tmb_output_object = copy(output_TMB); alpha=0.1

  ## Get critical region value (2 sided)
  crit_value <- abs(qnorm(p = alpha / 2))


  ## Get the data.table with p-values from SDReport object
  pval_DT <- data.table(summary(tmb_output_object$FC_model, p.value = T), keep.rownames = T)
  colnames(pval_DT) <- c("param", "est", "se", "zval", "pval")

  ## Note: We don't need to test every single parameter's stat sig, so filter those out first

  #### BUT: If we are doing rigorous tests, then we should test everything except the country random effect estimates
  if (!rigorous) {
    pval_DT <- pval_DT[param %in% c(
      "b", "a", "logSigma", "logggroup", "loggrcoef", "c", "rho_global",
      "logSigma_rho_country", "theta_global", "var_theta_global", "var_theta_country"
    )]
  } else {
    pval_DT <- pval_DT[!(param %in% c("z", "z_coef"))]
  }


  ## Create criteria column
  pval_DT[, pass_sig_test := pval < 0.1 ]

  stat_sig <- 1

  ## If there are less number of TRUEs than the number of parameters, return 0
  if (sum(pval_DT$pass_sig_test) != nrow(pval_DT)) {
    stat_sig <- 0
  }

  print(pval_DT)
  return(stat_sig)
})


### Compute the empirically derived upper and lower bounds
sfa_bounds <- function(data, depvar, transform, panelvar, yearvar = "year", plot = F) {

  ## Copy out and get YoY percent changes
  sfa_prep <- copy(data)

  if (transform == "log") {
    sfa_prep[, yoy_pc := 100 * (shift(exp(get(depvar)), type = "lead") - (exp(get(depvar)))) / ((exp(get(depvar)))), panelvar]
  }
  if (transform == "logit") {
    sfa_prep[, yoy_pc := 100 * (shift(arm::invlogit(get(depvar)), type = "lead") - (arm::invlogit(get(depvar)))) / ((arm::invlogit(get(depvar)))), panelvar]
  }
  if (transform == "logit_trans") {
    sfa_prep[, yoy_pc := 100 * (shift(inv_logit_trans(get(depvar)), type = "lead") - (inv_logit_trans(get(depvar)))) / ((inv_logit_trans(get(depvar)))), panelvar]
  }
  if (transform == "custom") {
    sfa_prep[, yoy_pc := 100 * (shift(custom_inv_fn(get(depvar)), type = "lead") - (custom_inv_fn(get(depvar)))) / ((custom_inv_fn(get(depvar)))), panelvar]
  }

  sfa_prep <- sfa_prep[yoy_pc < 100 & yoy_pc > -100  ]

  ## Split the obs into positive and negative pc changes and take the log
  sfa_prep[yoy_pc > 0, log_pc := log(yoy_pc)]
  sfa_prep[yoy_pc < 0, neg_log_pc := log(abs(yoy_pc))]


  ## Outlier stuff
  sfa_prep <- sfa_prep[(log_pc > -6 & log_pc < 6) | is.na(log_pc)]
  sfa_prep <- sfa_prep[(neg_log_pc > -6 & neg_log_pc < 6) | is.na(neg_log_pc)]

  ## Separate out the neg and pos DTs
  sfa_neg <- sfa_prep[!is.na(neg_log_pc) & !is.na(depvar), .SD, .SDcols = c(panelvar, yearvar, "yoy_pc", depvar, "neg_log_pc")]
  sfa_pos <- sfa_prep[!is.na(log_pc) & !is.na(depvar), .SD, .SDcols = c(panelvar, yearvar, "yoy_pc", depvar, "log_pc")]

  ## Run SFA on each of the bounds
  sfa_model_pos <- frontier::sfa(data = sfa_pos, formula = log_pc ~ 1 + get(depvar))
  sfa_model_neg <- frontier::sfa(data = sfa_neg, formula = neg_log_pc ~ 1 + get(depvar))

  ## Extract the coefficients and fitted values
  sfa_pos[, intercept := coef(sfa_model_pos)[1]]
  sfa_pos[, upper_beta := coef(sfa_model_pos)[2]]
  sfa_pos[, upper_frontier := exp(fitted(sfa_model_pos))]

  sfa_neg[, intercept := coef(sfa_model_neg)[1]]
  sfa_neg[, lower_beta := coef(sfa_model_neg)[2]]
  sfa_neg[, lower_frontier := -exp(fitted(sfa_model_neg))]

  ## Order by x
  sfa_neg <- sfa_neg[order(get(depvar))]
  sfa_pos <- sfa_pos[order(get(depvar))]

  ## Merge on with sfa_prep
  sfa_prep <- merge(sfa_prep, sfa_pos[, .SD, .SDcols = c(panelvar, yearvar, depvar, "log_pc")], c(panelvar, yearvar), all.x = T)
  sfa_prep <- merge(sfa_prep, sfa_neg[, .SD, .SDcols = c(panelvar, yearvar, depvar, "neg_log_pc")], c(panelvar, yearvar), all.x = T)

  if (plot) {

    ## Make a plot we all like
    if (transform == "logit") {
      plot(ggplot2::ggplot() + geom_point(data = sfa_neg, aes(x = arm::invlogit(get(depvar)), y = yoy_pc)) + geom_line(data = sfa_neg, aes(x = arm::invlogit(get(depvar)), y = lower_frontier), color = "red") +
        geom_point(data = sfa_pos, aes(x = arm::invlogit(get(depvar)), y = yoy_pc)) + geom_line(data = sfa_pos, aes(x = arm::invlogit(get(depvar)), y = upper_frontier), color = "red"))
    }
    if (transform == "logit_trans") {
      plot(ggplot2::ggplot() + geom_point(data = sfa_neg, aes(x = inv_logit_trans(get(depvar)), y = yoy_pc)) + geom_line(data = sfa_neg, aes(x = inv_logit_trans(get(depvar)), y = lower_frontier), color = "red") +
        geom_point(data = sfa_pos, aes(x = inv_logit_trans(get(depvar)), y = yoy_pc)) + geom_line(data = sfa_pos, aes(x = inv_logit_trans(get(depvar)), y = upper_frontier), color = "red"))
    }

    if (transform == "log") {
      plot(ggplot2::ggplot() + geom_point(data = sfa_neg, aes(x = exp(get(depvar)), y = yoy_pc)) +
        geom_line(data = sfa_neg, aes(x = exp(get(depvar)), y = lower_frontier), color = "red") +
        geom_point(data = sfa_pos, aes(x = exp(get(depvar)), y = yoy_pc)) + geom_line(data = sfa_pos, aes(x = exp(get(depvar)), y = upper_frontier), color = "red"))
    }

    if (transform == "custom") {
      plot(ggplot2::ggplot() + geom_point(data = sfa_neg, aes(x = custom_inv_fn(get(depvar)), y = yoy_pc)) + geom_line(data = sfa_neg, aes(x = custom_inv_fn(get(depvar)), y = lower_frontier), color = "red") +
        geom_point(data = sfa_pos, aes(x = custom_inv_fn(get(depvar)), y = yoy_pc)) + geom_line(data = sfa_pos, aes(x = custom_inv_fn(get(depvar)), y = upper_frontier), color = "red"))
    }
  }

  return_list <- list()
  return_list[["upper_intercept"]] <- coef(sfa_model_pos)[1]
  return_list[["upper_beta"]] <- coef(sfa_model_pos)[2]
  return_list[["lower_intercept"]] <- coef(sfa_model_neg)[1]
  return_list[["lower_beta"]] <- coef(sfa_model_neg)[2]

  return(return_list)
}



### Percent change bounds
test_bounds <- compiler::cmpfun(function(data, depvar, predict_years, rev_trans, panelvar, yearvar, sfa_output, threshold = 0) {

  ## Compute year over year percent change
  data_pc <- copy(data)

  data_pc[, upper_bound := exp(sfa_output$upper_beta * (get(paste0(depvar))) + sfa_output$upper_intercept) ]
  data_pc[, lower_bound := -exp(sfa_output$lower_beta * (get(paste0(depvar))) + sfa_output$lower_intercept) ]

  # data_pc[, pc := (shift(log(get(depvar_in_levels)), 1, type = "lead") - log(get(depvar_in_levels)))*100/log(get(depvar_in_levels)), panelvar ]

  data_pc[, pc := (shift(eval(parse(text = paste0(rev_trans, "(", depvar, ")"))), 1, type = "lead") - (eval(parse(text = paste0(rev_trans, "(", depvar, ")"))))) * 100 / eval(parse(text = paste0(rev_trans, "(", depvar, ")"))), panelvar]

  ### Keep if the percent change is within the bound
  data_pc <- data_pc[get(yearvar) %in% c(eval(predict_years[1]) + 1:predict_years[2])]
  data_pc[pc > upper_bound & pc > 0, sfa_up := 1]
  data_pc[pc < lower_bound & pc < 0, sfa_low := 1]

  bounds <- 1
  print(sum_up <- sum(data_pc[, sfa_up], na.rm = T))
  print(sum_low <- sum(data_pc[, sfa_low], na.rm = T))

  if (sum_up > threshold | sum_low > threshold) {
    bounds <- 0
  }

  return(bounds)
})



############################################################
### OOS and residual functions
############################################################


## Making residuals from TMB output
create_residuals <- function(tmb_output_obj, tmb_data_param) {

  # tmb_output_obj = copy(output_TMB)
  # tmb_data_param = copy(data_params)

  ## Get the Y-hats from the TMB run
  yhat <- tmb_output_obj$obj$report()$Y_hat

  ## Get the y-input from the data_params (level space)
  yinput <- tmb_data_param$data$Y_input

  ## Get the residuals from the TMB
  resid <- tmb_output_obj$obj$report()$resid




  #### Trimming

  ## If we have AR/MA terms, then we will drop the first few columns
  arma_drop <- max(tmb_data_param$specifications$ar, tmb_data_param$specifications$ma)

  ## Drop them from both matrices
  yhat <- yhat[, c(eval(arma_drop + 1):ncol(yhat))]
  yinput <- yinput[, c(eval(arma_drop + 1):ncol(yinput))]
  resid <- resid[, c(eval(arma_drop + 1):ncol(resid))]



  #### FD models

  ## If we evaluated first difference models, then assert that the yhat is one less col than yinput
  if (tmb_data_param$specifications$fd) {
    if (ncol(yhat) != ncol(yinput) - 1) {
      stop("Something wrong with Y dimensionaliteh")
    }


    ## Melt matrices into DT
    yinput_dt <- data.table(melt(yinput))
    colnames(yinput_dt) <- c("iso3_str", "year_str", "truth")
    yinput_dt[, year := as.numeric(factor(year_str))]
    yinput_dt[, iso3 := as.numeric(factor(iso3_str))]



    yhat_dt <- data.table(melt(yhat))
    colnames(yhat_dt) <- c("iso3", "year", "pred")

    ## If first diff, then add plus year to years for yhat_dt to account for year gap
    if (tmb_data_param$specifications$fd) {
      yhat_dt[, year := year + 1]
    }


    ## Merge dem
    dt_merged <- merge(yhat_dt, yinput_dt, c("iso3", "year"), all.y = T)

    ## Cumsum to levs
    dt_merged[is.na(pred), pred := truth]
    dt_merged[, pred2 := cumsum(pred), by = c("iso3")]


    ## Fix resid row and colnames
    colnames(resid) <- colnames(yinput)[2:ncol(yinput)]
    rownames(resid) <- rownames(yinput)


    ## Bring in diff space resids as well
    resid_tmb <- data.table(melt(resid))
    colnames(resid_tmb) <- c("iso3", "year", "diff_resid")

    dt_merged <- dt_merged[, .(iso3 = iso3_str, year = year_str, level_resid = truth - pred2)]
    dt_merged <- merge(dt_merged, resid_tmb, c("iso3", "year"), all.x = T)
  } else {

    ## Just make the Y-hat pretty and return
    ## Copy row and column names of Y-input into yhat
    colnames(yhat) <- colnames(yinput)
    rownames(yhat) <- rownames(yinput)

    ## Melt and out yhat
    dt_yhat <- data.table(melt(yhat))
    colnames(dt_yhat) <- c("iso3", "year", "pred")

    ## Melt and out yinput
    dt_input <- data.table(melt(yinput))
    colnames(dt_input) <- c("iso3", "year", "truth")

    ## Merge the two
    dt_merged <- merge(dt_yhat, dt_input, c("iso3", "year"))
    dt_merged[, level_resid := truth - pred]

    ## Make diff resids by converting pred and truth to diff space, and then taking residuals
    dt_merged[, diff_resid := (truth - shift(truth)) - (pred - shift(pred)), by = "iso3"]
  }
  setkeyv(dt_merged, c("iso3", "year"))



  return(dt_merged[, .(iso3, year, level_resid, diff_resid)])
}




#### OOS generator for NON CHAOS
RMSE_generator <- function(mean_data, OOS_data, panelvar, yearvar,
                           depvar_dt, oos_start, in_sample_end,
                           region_metrics = F, region_data = NA) {

  ## Merge in the two datasets we'll compare values from (depending on which space we compute the RMSE in)
  d1 <- copy(mean_data)[get(yearvar) <= in_sample_end, .SD, .SDcols = c(panelvar, yearvar, depvar_dt)]
  colnames(d1)[3] <- "truth"
  d2 <- copy(OOS_data)[, .SD, .SDcols = c(panelvar, yearvar, depvar_dt) ]
  colnames(d2)[3] <- "OOS"
  RMSE_data <- merge(d1, d2, c(panelvar, yearvar), all = T)


  ### Make sure that region_metrics has the right column names to use
  if (!is.na(region_metrics)) {
    if (sum(c(panelvar, "region_name", "super_region_name") %in% colnames(region_data)) != 3) {
      stop("Region metrics need a properly names region_data data.table")
    } else {

      ## Merge in region_data with RMSE data
      RMSE_data <- merge(RMSE_data, region_data[, .SD, .SDcols = c(panelvar, "region_name", "super_region_name")], panelvar, all.x = T)
    }
  }


  ## Compute RMSE
  RMSE_data <- RMSE_data[get(yearvar) >= oos_start]
  RMSE_data[, error_sq := (truth - OOS)**2]


  ## Get CS and global RMSEs
  RMSE_data[, rmse_iso := sqrt(mean(error_sq)), panelvar]
  RMSE_data[, rmse_global := sqrt(mean(error_sq, na.rm = T)), ]



  ##### If we're gonna get region metric:
  if (region_metrics) {
    RMSE_data[, rmse_REG := sqrt(mean(error_sq, na.rm = T)), "region_name"]
    RMSE_data[, rmse_SRREG := sqrt(mean(error_sq, na.rm = T)), "super_region_name"]
  }


  ## Keep unique
  RMSE_data <- RMSE_data[get(yearvar) == oos_start]




  # Prep output
  if (region_metrics) {
    RMSE_data <- RMSE_data[, .SD, .SDcols = c(panelvar, "rmse_iso", "rmse_REG", "rmse_SRREG", "rmse_global")]
  } else {
    RMSE_data <- RMSE_data[, .SD, .SDcols = c(panelvar, "rmse_iso", "rmse_global")]
  }


  return(RMSE_data)
}







#### OOS generator for CHAOS
RMSE_generator_2 <- function(mean_data, OOS_data, panelvar, yearvar,
                             depvar_dt, oos_start, in_sample_end,
                             region_metrics = F, region_data = NA) {

  ## Merge in the two datasets we'll compare values from (depending on which space we compute the RMSE in)
  d1 <- copy(mean_data)[get(yearvar) <= in_sample_end, .SD, .SDcols = c(panelvar, yearvar, depvar_dt)]
  colnames(d1)[3] <- "truth"
  d2 <- copy(OOS_data)[, .SD, .SDcols = c(panelvar, yearvar, depvar_dt) ]
  colnames(d2)[3] <- "OOS"
  RMSE_data <- merge(d1, d2, c(panelvar, yearvar), all = T)


  ### Make sure that region_metrics has the right column names to use
  if (!is.na(region_metrics)) {
    if (sum(c(panelvar, "region_name", "super_region_name") %in% colnames(region_data)) != 3) {
      stop("Region metrics need a properly names region_data data.table")
    } else {

      ## Merge in region_data with RMSE data
      RMSE_data <- merge(RMSE_data, region_data[, .SD, .SDcols = c(panelvar, "region_name", "super_region_name")], panelvar, all.x = T)
    }
  }


  ## Compute RMSE
  RMSE_data <- RMSE_data[get(yearvar) >= oos_start]
  RMSE_data[, error_sq := (truth - OOS)**2]


  ## With this scheme:
  ###### OOS at year 1 will be based on 1st 2 years of forecasts;
  ###### OOS at year 2 will be based on 2/3/4th years of forecasts;
  ###### OOS at year 3 will be based on 3/4/5th years of forecasts;
  ## ... OOS at year T-1 will be based on T-2/T-1/T years of forecasts;
  ## ... OOS at year T will be the same values as of T-1;

  ## Start by getting the length of OOS years first
  oos_vector <- c(eval(oos_start):in_sample_end)
  length_of_OOS_yrs <- length(oos_vector)

  ## Get a neighboring vector
  oo <- 0
  for (i in oos_vector) {
    oo <- oo + 1

    if (oo == 1) {
      RMSE_data[get(yearvar) == i | get(yearvar) == eval(i + 1), paste0("rmse_", i) := sqrt(mean(error_sq)), panelvar]
      RMSE_data[get(yearvar) == i | get(yearvar) == eval(i + 1), paste0("rmse_global_", i) := sqrt(mean(error_sq, na.rm = T))]
    }

    if (oo > 1 & oo < length_of_OOS_yrs) {
      RMSE_data[get(yearvar) >= i - 1 & get(yearvar) <= eval(i + 1), paste0("rmse_", i) := sqrt(mean(error_sq)), panelvar]
      RMSE_data[get(yearvar) >= i - 1 & get(yearvar) <= eval(i + 1), paste0("rmse_global_", i) := sqrt(mean(error_sq, na.rm = T)), ]
    }

    if (oo == length_of_OOS_yrs) {
      RMSE_data[get(yearvar) >= i - 2, paste0("rmse_", i) := sqrt(mean(error_sq)), panelvar]
      RMSE_data[get(yearvar) >= i - 2, paste0("rmse_global_", i) := sqrt(mean(error_sq, na.rm = T)), ]
    }
  }


  ##### If we're gonna get region metric:
  if (region_metrics) {
    ## Get a neighboring vector
    oo <- 0
    for (i in oos_vector) {
      oo <- oo + 1

      if (oo == 1) {
        RMSE_data[get(yearvar) == i | get(yearvar) == eval(i + 1), paste0("rmse_REG_", i) := sqrt(mean(error_sq, na.rm = T)), "region_name"]
        RMSE_data[get(yearvar) == i | get(yearvar) == eval(i + 1), paste0("rmse_SRREG_", i) := sqrt(mean(error_sq, na.rm = T)), "super_region_name"]
      }

      if (oo > 1 & oo < length_of_OOS_yrs) {
        RMSE_data[get(yearvar) >= i - 1 & get(yearvar) <= eval(i + 1), paste0("rmse_REG_", i) := sqrt(mean(error_sq, na.rm = T)), "region_name"]
        RMSE_data[get(yearvar) >= i - 1 & get(yearvar) <= eval(i + 1), paste0("rmse_SRREG_", i) := sqrt(mean(error_sq, na.rm = T)), "super_region_name"]
      }

      if (oo == length_of_OOS_yrs) {
        RMSE_data[get(yearvar) >= i - 2, paste0("rmse_REG_", i) := sqrt(mean(error_sq, na.rm = T)), "region_name"]
        RMSE_data[get(yearvar) >= i - 2, paste0("rmse_SRREG_", i) := sqrt(mean(error_sq, na.rm = T)), "super_region_name"]
      }
    }
  }

  # Get a single column of RMSE

  for (i in oos_vector) {
    RMSE_data[get(yearvar) == i, rmse_iso := get(paste0("rmse_", i))]
    RMSE_data[get(yearvar) == i, rmse_global := get(paste0("rmse_global_", i))]

    if (region_metrics) {
      RMSE_data[get(yearvar) == i, rmse_REG := get(paste0("rmse_REG_", i))]
      RMSE_data[get(yearvar) == i, rmse_SRREG := get(paste0("rmse_SRREG_", i))]
    }
  }

  if (region_metrics) {
    RMSE_data <- RMSE_data[, .SD, .SDcols = c(panelvar, yearvar, "rmse_iso", "rmse_REG", "rmse_SRREG", "rmse_global")]
  } else {
    RMSE_data <- RMSE_data[, .SD, .SDcols = c(panelvar, yearvar, "rmse_iso", "rmse_global")]
  }


  ## Get OOS and model number indicator
  RMSE_data[, oos := get(yearvar) - oos_start + 1]


  return(RMSE_data)
}

## Creating bunch of stuff with the residuals (MAD, median, mean, SD)
create_stats <- function(data, panelvar, variable) {
  data_out <- copy(data)

  data_out <- data_out[!is.na(get(variable))]

  ## Median
  data_out[, median := median(get(variable), na.rm = T), panelvar ]

  ## MAD
  data_out[, mad := mad(get(variable), na.rm = T), panelvar ]

  ## Mean
  data_out[, mean := mean(get(variable), na.rm = T), panelvar ]

  ## SD
  data_out[, sd := sd(get(variable), na.rm = T), panelvar ]

  data_out <- data_out[, .SD, .SDcols = c(panelvar, "mean", "sd", "median", "mad")]

  return(unique(data_out, by = panelvar))
}


### Stat making function across draws
stat_maker <- cmpfun(function(data, melt = F, merge = T, idvar = c("iso3", "year")) {

  ## Melt
  if (melt == T) {
    ## Melt super long
    print("Melting data")
    data2 <- melt(data, idvar, value.name = "data", variable.name = "draws")
  } else {
    data2 <- data
  }



  ## Get stats
  print("Make stats")
  data_stats <- data2[, as.list(c(
    mean(data),
    quantile(data, c(0.025, 0.975))
  )), by = idvar]

  colnames(data_stats) <- c(idvar, "mean", "lower", "upper")
  setkeyv(data_stats, idvar)

  if (merge) {
    ## Merge with data
    print("Merge with data")
    outdata <- merge(data_stats, data, idvar)
    setkeyv(outdata, idvar)
    return(outdata)
  }

  else {
    return(data_stats)
  }
})


### Create fun summary stuff
postfile_maker <- function(tmb_output_obj, tmb_data_param, model_number) {

  # tmb_output_obj = copy(output_TMB)
  # tmb_data_param = copy(data_params)


  ## Model number
  postfile_df <- data.table(model_number = model_number)


  ## Extract fixed effects from the model
  postfile_df <- data.table(model_number = model_number)

  ## Add AR term
  postfile_df[, ar := tmb_data_param$data$ar]

  ## AR mod
  postfile_df[, ar_mod := tmb_data_param$specifications$ar_mod]

  ## Add MA term
  postfile_df[, ma := tmb_data_param$data$ma]

  ## MA mod
  postfile_df[, ma_mod := tmb_data_param$specifications$ma_mod]

  ## FD?
  postfile_df[, fdiff := tmb_data_param$specifications$fd]

  ## AR constrain?
  postfile_df[, ar_constrain := tmb_data_param$specifications$ar_constrain]

  ## Convergence term
  if (tmb_data_param$specifications$c) {
    postfile_df[, conv := tmb_output_obj$obj$report()$c]
  }

  ## Scaled convergence term
  if (tmb_data_param$specifications$scaled_c) {
    postfile_df[, scaled_c := tmb_output_obj$obj$report()$scaled_c]
  }

  ## Upweights
  postfile_df[, weights := tmb_data_param$data$weight_decay ]

  ## Fixed effects?
  if (!is.null(tmb_data_param$specifications$xvar)) {
    postfile_df[, paste0("FE_", tmb_data_param$specifications$xvar) := lapply(c(1:length(tmb_data_param$specifications$xvar)), function(x) tmb_output_obj$obj$report()$b[x]) ]
  }

  ## Random coefs?
  if (!is.null(tmb_data_param$specifications$re_vars)) {
    postfile_df[, paste0("RE_", (tmb_data_param$specifications$re_vars)) := T]
  }

  ## Intercepts

  ## Global int?
  postfile_df[, global_int := tmb_data_param$specifications$a]

  ## Country ints and random or not?
  postfile_df[, country_int := tmb_data_param$specifications$z]
  postfile_df[, country_int_dist := tmb_data_param$specifications$z_ran]


  ## Yvar:
  postfile_df[, yvar := tmb_data_param$specifications$yvar]



  return(postfile_df)
}






################################################
## THE DATA COMPILER FOR DRAWS
################################################



## Function which takes the distro draws info and compiles all the draws (by either level of diff)
### NOTE: We need the very first year of level data to cumsum (hehe) from diffs
country_looper <- function(oos_input, years, draw_type = "diff", input_rmse_data, draws_data) {

  ## Loop over each country
  country_applying <- foreach(x = country_list, .inorder = FALSE) %dopar% {

    ## Filter out which OOS we're keeping and the country for this iteration (if Chaos)
    if (!is.na(oos_input)) {
      country_map <- input_rmse_data[oos == oos_input & iso3 == paste0(x), .(iso3, model_number, draw_num)]
    } else {
      country_map <- input_rmse_data[iso3 == paste0(x), .(iso3, model_number, draw_num)]
    }



    ## Loop over each sub model for that country and OOS year
    draw_DT <- foreach(mod = c(1:nrow(country_map))) %do% {
      get_model <- country_map[mod, model_number]

      ## Get the number of draws for that particular mod
      drawz <- country_map[mod, draw_num]

      ## Get the forecasts just for those years and country
      if (draw_type == "diff") {
        get_dat <- draws_data[[paste0(get_model)]][iso3 == paste0(x) & year %in% years, .SD, .SDcols = c("iso3", "year", paste0("ydiff_", c(1:drawz)))]
      } else if (draw_type == "level") {
        get_dat <- draws_data[[paste0(get_model)]][iso3 == paste0(x) & year %in% years, .SD, .SDcols = c("iso3", "year", paste0("yvar_", c(1:drawz)))]
      }


      ## Clean up
      # if(mod > 1) {
      #   get_dat <- get_dat[,iso3 := NULL]; get_dat <- get_dat[,year := NULL]
      # }

      ## Change column names to be able to do the merge easily
      colnames(get_dat) <- c("iso3", "year", paste0("dr_", mod, "_", c(1:drawz)))


      return(get_dat)
    }

    ## Column bind the models
    # draws_binded <- do.call(cbind, draw_DT)

    ## Go for a merge instead
    draws_binded <- draw_DT[[1]]
    for (dt in c(2:length(draw_DT))) {
      draws_binded <- merge(draws_binded, draw_DT[[dt]], c("iso3", "year"), all = T)
    }
    ## Change column name
    number_of_cols <- ncol(draws_binded) - 2

    colnames(draws_binded) <- c("iso3", "year", paste0("draw_", c(1:number_of_cols)))

    return(draws_binded)
  }

  ## Return a binded list, and keep just N draws
  binded_data <- rbindlist(country_applying, fill = T, use.names = T)[, .SD, .SDcols = c("iso3", "year", paste0("draw_", c(1:N_draws)))]


  return(binded_data)
}



##### Same funks but for means


## Function which takes the distro draws info and compiles all the draws (by either level of diff)
### NOTE: We need the very first year of level data to cumsum (hehe) from diffs
country_looper_means <- function(oos_input, years, draw_type = "diff", input_rmse_data, means_data) {

  ## Loop over each country
  country_applying <- foreach(x = country_list, .inorder = FALSE) %dopar% {

    ## Filter out which OOS we're keeping and the country for this iteration (if Chaos)
    if (!is.na(oos_input)) {
      country_map <- input_rmse_data[oos == oos_input & iso3 == paste0(x), .(iso3, model_number, draw_num)]
    } else {
      country_map <- input_rmse_data[iso3 == paste0(x), .(iso3, model_number, draw_num)]
    }



    ## Loop over each sub model for that country and OOS year
    draw_DT <- foreach(mod = c(1:nrow(country_map))) %do% {
      get_model <- country_map[mod, model_number]

      ## Get the number of draws for that particular mod
      drawz <- country_map[mod, draw_num]

      ## Get the forecasts just for those years and country
      if (draw_type == "diff") {
        get_dat <- means_data[[paste0(get_model)]][iso3 == paste0(x) & year %in% years, .SD, .SDcols = c("iso3", "year", "ydiff")]
      } else if (draw_type == "level") {
        get_dat <- means_data[[paste0(get_model)]][iso3 == paste0(x) & year %in% years, .SD, .SDcols = c("iso3", "year", "yvar")]
      }


      ## Clean up

      # if(mod > 1) {
      #   get_dat <- get_dat[,iso3 := NULL]; get_dat <- get_dat[,year := NULL]
      # }

      ## Change column names to be able to do the merge easily
      colnames(get_dat) <- c("iso3", "year", paste0("dr_", mod))

      return(get_dat)
    }

    ## Column bind the models
    # draws_binded <- do.call(cbind, draw_DT)

    ## Go for a merge instead
    draws_binded <- draw_DT[[1]]
    for (dt in c(2:length(draw_DT))) {
      draws_binded <- merge(draws_binded, draw_DT[[dt]], c("iso3", "year"), all = T)
    }

    ## Change column names
    number_of_cols <- ncol(draws_binded) - 2

    colnames(draws_binded) <- c("iso3", "year", paste0("y_", c(1:number_of_cols)))

    return(draws_binded)
  }

  ## Return a binded list, and keep just N draws
  binded_data <- rbindlist(country_applying, fill = T, use.names = T)

  return(binded_data)
}



#############################################
### Some math funks
#############################################

## Lemon squeeze logit transformer
logit_trans <- function(x) {
  return(log(((x * 999 / 1000) + (0.5 / 1000)) / (1 - ((x * 999 / 1000) + (0.5 / 1000)))))
}

## Inv Lemon squeeze logit transformer
inv_logit_trans <- function(x) {
  return(((exp(x) / (1 + exp(x))) - (0.5 / 1000)) * (1000 / 999))
}



logit_fn <- function(y, y_min, y_max, epsilon) {
  log((y - (y_min - epsilon)) / (y_max + epsilon - y))
}

antilogit_fn <- function(antiy, y_min, y_max, epsilon) {
  (exp(antiy) * (y_max + epsilon) + y_min - epsilon) /
    (1 + exp(antiy))
}
