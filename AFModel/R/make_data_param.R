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
