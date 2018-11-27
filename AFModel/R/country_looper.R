################################################
## THE DATA COMPILER FOR DRAWS
################################################
## Function which takes the distro draws info and compiles all the draws (by either level of diff)
### NOTE: We need the very first year of level data to cumsum (hehe) from diffs
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param oos_input PARAM_DESCRIPTION
#' @param years PARAM_DESCRIPTION
#' @param draw_type PARAM_DESCRIPTION, Default: 'diff'
#' @param input_rmse_data PARAM_DESCRIPTION
#' @param draws_data PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname country_looper
#' @export 
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
