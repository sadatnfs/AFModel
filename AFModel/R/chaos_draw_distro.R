## This function will distribute draws across sub-models by Chaos percent and RMSE, if Chaos. It's dope
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param chaos_pc PARAM_DESCRIPTION, Default: 10
#' @param iso_portion PARAM_DESCRIPTION, Default: 1
#' @param metadata PARAM_DESCRIPTION, Default: ensemble_metadata
#' @param region_portion PARAM_DESCRIPTION, Default: 0
#' @param super_region_portion PARAM_DESCRIPTION, Default: 0
#' @param global_portion PARAM_DESCRIPTION, Default: 0
#' @param N_draws PARAM_DESCRIPTION, Default: 1000
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname chaos_draw_distro
#' @export 
chaos_draw_distro <- function(data, chaos_pc = 10, iso_portion = 1, metadata = ensemble_metadata,
                              region_portion = 0, super_region_portion = 0, global_portion = 0,
                              N_draws = 1000) {

  # data = copy(data_models_passed)
  # chaos_pc <- 10
  # iso_portion = 2/3; region_portion = 1/3; super_region_portion = 0; global_portion = 0

  ## First, stop if the portion sums are greater than 1
  if (region_portion + iso_portion + super_region_portion + global_portion > 1) {
    stop("RMSE fractions are > 1.")
  }



  print("Computing the weighted RMSE by country-region-SR-global portions")
  data[, rmse_WREG := (region_portion * rmse_REG) + (iso_portion * rmse_iso) + (super_region_portion * rmse_SRREG) + (global_portion * rmse_global)]

  print("Rank each submodel by iso3/region/sr specific by iso3 and OOS year")
  data <- data[!is.na(rmse_WREG), iso_rank := frank(rmse_WREG, ties.method = "first"), by = c("iso3", "oos")]
  data <- data[order(iso3, oos, iso_rank)]

  ## Drop if rmse_iso is missing
  data <- data[!is.na(rmse_WREG)]


  ## What's the highest rank for each? We want the chaos% (and respective draws for each)

  #### NOTE: We use the length as opposed to the max, because that will allow us to filter by criteria
  ##### and therefore, we can get a higher number of draws to use by filtering out

  ## We also wanna drop if RMSE is NA (because those models weren't picked up through virtue of missing vals)

  ## Find the cutoff number of models
  ## What's the max number of models per country we got
  data[, max_rank := max(iso_rank, na.rm = T), c("iso3")]

  ## What's the Chaos% cutoff for each country?
  print(paste0("Keep only the top ", chaos_pc, "% models"))
  data[, draw_cutoff := ceiling(chaos_pc / 100 * max_rank)]

  print(paste0("Countries where draw_cutoff is <=", chaos_pc, ", we bump it up to the minimum of ", chaos_pc, " or the max_rank"))
  data[draw_cutoff <= chaos_pc, draw_cutoff := min(chaos_pc, max_rank, na.rm = T), by = c("iso3")]

  ## Cut off the models passed beyond the cutoff value
  data <- data[iso_rank <= draw_cutoff]


  ### What's the number of draws per country-model ?
  data[, draw_num := ceiling(N_draws / draw_cutoff)]

  #### ORDER FEs in the order of ensemble grid !!!! ####
  if (!is.null(metadata$xvar)[1]) {
    order_of_FEs <- metadata$xvar

    ## Get no FE col names
    data_cols_no_FE <- setdiff(colnames(data), order_of_FEs)

    ## Get FE cols in data and drop the ones where the FE did not pass whatsoever
    FE_data_cols <- colnames(data)[grep("^FE_", colnames(data))]

    ## Drop from order_of_FEs (use intersect of large with small)
    order_of_FEs <- intersect(order_of_FEs, FE_data_cols)

    ## Reorder data
    setcolorder(data, neworder = c(order_of_FEs, data_cols_no_FE))
  }


  print("Making draws:")

  ## Let's get the unique models (with the max number of draws) remaining we will create draws over
  models_remaining <- unique(data[, .(draw_num, model_number, yvar)])
  models_remaining[, drawz := max(draw_num), "model_number"]
  models_remaining <- models_remaining[drawz == draw_num]
  models_remaining[, draw_num := NULL]

  ## Create task ID (1:n) and number of draws
  array_grid <- copy(models_remaining)
  array_grid <- array_grid[, id := .I]

  print("Compute the MAD cap from ALL the MADs if we want to cap our residual errors")
  # data[, diff_mad_cap:= (qnorm(0.90)/.6745)*mad(diff_mad) + median(diff_mad)]
  data[, diff_mad_cap := DoubleMAD(diff_mad_cap)[1] ]
  data[ diff_mad > diff_mad_cap, diff_mad := diff_mad_cap]

  # data[, level_mad_cap:= (qnorm(0.90)/.6745)*mad(level_mad) + median(level_mad)]
  data[, level_mad_cap := DoubleMAD(level_mad_cap)[1] ]
  data[ level_mad > level_mad_cap, level_mad := level_mad_cap]


  ## Extract the MAD cap scalars
  diff_mad_cap <- data[1, diff_mad_cap]
  level_mad_cap <- data[1, level_mad_cap]

  return(list(
    array_grid = array_grid,
    data = data,
    level_mad_cap = level_mad_cap, diff_mad_cap = diff_mad_cap,
    N_draws = N_draws
  ))
}
