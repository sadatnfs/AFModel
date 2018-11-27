############################################################
### OOS and residual functions
############################################################
## Making residuals from TMB output
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param tmb_output_obj PARAM_DESCRIPTION
#' @param tmb_data_param PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname create_residuals
#' @export 
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
