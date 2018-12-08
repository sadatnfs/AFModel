
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param diag_plots PARAM_DESCRIPTION
#' @param root_fold PARAM_DESCRIPTION
#' @param out_specs choices of which diagnostics to make (see Details), Default: c(1,2,3,4,5)
#' @return OUTPUT_DESCRIPTION
#' @details Create the following diagnostic plots and tables:
#'   (1) Overlapping plots of reference by countries with a baseline model
#'   (2) Scenarios plots of new model
#'   (3) Table with mean, upper and lower
#'   (4) Scatter of data at specific years against baseline
#'   (5) Scatter of AROC between past and future
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname print_diag_plots
#' @export
print_diag_plots <- function(diag_plots, root_fold, out_specs = c(1, 2, 3, 4, 5)) {
  if (1 %in% out_specs) {
    print("Loop over and print plot 1")
    Cairo(
      file = paste0(root_fold, "/summary_files/Diag1_Compare_with_Baseline.pdf"),
      onefile = T, type = "pdf",
      dpi = 80,
      width = 1600, height = 800
    )
    for (lll in c(1:length(diag_plots[[1]]))) {
      plot(diag_plots[[1]][[lll]])
    }
    dev.off()
  }


  if (2 %in% out_specs) {
    print("Loop over and print plot 2")
    Cairo(
      file = paste0(root_fold, "/summary_files/Diag2_Compare_with_Scenarios.pdf"),
      onefile = T, type = "pdf",
      dpi = 80,
      width = 1600, height = 800
    )

    for (lll in c(1:length(diag_plots[[2]]))) {
      plot(diag_plots[[2]][[lll]])
    }
    dev.off()
  }


  if (3 %in% out_specs) {
    print("Write table")
    fwrite(
      diag_plots[[3]],
      paste0(root_fold, "/summary_files/Diag3_Forecast_Data.csv")
    )
  }

  if (4 %in% out_specs) {
    print("Print scatter plots")
    Cairo(
      file = paste0(root_fold, "/summary_files/Diag4_Scatter_against_Baseline.pdf"),
      onefile = T, type = "pdf",
      dpi = 80,
      width = 1600, height = 800
    )

    for (lll in c(1:length(diag_plots[[4]]))) {
      plot(diag_plots[[4]][[lll]])
    }
    dev.off()
  }

  if (5 %in% out_specs) {
    print("Print AROC plots")
    Cairo(
      file = paste0(root_fold, "/summary_files/Diag5_AROC_Past_and_Future.pdf"),
      onefile = T, type = "pdf",
      dpi = 80,
      width = 1600, height = 800
    )

    plot(diag_plots[[5]])
    dev.off()
  }

  return(0)
}
