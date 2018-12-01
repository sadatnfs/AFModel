
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param diag_plots PARAM_DESCRIPTION
#' @param root_fold PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname print_diag_plots
#' @export
print_diag_plots <- function(diag_plots, root_fold) {
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

  print("Write table")
  fwrite(
    diag_plots[[3]],
    paste0(root_fold, "/summary_files/Diag3_Forecast_Data.csv")
  )

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

  print("Print AROC plots")
  Cairo(
    file = paste0(root_fold, "/summary_files/Diag5_AROC_Past_and_Future.pdf"),
    onefile = T, type = "pdf",
    dpi = 80,
    width = 1600, height = 800
  )

  plot(diag_plots[[5]])

  return(0)
}
