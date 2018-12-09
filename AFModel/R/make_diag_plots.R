#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param diag_data PARAM_DESCRIPTION
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
#' @rdname make_diag_plots
#' @export
make_diag_plots <- function(diag_data, out_specs = c(1, 2, 3, 4, 5)) {


  ### Initialize the output objects
  plot1_list <- NULL
  plot2_list <- NULL
  table_out <- NULL
  plot4_list <- NULL
  plot5 <- NULL

  ## Plots (1)
  if (1 %in% out_specs) {
    plot1_list <- lapply(
      sort(unique(diag_data$data_merged$location_name)),
      function(country) {
        ggplot(diag_data$data_merged[scenario == 0 & location_name == country]) +
          geom_ribbon(aes(x = year, ymin = new_lower, ymax = new_upper), color = NA, fill = "steelblue", alpha = .3) +
          geom_ribbon(aes(x = year, ymin = old_lower, ymax = old_upper), color = NA, fill = "red1", alpha = .3) +
          geom_line(aes(x = year, y = new_mean), color = "blue") +
          geom_line(aes(x = year, y = old_mean), color = "red") +
          theme_bw() +
          expand_limits(y = 0) +
          xlab("Year") + ylab("") +
          ggtitle(country) +
          theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5))
      }
    )
  }



  ## Plots (2)
  if (2 %in% out_specs) {
    diag_data$data_merged[, scenario := factor(scenario, levels = c(-1, 0, 1))]
    plot2_list <- lapply(
      sort(unique(diag_data$data_merged$location_name)),
      function(country) {
        ggplot(diag_data$data_merged[ location_name == country]) +
          geom_ribbon(aes(x = year, ymin = new_lower, ymax = new_upper, fill = scenario, group = scenario), color = NA, alpha = .3) +
          geom_line(aes(x = year, y = new_mean, group = scenario, color = scenario)) +
          scale_fill_manual(values = c("red1", "steelblue", "green1"), name = "Scenario", labels = c("Worse", "Reference", "Better")) +
          scale_color_manual(values = c("red4", "blue4", "green4"), name = "Scenario", labels = c("Worse", "Reference", "Better")) +
          theme_bw() +
          expand_limits(y = 0) +
          xlab("Year") + ylab("") +
          ggtitle(country) +
          theme(
            text = element_text(size = 16), plot.title = element_text(hjust = 0.5),
            legend.position = "bottom"
          )
      }
    )
  }

  ## Table (3)
  if (3 %in% out_specs) {
    table_out <- diag_data$data_merged[, .(location_id, iso3,
      Country = location_name,
      Scenario = scenario, Year = year,
      Mean = new_mean, Lower = new_lower, Upper = new_upper
    )]
    table_out[, Scenario := as.character(Scenario)]
    table_out[Scenario == "-1", Scenario := "Worse"]
    table_out[Scenario == "0", Scenario := "Reference"]
    table_out[Scenario == "1", Scenario := "Better"]
  }


  ## Plots (4)
  if (4 %in% out_specs) {
    ## Create a ratio of new to old
    diag_data$data_merged[, ratio_new_old := new_mean / old_mean]
    plot4_list <- lapply(diag_data$scatter_yrs, function(yeer) {
      ggplot(diag_data$data_merged[year == yeer & scenario == 0]) +
        geom_abline(linetype = "dashed", color = "red", alpha = .3) +
        geom_point(aes(x = old_mean, y = new_mean)) +
        geom_label(
          data = diag_data$data_merged[year == yeer & scenario == 0 & (ratio_new_old > 1.4 | ratio_new_old < 0.6) ],
          aes(x = old_mean, y = new_mean, label = iso3)
        ) +
        theme_bw() +
        xlab("Old") + ylab("New") +
        ggtitle(yeer) +
        theme(
          text = element_text(size = 16), plot.title = element_text(hjust = 0.5)
        )
    })
  }


  ## Plot (5)
  if (5 %in% out_specs) {
    diag_data$aroc_data[, ratio_fut_past := get(paste0(diag_data$aroc_yrs[3])) / get(paste0(diag_data$aroc_yrs[2])) ]
    plot5 <- ggplot() +
      geom_abline(linetype = "dashed", color = "red", alpha = .3) +
      geom_point(
        data = diag_data$aroc_data[scenario == 0],
        aes(
          x = get(paste0(diag_data$aroc_yrs[2])),
          y = get(paste0(diag_data$aroc_yrs[3]))
        )
      ) +
      geom_label(
        data = diag_data$aroc_data[scenario == 0 ],
        aes(
          label = iso3,
          x = get(paste0(diag_data$aroc_yrs[2])),
          y = get(paste0(diag_data$aroc_yrs[3]))
        )
      ) +
      theme_bw() +
      xlab("Past") + ylab("Future") +
      ggtitle(paste0(
        "AROC (percent space) of Past (",
        diag_data$aroc_yrs[1], "-",
        diag_data$aroc_yrs[2], ") vs Future (",
        diag_data$aroc_yrs[2], "-",
        diag_data$aroc_yrs[3], ")"
      )) +
      theme(
        text = element_text(size = 16), plot.title = element_text(hjust = 0.5)
      )
  }


  ## Return all in a list
  outlist <- list(
    plot1_list,
    plot2_list,
    table_out,
    plot4_list,
    plot5
  )
  return(outlist)
}
