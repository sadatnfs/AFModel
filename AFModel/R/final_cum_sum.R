### Cumsumming Chaos (or not) POST DRAWS
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param root_fold PARAM_DESCRIPTION
#' @param chaos PARAM_DESCRIPTION
#' @param scenario PARAM_DESCRIPTION, Default: F
#' @param oos_years PARAM_DESCRIPTION
#' @param N_draws PARAM_DESCRIPTION
#' @param rev_trans PARAM_DESCRIPTION
#' @param pop_data PARAM_DESCRIPTION, Default: NULL
#' @param pop_var PARAM_DESCRIPTION, Default: 'total_pop'
#' @param pop_action PARAM_DESCRIPTION, Default: NULL
#' @param hack_drop_NAs PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname final_cum_sum
#' @export 
final_cum_sum <- function(root_fold, chaos, scenario = F, oos_years, N_draws, rev_trans, pop_data = NULL, pop_var = "total_pop", pop_action = NULL, hack_drop_NAs = F) {
  if (chaos) {
    if (!scenario) {
      print("Bring all the Chaos compiles into memory")
      chaos_compiles <- rbindlist(lapply(c(1:oos_years), function(f) readRDS(paste0(root_fold, "/summary_files/chaos_oos_", f, ".rds"))))
    } else {
      print("Bring all the Chaos compiles into memory")
      chaos_compiles_worse <- rbindlist(lapply(c(1:oos_years), function(f) readRDS(paste0(root_fold, "/summary_files_scenarios/worse_chaos_oos_", f, ".rds"))))[, scenario := -1]
      chaos_compiles_better <- rbindlist(lapply(c(1:oos_years), function(f) readRDS(paste0(root_fold, "/summary_files_scenarios/better_chaos_oos_", f, ".rds"))))[, scenario := 1]
      chaos_compiles <- rbindlist(list(chaos_compiles_better, chaos_compiles_worse))
    }
  } else {
    if (!scenario) {
      print("Bring all the draws into memory")
      chaos_compiles <- readRDS(paste0(root_fold, "/summary_files/chaos_oos.rds"))
    } else {
      print("Bring all the draws into memory")
      chaos_compiles_worse <- readRDS(paste0(root_fold, "/summary_files_scenarios/worse_chaos_oos.rds"))[, scenario := -1]
      chaos_compiles_better <- readRDS(paste0(root_fold, "/summary_files_scenarios/better_chaos_oos.rds"))[, scenario := 1]
      chaos_compiles <- rbindlist(list(chaos_compiles_better, chaos_compiles_worse))
    }
  }

  if (!scenario) {
    ## Set iso3 and year index
    setkeyv(chaos_compiles, c("iso3", "year"))

    print("Cumsum all draws for each country, and reverse transform")
    chaos_compiles[, paste0("draw_", c(1:N_draws)) := lapply(paste0("draw_", c(1:N_draws)), function(x) cumsum(get(paste0(x)))), by = c("iso3")]


    ## Reserve transform; rev_trans = 'exp'
    chaos_compiles[, paste0("draw_", c(1:N_draws)) := lapply(paste0("draw_", c(1:N_draws)), function(x) get(rev_trans)(get(paste0(x)))) ]


    if (!is.null(pop_data)) {
      ## Merge in pops
      chaos_compiles <- merge(chaos_compiles, pop_data, c("iso3", "year"))

      if (pop_action == "div") {
        chaos_compiles[, paste0("draw_", c(1:N_draws)) := lapply(paste0("draw_", c(1:N_draws)), function(x) get(paste0(x)) / get(pop_var)) ]
      } else if (pop_action == "multiply") {
        chaos_compiles[, paste0("draw_", c(1:N_draws)) := lapply(paste0("draw_", c(1:N_draws)), function(x) get(paste0(x)) * get(pop_var)) ]
      }

      chaos_compiles[, (pop_var) := NULL]
    }

    ## Create stats
    if (hack_drop_NAs) {
      chaos_compiles <- chaos_compiles[!is.na(draw_1)]
      chaos_stats <- stat_maker(data = chaos_compiles, melt = T, merge = F)
    } else {
      chaos_stats <- stat_maker(data = chaos_compiles, melt = T, merge = F)
    }
  } else {

    ## Set iso3 and year index
    setkeyv(chaos_compiles, c("iso3", "scenario", "year"))

    print("Cumsum all draws for each country, and reverse transform")
    chaos_compiles[, paste0("draw_", c(1:N_draws)) := lapply(paste0("draw_", c(1:N_draws)), function(x) cumsum(get(paste0(x)))), by = c("iso3", "scenario")]


    ## Reserve transform; rev_trans = 'exp'
    chaos_compiles[, paste0("draw_", c(1:N_draws)) := lapply(paste0("draw_", c(1:N_draws)), function(x) get(rev_trans)(get(paste0(x)))) ]


    if (!is.null(pop_data)) {
      ## Merge in pops
      chaos_compiles <- merge(chaos_compiles, pop_data, c("iso3", "year"))

      if (pop_action == "div") {
        chaos_compiles[, paste0("draw_", c(1:N_draws)) := lapply(paste0("draw_", c(1:N_draws)), function(x) get(paste0(x)) / get(pop_var)) ]
      } else if (pop_action == "multiply") {
        chaos_compiles[, paste0("draw_", c(1:N_draws)) := lapply(paste0("draw_", c(1:N_draws)), function(x) get(paste0(x)) * get(pop_var)) ]
      }

      chaos_compiles[, (pop_var) := NULL]
    }

    ## Create stats
    if (hack_drop_NAs) {
      chaos_compiles <- chaos_compiles[!is.na(draw_1)]
      chaos_stats <- stat_maker(data = chaos_compiles, idvar = c("iso3", "scenario", "year"), melt = T, merge = F)
    } else {
      chaos_stats <- stat_maker(data = chaos_compiles, idvar = c("iso3", "scenario", "year"), melt = T, merge = F)
    }
  }

  return(list(draws = chaos_compiles, stats = chaos_stats))
}
