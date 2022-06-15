#' Function create a data frame of case data
#'
#' @author Carla White
#' @param n.sim no of simulations
#' @param cap_max_days max no of days for simulation
#'
#' @return a data frame of case data
#' @export
#' @examples
#'
create_case_data <- function(results,n_sim,cap_max_days) {

  results <- results[, day := floor(onset)
  ][, .(daily_cases = .N), by = day]

  # maximum outbreak days
  max_day <- floor(cap_max_days)

  # remove days after max_day cap
  results <- results[day < cap_max_days]

  # weeks with 0 cases in 0:max_day
  missing_days <- (0:max_day)[!(0:max_day %in% results$day)]

  # add in missing days if any are missing
  if (length(missing_days > 0)) {
    results <- data.table::rbindlist(list(results,
                                              data.table(day = missing_days,
                                                         daily_cases = 0)))
  }

  # add cumulative results
  results <- results[order(day)][, cumulative := cumsum(daily_cases)]

  return(results)
}

#' Function to create mean and median of runs
#'
#' @return a data frame of mean and median of the runs
#' @export
#' @examples
#'
create_averages_data <- function(results) {

  outbreak_averages <- results %>%
    group_by(day) %>%
    summarise(mean=mean(cumulative),median=median(cumulative)) %>%
    pivot_longer(cols=c(mean,median),values_to = "value",names_to = "label")

  return(outbreak_averages)
}


#' Function to filter the results with the sims to plot
#'
#' @param no_of_sims no of simulations to select for plotting
#' @param end_day max no of days for simulation
#'
#' @return a filtered data frame of the sims needed for plotting
#' @export
#' @examples
#'
sims_to_plot <- function(results,no_of_sims,end_day) {

  sims <- results %>%
    filter(day==end_day & cumulative > no_of_sims) %>%
    arrange(cumulative) %>%
    filter(row_number() %in% floor(seq(1,length(day),length.out=no_of_sims))) %>%
    select(sim) %>% arrange(sim)

  results <- results %>%
    filter(sim %in% sims$sim) %>%
    arrange(sim,day)

  for (i in seq(1,no_of_sims)){
    results$sim <- replace(results$sim,results$sim==sims$sim[i],i)
  }

  return(results)
}


#' Function to select sim results for branching vis
#'
#' @param sim_no_list list with the sim numbers required for exporting
#'
#' @return sim results for branching vis
#' @export
#' @examples
#'
branching_output <- function(results,sim_no_list,params) {

  for(i in seq(1,length(sim_no_list))) {
    res <- results[[sim_no_list[i]]] %>%
      select(onset,caseid,infector) %>%
      mutate_if(is.numeric, floor) %>%
      arrange(onset)

    write.csv(res,paste0("inst/outputs/branching_results_",Sys.Date(),"_r",params$r0community,"_disp",
                         params$disp.com,"_ic",params$num.initial.cases,"_",i,".csv"),row.names = FALSE)

    saveRDS(params,paste0("inst/outputs/branching_results_",Sys.Date(),"_r",params$r0community,"_disp",
                          params$disp.com,"_ic",params$num.initial.cases,"_",i,".rds"))
  }

}




