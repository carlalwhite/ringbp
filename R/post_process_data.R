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
    summarise(mean=mean(cumulative),median=median(cumulative),
              quartile_25=quantile(cumulative,0.25), quartile_75=quantile(cumulative,0.75)) %>%
    pivot_longer(cols=c(mean,median,quartile_25,quartile_75),values_to = "value",names_to = "label")

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
    filter(day==end_day & cumulative > 5) %>%
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


#' Function to create probability table for nbinom distribution
#'
#' @return table of probabilities
#' @export
#' @examples
#'
create_prob_table <- function(r0,disp,prob_groups) {

  prob_table <- data.frame(x=prob_groups[[1]],P=dnbinom(prob_groups[[1]],size=disp,mu=r0))

  grouped_rows <- sapply(prob_groups[2:length(prob_groups)], function(x){
    c(x=paste0(x[1],"-",x[length(x)]),P=sum(dnbinom(x,size=disp,mu=r0)))
  }) %>%
    t()

  prob_table <- prob_table %>%
    rbind.data.frame(grouped_rows)

  prob_table$P <- as.numeric(prob_table$P)

  prob_table <- prob_table %>%
    mutate('%'=round(P*100))

  return(prob_table)

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

    res$onset <- res$onset-res$onset[1]
    res <- res[-c(1),]
    result = data.frame(results = results[[sim_no_list[i]]], params = params)

    write.csv(res,paste0("inst/outputs/branching_results_",Sys.Date(),"_r",params$r0community,"_disp",
                         params$disp.com,"_ic",params$num.initial.cases,"_",i,".csv"),row.names = FALSE)

    saveRDS(result,paste0("inst/outputs/branching_results_",Sys.Date(),"_r",params$r0community,"_disp",
                          params$disp.com,"_ic",params$num.initial.cases,"_",i,".rds"))
  }

}




