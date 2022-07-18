library(ringbp)
library(tidyverse)

scenarios <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  r0_com_group = list(tibble::tibble(
    r0community = c(1.1, 0.95, 0.6),
    disp.com = c(0.5, 2, 100)
  )),
  r0_iso_group = list(tibble::tibble(
    r0isolated = c(0),
    disp.iso = c(0.1)
  )),
  dist_group = list(tibble::tibble(
    dist_shape = c(3),
    dist_scale = c(11)
  )),
  delay_group = list(tibble::tibble(
    delay_shape = c(1.1),
    delay_scale = c(4)
  )),
  num.initial.cases = c(rep(1,1)),
  prop.ascertain = 0,
  prop.asym = 0,
  k = 0
) %>%
  tidyr::unnest("r0_com_group") %>%
  tidyr::unnest("r0_iso_group") %>%
  tidyr::unnest("dist_group") %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

params <- list(n.sim = 1000,
               cap_max_days = 120)

## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = params$cap_max_days,
                                  cap_cases = 100000)

## Set up multicore if using see ?future::plan for details
## Use the workers argument to control the number of cores used.
future::plan("multisession")

## Run paramter sweep
sweep_results <- ringbp::parameter_sweep(scenarios,
                                         sim_fn = sim_with_params,
                                         samples = params$n.sim,
                                         show_progress = TRUE)

# saveRDS(sweep_results, file = "data-raw/res.rds")

# daily_results <- map(1:length(sweep_results$sims), function(i) sweep_results$sims[[i]] %>%
#                        map(create_case_data, params$n.sim, params$cap_max_days) %>%
#                        data.table::rbindlist() %>%
#                        mutate(sim = rep(1:params$n.sim, rep(floor(params$cap_max_days) + 1, params$n.sim))))
#
# daily_results <- daily_results[[1]]
# max_sim <- daily_results[which.max(daily_results$cumulative),]$sim
#
# plot_data <- daily_results %>%
#   filter(sim == daily_results[which.max(daily_results$cumulative),]$sim)
#
# ggplot(plot_data)+
#   geom_line(aes(x=day, y=cumulative))
