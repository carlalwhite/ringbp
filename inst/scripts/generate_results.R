library(ringbp)
library(tidyverse)

scenarios <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  dist_group = list(tibble::tibble(
    dist = c("Dist1"),
    dist_shape = c(2.101547),
    dist_scale = c(11.29064)
  )),
  delay_group = list(tibble::tibble(
    delay = c("Delay1"),
    delay_shape = c(1.347738),
    delay_scale = c(4.360796)
  )),
  r0community = c(1.5, 2, 2.5),
  r0isolated = c(0.1, 0.5, 0.8),
  prop.ascertain = c(0),
  num.initial.cases = c(1,5)) %>%
  tidyr::unnest("dist_group") %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = 90,
                                  cap_cases = 5000,
                                  prop.asym = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16,
                                  k=0)

## Set up multicore if using see ?future::plan for details
## Use the workers argument to control the number of cores used.
future::plan("multisession")


## Run paramter sweep
sweep_results <- ringbp::parameter_sweep(scenarios,
                                         sim_fn = sim_with_params,
                                         samples = 10,
                                         show_progress = TRUE)

saveRDS(sweep_results, file = "data-raw/res.rds")

