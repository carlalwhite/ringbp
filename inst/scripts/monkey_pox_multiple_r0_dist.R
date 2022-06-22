library(ringbp)
library(tidyverse)

scenarios <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  r0_com_group = list(tibble::tibble(
    r0community = c(0.8, 0.8, 0.8, 1.1, 1.1, 1.1, 1.1, 1, 0.8),
    disp.com = c(0.5, 2, 100, 0.5, 2, 100, 0.5, 2, 100)
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
  num.initial.cases = 10,
  prop.ascertain = 0,
  prop.asym = 0,
  k = 0
) %>%
  tidyr::unnest("r0_com_group") %>%
  tidyr::unnest("r0_iso_group") %>%
  tidyr::unnest("dist_group") %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = 120,
                                  cap_cases = 5000)

## Set up multicore if using see ?future::plan for details
## Use the workers argument to control the number of cores used.
future::plan("multisession")


## Run paramter sweep
sweep_results <- ringbp::parameter_sweep(scenarios,
                                         sim_fn = sim_with_params,
                                         samples = 1000,
                                         show_progress = TRUE)

saveRDS(sweep_results, file = "data-raw/res.rds")

