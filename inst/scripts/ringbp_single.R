library(ringbp)
library(dplyr)
library(ggplot2)

params = list(n.sim = 100,
           num.initial.cases = 10,
           prop.asym=0,
           prop.ascertain = 0,
           cap_cases = 4500,
           cap_max_days = 120,
           r0isolated = 0,
           r0community = 0.8,
           disp.com = 0.5,
           disp.iso = 0.6,
           dist_shape = 3,
           dist_scale = 11,
           delay_shape = 1.1,
           delay_scale = 4,
           k = 4)

results <- do.call(scenario_sim, params)

daily_results <- map(results, create_case_data, params$n.sim, params$cap_max_days) %>%
  data.table::rbindlist() %>%
  mutate(sim = rep(1:params$n.sim, rep(floor(params$cap_max_days) + 1, params$n.sim)))

avg_data <- create_averages_data(daily_results)
avg_data[avg_data$day==120,]

plot_data <- sims_to_plot(daily_results, no_of_sims=10, end_day=params$cap_max_days)

ggplot2::ggplot() +
  ggplot2::geom_line(data=plot_data, ggplot2::aes(x=day, y=cumulative, group = as.factor(sim)),
                     colour= "#D55E00", alpha = 0.3) +
  ggplot2::geom_line(data=avg_data, ggplot2::aes(x=day, y=value, group = label, colour = label)) +
  ggplot2::scale_color_manual(values = c("median"="#0072B2","mean"="#009E73")) +
  ggplot2::scale_y_continuous(name="Number of cases") +
  ggplot2::theme_bw() +
  theme(legend.title=element_blank(),legend.position = c(0.1, 0.9))



sum(detect_extinct(daily_results,params$cap_cases)$extinct)

# output_sims <- c(42,8,12)
# branching_output(results,output_sims,params)

