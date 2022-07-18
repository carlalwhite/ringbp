library(ggplot2)
library(gridExtra)
library(gtable)
library(grid)

# params <- list(n.sim = 1000,
#                cap_max_days = 120)

daily_results <- map(1:length(sweep_results$sims), function(i) sweep_results$sims[[i]] %>%
                       map(create_case_data, params$n.sim, params$cap_max_days) %>%
                       data.table::rbindlist() %>%
                       mutate(sim = rep(1:params$n.sim, rep(floor(params$cap_max_days) + 1, params$n.sim))))

avg_data <- map(daily_results, create_averages_data)

m <- NULL
for (i in 1:length(sweep_results$sims)) {
  m[i] <- avg_data[[i]]%>%
    filter(day == 120 & label == 'median') %>%
    select(value)
}

plot_data <- map(daily_results, sims_to_plot,no_of_sims=100,
                 end_day=params$cap_max_days)

probs <- map2(sweep_results$r0community, sweep_results$disp.com, create_prob_table,
                prob_groups=list(c(0:10),c(2:4),c(5:10)))

# title_labs - c("ESN", "HCW", "Community")
individ_plots <- lapply(1:length(sweep_results$scenario), function(i)
  arrangeGrob(single_plot_simulations(plot_data[[i]],c("day","cumulative","sim"),avg_data[[i]]),
  single_plot_nbinom(probs[[i]], probs_to_plot=c(0:10)),
  single_prob_table(probs[[i]], probs_for_table=c("0","1","2-4","5-10")),
  layout_matrix = rbind(c(1,1,1),c(2,2,3)), top=paste0("R0 = ",sweep_results$r0community[i],
                                                                      ", k = ", sweep_results$disp.com[i]))
)

plot1 <- grid.arrange(individ_plots[[1]],individ_plots[[2]],individ_plots[[3]],nrow=1,ncol=3,top=) %>%
  ggsave(filename="inst/plots/12_07_2022_scenario1.png", width=8*1.5, height=4*1.5)
# plot2 <- grid.arrange(individ_plots[[4]],individ_plots[[5]],individ_plots[[6]],nrow=1) %>%
#   ggsave(filename="inst/plots/scenario2.png", width=8*1.5, height=4*1.5)
# plot3 <- grid.arrange(individ_plots[[7]],individ_plots[[8]],individ_plots[[9]],nrow=1) %>%
#   ggsave(filename="inst/plots/scenario3.png", width=8*1.5, height=4*1.5)

# sims <- c(540,511,307)
#
# for (i in 1:3) {
#   branching_output(sweep_results$sims[[i]],sims[i],params = c(sweep_results[i,c(2,3,10)]))
# }








