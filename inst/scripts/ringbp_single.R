library(ringbp)
library(dplyr)
library(ggplot2)

# res <- ringbp::scenario_sim(n.sim = 100, num.initial.cases = 1,prop.asym=0,
#                             prop.ascertain = 0, cap_cases = 4500, cap_max_days = 90,
#                             r0isolated = 1.2, r0community = 2.1, disp.com = 0.16, disp.iso = 2, delay_shape = 1.651524,
#                             delay_scale = 4.287786,k = 0, quarantine = FALSE)

results <- scenario_sim(n.sim = 500,
                        num.initial.cases = 1,
                        prop.asym=0,
                        prop.ascertain = 0,
                        cap_cases = 4500,
                        cap_max_days = 90,
                        r0isolated = 1.3,
                        r0community = 2.5,
                        disp.com = 0.38,
                        disp.iso = 1,
                        dist_shape = 2.694934,
                        dist_scale = 11.24573,
                        delay_shape = 0.999976,
                        delay_scale = 1.99998,
                        k = 0)

# Plot of daily cases
gg <- results %>%
  ggplot()+
  geom_line(aes(x=day, y=cumulative, col = as.factor(sim)),alpha=0.6,show.legend = FALSE)+
  scale_y_continuous(name="Number of cases")

outbreak_sims <- results %>%
  filter(day==90 & cumulative > 10) %>%
  select(sim)

outbreak_results <- results %>%
  filter(sim %in% outbreak_sims$sim)

outbreak_mean <- outbreak_results %>%
  group_by(day) %>%
  summarise(mean=mean(cumulative),median=median(cumulative))

gg <- gg+
  geom_line(data=outbreak_mean,aes(x=day,y=mean),colour="black")+
  geom_line(data=outbreak_mean,aes(x=day,y=median),colour="blue")

# data <- read.csv("data/monkeypox_uk.csv")
#
# gg <- gg+
#   geom_point(data=data,aes(x=day-7,y=cumulative))

gg
# res2 <- res %>%
#   group_by(week) %>%
#   summarise(mean=mean(cumulative))

# Plot of weekly cases
# ggplot2::ggplot(data=res, ggplot2::aes(x=week, y=cumulative, col = as.factor(sim))) +
#   ggplot2::geom_line(show.legend = FALSE, alpha=0.3) +
#   ggplot2::scale_y_continuous(name="Number of cases") +
#   ggplot2::theme_bw()
#
# ringbp::extinct_prob(res,cap_cases = 4500)

# plot(res2$week,res2$mean)
