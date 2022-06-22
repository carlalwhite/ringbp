#' Function to create a single plot
#'
#' @author Carla White
#'
#' @return plotting data
#' @export
#' @examples
#'
single_plot_simulations <- function(results, cols, avg_data = NULL, avg_type=NULL) {

  ggdata <- ggplot() +
    geom_line(data=results, aes_string(x="day", y=cols[2], group = "sim"),
              colour = "#cadedc", alpha = 0.8)+
    scale_y_continuous(name="Number of cases")+
    scale_x_continuous("Day")+
    theme_bw()

  if(!is.null(avg_data)){
    if(!is.null(avg_type)){
      avg_data <- avg_data %>%
        filter(label %in% avg_type)
    }

    ggdata <- ggdata +
      geom_line(data=avg_data, aes_string(x="day", y="value",group="label", colour="label"))+
      theme(legend.title=element_blank(),legend.position = c(0.2, 0.7))+
      scale_color_manual(values = c("median"="#9F196C","mean"="#BB0D47",
                                    "quantile_25"="#9BB460", "quantile_75"="#19819A"))
  }
  ggdata <- ggplotGrob(ggdata)
  return(ggdata)
}


#' Function to plot the probability mass function of the negative binomial distribution
#'
#' @return plotting data
#' @export
#' @examples
#'
single_plot_nbinom <- function(prob_data, probs_to_plot) {

  plot_data <- prob_data[prob_data$x %in% probs_to_plot,]

  pmf <- ggplot(data=plot_data,aes(x=as.numeric(x),y=P))+
    geom_bar(stat = "identity", fill = "#67A29B")+
    theme_bw()+
    scale_x_continuous(breaks = seq(as.numeric(probs_to_plot[1]),
                                    as.numeric(probs_to_plot[length(probs_to_plot)]), by = 1))

  pmf <- ggplotGrob(pmf)
  return(pmf)
}


#' Function to create a table of probabilities for plots
#'
#' @return plotting data
#' @export
#' @examples
#'
single_prob_table <- function(prob_data, probs_for_table) {

  tab_data <- prob_data[prob_data$x %in% probs_for_table,]%>%
    select(x,"%") %>%
    tableGrob(rows=NULL)

  return(tab_data)
}


