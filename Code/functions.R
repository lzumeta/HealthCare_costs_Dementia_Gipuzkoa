## functions.R

## Select the best power value according to the AIC criteria
best_power_par <- function(formula, power_values = seq(1.1, 2.0, by = 0.1), num_predicts) {
  res <- data.frame(power = numeric(), AIC = numeric())
  for (p in power_values) {
    mod <- glm(as.formula(formula),
               data = data_cost_san,
               family = tweedie(var.power = p, link.power = 1),
               start = rep(0.1, num_predicts))
    
    aic_value <- AICtweedie(mod)
    res <- rbind(res, data.frame(power = p, AIC = aic_value))
  }
  
  # res
  # res[which.min(res$AIC[-10]),] ## best model with power = 1.8
  best_par <- res[which.min(res$AIC[-length(power_values)]), "power"]
  
  
  return(best_par)
}



plot_estimates <- function(plot_data, 
                           title_text = "",
                           annotate_text = "",
                           x_pos = 0,
                           y_pos = 0,
                           ylimits_sum = 0,
                           breaks_max = 1200) {
  ggplot(plot_data, aes(x = group_col, y = predicted, color = group_col)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    annotate(geom = "text",
             x = x_pos, y = y_pos,
             label = annotate_text,
             hjust = 0) +
    scale_y_continuous(labels = scales::label_number(suffix = "€"), breaks = seq(0, breaks_max, by = 300),
                       limits = c(0, max(plot_data$predicted) + ylimits_sum)) +
    scale_color_manual(values = c("seagreen3", "darkorange", "blue", "red"),
                       labels = c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"),
                       name = "Dementia\nor/and NPSs:") +
    labs(x = "", y = "", title = title_text) +
    theme(legend.position = "none")
}



plot_estimates_sex <- function(plot_data,
                               title_text = "",
                               annotate_text = "",
                               x_pos = 0,
                               y_pos = 0,
                               ylimits_sum = 0,
                               breaks_max = 1200) {
  ggplot(plot_data, aes(x = x, y = predicted, color = group_col, group = group_col)) +
    geom_point(size = 3,
               position = position_dodge(width = 0.2)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2,
                  position = position_dodge(width = 0.2)) +
    annotate(geom = "text",
             x = x_pos, y = y_pos,
             label = annotate_text,
             hjust = 0) +
    scale_y_continuous(labels = scales::label_number(suffix = "€"), breaks = seq(0, breaks_max, by = 300),
                       limits = c(0, max(plot_data$predicted) + ylimits_sum)) +
    scale_color_manual(values = c("gold1", "deepskyblue3"),
                       labels = c("Male", "Female"),
                       name = "Sex:") +
    labs(x = "", y = "", title = title_text)
}
