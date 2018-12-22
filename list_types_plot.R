# list_types_plot.R
# Yimeng Li
# This script contains a function that obtains two figures showing the estimated proportions of different types of respondents
# (1) under the no design effect assumption
# (2) under the no design effect assumption and the relaxed liars assumption with a maximal number of liars
# conditional on the number of control items answered affirmatively.
# As an example, see Figure 1 in "Relaxing the No Liars Assumption in List Experiment Analyses".
# This version: Dec 18, 2018.

# function list_types_plot
# Inputs:
# (1) output.list_relaxed_liars: Output from function list_relax_liars
# Outputs: A list containing estimates for
# (1) Proportions of truthtellers and nonsupporters/liars,
#       conditional on the number of control items answered affirmatively
# (2) Proportions of truthtellers, max liars, and min nonsupporters,
#       conditional on the number of control items answered affirmatively

library(ggplot2)

list_types_plot <- function(output.list_relaxed_liars) {
  
  # Retrive proportions of truth-tellers (pt), liars/non-supporters (pnl),
  # maximal liars (pl_max) and minimal non-supporters (pn_min) under the relaxed liars assumption
  # from function list_relax_liars's output:
  pt = output.list_relaxed_liars$id_props$pt
  pnl = output.list_relaxed_liars$id_props$pnl
  pt = pmax(0, pmin(1, pt))
  pnl = pmax(0, pmin(1, pnl))
  pl_max = output.list_relaxed_liars$pl_max
  pn_min = pnl - pl_max
  
  # Organize the proportions of different types of respondents
  # conditional on the number of control items answered affirmatively
  # to form dataframes:
  df_pt <- data.frame(num_control = seq(0, length(pt)-1),
                      respondent_type = "Truth-telling supporters",
                      cond_prop = pt/(pt+pnl))
  df_pnl <- data.frame(num_control = seq(0, length(pt)-1),
                      respondent_type = "Non-supporters/Liars",
                      cond_prop = pnl/(pt+pnl))
  df_pl_max <- data.frame(num_control = seq(0, length(pt)-1),
                          respondent_type = "Liars",
                          cond_prop = pl_max/(pt+pnl))
  df_pn_min <- data.frame(num_control = seq(0, length(pt)-1),
                          respondent_type = "Non-supporters",
                          cond_prop = pn_min/(pt+pnl))
  df_two_types <- rbind(df_pt, df_pnl)
  df_two_types$respondent_type <- factor(df_two_types$respondent_type,
                                         c("Non-supporters/Liars","Truth-telling supporters"))
  df_three_types <- rbind(df_pt, df_pl_max, df_pn_min)
  df_three_types$respondent_type <- factor(df_three_types$respondent_type,
                                           c("Non-supporters","Liars","Truth-telling supporters"))
  
  # Plot the proportions of different types of respondents
  # conditional on the number of control items answered affirmatively:
  plot_two_types <- ggplot(data=df_two_types, aes(x=num_control, y=cond_prop, fill=respondent_type)) +
    geom_bar(stat="identity", width = 0.5) +
    theme_bw() +
    theme(axis.text = element_text(size = 10)) +
    labs(x = "Number of Control Items", y= "Conditional proportion of different types", fill="Respondent Type") +
    scale_fill_manual(values=c("#CCCCCC", "#000000"))
  
  plot_three_types <- ggplot(data=df_three_types, aes(x=num_control, y=cond_prop, fill=respondent_type)) +
    geom_bar(stat="identity", width = 0.5) +
    theme_bw() +
    theme(axis.text = element_text(size = 10)) +
    labs(x = "Number of Control Items", y= "Conditional proportion of different types", fill="Respondent Type") +
    scale_fill_manual(values=c("#CCCCCC", "#666666", "#000000"))
  
  return(list(plot_two_types = plot_two_types, plot_three_types = plot_three_types))
}