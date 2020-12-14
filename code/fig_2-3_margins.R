# Creates figues 2 and 3 from the main text; 
# will be called from within 'regressions.R'

# Figure 2 --------------------------------------------------------------------
# Marginal effects at different levels of initial technological capabilities

reg_predict_7_obj <- reg_predict_7[["reg"]]

#note: ci.lvl = ... sets the confidence interval (outer probability or high 
# density probability; see package documentation sjplot)

fig_2 <- plot_model(
  reg_predict_7_obj, type = "pred", 
  terms = c("GDP_pc_PPP_log", "eci [-2, 0, 2]"), ci.lvl=0.9) + 
  font_size(title = 12) +     
  labs(
    y="GDP per capita in 1985 (in logs)",
    x="Predicted average growth in GDP per capita (1985-2014)",
    title="Predicted values of average GDP per capita growth over 1985-2014"
  ) +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_x_continuous(expand = expansion(add = c(0, 0.1))) +
  scale_y_continuous(expand = expansion()) +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.title.x = element_text(size = 8), 
    axis.text.x = element_text(size = 8), 
    axis.title.y = element_text(size = 8), 
    axis.text.y = element_text(size = 8), 
    legend.position = "bottom")
fig_2
ggsave(plot = fig_2, 
       filename = here("output/fig_2-marginal-effects.pdf"), 
       width = 6, height = 4)

# Figure 3 --------------------------------------------------------------------
# Figure 3: Marginal effects at different levels of initial GDP per capita

fig_3 <- plot_model(
  reg_predict_7_obj, type = "pred", 
  terms = c("eci", "GDP_pc_PPP_log [7, 8.5, 10]"), 
  ci.lvl=0.9) +
  font_size(title = 12) +     
  labs(
    y="GDP per capita in 1985 (in logs)",
    x="Predicted average growth in GDP per capita (1985-2014)",
    title="Predicted values of average GDP per capita growth over 1985-2014"
  ) +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_x_continuous(expand = expansion(add = c(0, 0.1))) +
  scale_y_continuous(expand = expansion()) +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.title.x = element_text(size = 8), 
    axis.text.x = element_text(size = 8), 
    axis.title.y = element_text(size = 8), 
    axis.text.y = element_text(size = 8), 
    legend.position = "bottom")
fig_3

ggsave(plot = fig_3, 
       filename = here("output/fig_3-marginal-effects.pdf"), 
       width = 6, height = 4)
