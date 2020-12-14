suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(sjPlot))
suppressPackageStartupMessages(library(margins))
suppressPackageStartupMessages(library(here))
options(warn = -1)
load(here("data/intermediate_data.RData"))

# Figure 2 ---------
# Marginal effects at different levels of initial technological capabilities

reg_predict_7 <- lm(
  avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + eci*GDP_pc_PPP_log + 
    popgrowth + humancapital, 
  data=data_reg_predict_1985_2014, na.action=na.exclude)

#note: ci.lvl = ... sets the confidence interval (outer probability or high 
# density probability; see package documentation sjplot)

fig_2 <- plot_model(
  reg_predict_7, type = "pred", 
  terms = c("GDP_pc_PPP_log", "eci [-2, 0, 2]"), ci.lvl=0.9) + 
  font_size(title = 12) +     
  labs(
    y="Predicted average growth in GDP per capita (1985-2014)",
    x="GDP per capita in 1985 (in logs)",
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

# Figure 3 ----
# Figure 3: Marginal effects at different levels of initial GDP per capita

fig_3 <- plot_model(
  reg_predict_7, type = "pred", 
  terms = c("eci", "GDP_pc_PPP_log [7, 8.5, 10]"), 
  ci.lvl=0.9) +
  font_size(title = 12) +     
  labs(
    y="Predicted average growth in GDP per capita (1985-2014)",
    x="Economic Complexity Index in 1985",
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
