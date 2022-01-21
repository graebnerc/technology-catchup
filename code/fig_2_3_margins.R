suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(sjPlot))
suppressPackageStartupMessages(library(margins))
suppressPackageStartupMessages(library(here))
options(warn = -1)
load(here("data/intermediate_data.RData"))

data_reg_predict_1985_2014_use <- data_reg_predict_1985_2014 %>%
  rename(ECI=eci, 
         `GDP pc (PPP, log)` = GDP_pc_PPP_log)
# Figure 2 ---------
# Marginal effects at different levels of initial technological capabilities

reg_predict_7 <- lm(
  avg_GDP_pc_PPP_growth ~ `GDP pc (PPP, log)` + kof_econ + 
    ECI*`GDP pc (PPP, log)` + popgrowth + humancapital + inv_share, 
  data=data_reg_predict_1985_2014_use, na.action=na.exclude)

#note: ci.lvl = ... sets the confidence interval (outer probability or high 
# density probability; see package documentation sjplot)

fig_2 <- plot_model(
  reg_predict_7, type = "pred", 
  terms = c("GDP pc (PPP, log)", "ECI [-2, 0, 2]"), ci.lvl=0.9) + 
  font_size(title = 12) +    
  labs(
    title = "Marginal effect of GDP in 1st year",
    x = "GDP per capita in 1985 (in logs)",
    y = "Predicted average growth in GDP pc (1985-2014)"
  ) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill")) +
  scale_x_continuous(expand = expansion(add = c(0, 0.1))) +
  scale_y_continuous(expand = expansion()) +
  theme(
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(),
    axis.title.x = element_text(size = 8), 
    axis.text.x = element_text(size = 8), 
    axis.title.y = element_text(size = 8), 
    axis.text.y = element_text(size = 8), 
    legend.position = "bottom")
fig_2

# Figure 3 ----
# Figure 3: Marginal effects at different levels of initial GDP per capita

fig_3 <- plot_model(
  reg_predict_7, type = "pred", 
  terms = c("ECI", "GDP pc (PPP, log) [7, 8.5, 10]"), 
  ci.lvl=0.9) +
  font_size(title = 12) +     
  labs(
    title = "Marginal effect of ECI",
    x = "Economic Complexity Index",
    y = "Predicted average growth in GDP pc (1985-2014)"
  ) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill")) +
  scale_x_continuous(expand = expansion(add = c(0, 0.1))) +
  scale_y_continuous(expand = expansion()) +
  theme(
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(),
    axis.title.x = element_text(size = 8), 
    axis.text.x = element_text(size = 8), 
    axis.title.y = element_text(size = 8), 
    axis.text.y = element_text(size = 8), 
    legend.position = "bottom")
fig_3

fig23 <- ggpubr::ggarrange(
  fig_2, fig_3, ncol = 2, labels = c("A)", "B)"), 
  common.legend = FALSE, legend = "bottom")

fig23 <- ggpubr::annotate_figure(fig23,
  top = ggpubr::text_grob("Predicted average GDP pc growth over 1985-2014", size = 14))

ggsave(plot = fig23, 
       filename = here("output/fig_23-predictions.pdf"), 
       width = 9, height = 4)
