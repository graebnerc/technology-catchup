suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(here))
load("data/intermediate_data.RData")

plot_complexity <- data_reg_1985_2014 %>%
  dplyr::group_by(ccode) %>% 
  dplyr::summarize(
    eci_Mean = mean(eci, na.rm=TRUE), 
    Penn_GDP_PPP_log_Mean = mean(Penn_GDP_PPP_log, na.rm=TRUE), 
    .groups="drop") %>%
  ggplot(data=.,
         aes(x=eci_Mean, y=Penn_GDP_PPP_log_Mean)
         ) +
  geom_point() +
  geom_smooth(method="lm", formula = 'y~x') +
  labs(
    title = "Economic complexity and GDP per capita, average 1985-2014",
    x = "Economic Complexity Index", 
    y = "GDP per capita (log)", 
    caption = "Sources: Penn World Tables (v9.0), The Atlas of Economic Complexity (2019); own calculations."
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_blank(),
    plot.title = element_text(size=14),
    axis.text = element_text(size=13),
    axis.title = element_text(size=13)
  )
plot_complexity

ggsave(plot = plot_complexity, 
       filename = here("output/Fig_1-ECI-GDP.pdf"), 
       width = 8, height = 5)
