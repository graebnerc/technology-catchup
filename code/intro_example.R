library(tidyverse)
library(haven)
library(here)
maddison <- haven::read_dta(here::here("data/mpd2020.dta")) 

rich_poor_1776 <- max(filter(maddison, year==1776)[["gdppc"]])/
  min(filter(maddison, year==1776)[["gdppc"]])
rich_poor_2018 <- max(filter(maddison, year==2018)[["gdppc"]])/
  min(filter(maddison, year==2018)[["gdppc"]])
