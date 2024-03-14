### Purpose of this code is *both* to plot the data and to dump the associated data
###

library(tidyverse)
library(mgcv)
library(gratia)

here::i_am("R/007_plot.R")


model_file <- here::here("working",
                         "dail_model_all.rds")

mod <- readRDS(model_file)

sm <- smooth_estimates(mod, smooth = "s(age)") 

saveRDS(sm, file = here::here("outputs", "age_smooth_plot_data.rds"))
