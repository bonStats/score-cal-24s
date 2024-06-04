library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(RColorBrewer)

lv_raw <- read_csv("figs/lv-dataset.csv")

lv <- pivot_longer(lv_raw, -all_of("t"), names_to = "Species", values_to = "Value") 


                    