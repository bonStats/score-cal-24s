library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(RColorBrewer)

rn_raw <- read_csv("figs/rn-dataset.csv")

rn <- pivot_longer(rn_raw, -all_of("t"), names_to = "protein", values_to = "count") 

plevels = c("X", "E", "XE", "X^*", "P1", "X^*P[1]", "Y", "X^*Y", "Y^*", "P[2]", "Y^*P[2]")
plabels = c(expression(X), expression(E), expression(XE), expression(X^a), expression(P1), expression(X^a~P[1]), expression(Y), expression(X^a~Y), expression(Y^a), expression(P[2]), expression(Y^a~P[2]))

rn %>% mutate(protein = ordered(protein, level = plevels, labels = plabels))

pcolours_all <- RColorBrewer::brewer.pal(11, "Paired")

pcolour <- setNames(pcolours_all[c(3,4,5,1,6,7,8,9,2,10,11)], plevels)

                    