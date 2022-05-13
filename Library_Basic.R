library(pacman)

# tidyverse
library(tidyverse)
library(pipeR)
p_load(readxl)

select <- dplyr::select
rename <- dplyr::rename
summarise <- dplyr::summarise

# ggplot 
p_load(ggrepel, ggpubr, gridExtra, grid)

theme_set(theme_classic())
theme_update(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank(),
             # legend.key.size = unit(3, "mm"),
             plot.margin = unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

# Document
p_load(rmarkdown, knitr, rmdformats, glue, sjPlot)

# Graphic
p_load(pheatmap,
       plotrix,
       RColorBrewer,
       formattable)

# Data 
p_load(forcats, magrittr) # readxl is included tidyverse but

#  Stats
p_load(lme4, exactRankTests, tableone, qvalue, rstatix)
