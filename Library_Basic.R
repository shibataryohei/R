library(tidyverse)
library(pipeR) # for %>>%
library(pacman)


# Document
p_load(rmarkdown, knitr, rmdformats, ggpubr)

# Graphic
p_load(gridExtra,
       grid,plotrix,
       ggrepel,
       RColorBrewer,
       formattable)

# theme_set(theme_bw())
theme_set(theme_classic())
theme_update(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank(),
             # legend.key.size = unit(3, "mm"),
             plot.margin=unit(c(0,0,0,0),"mm"))
  

# Data 
p_load(readxl, forcats, magrittr, pipeR) # readxl is included tidyverse but

#  Stats
p_load(lme4, exactRankTests, tableone, qvalue, rstatix)

