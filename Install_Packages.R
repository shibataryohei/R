install.packages("tidyverse")
install.packages("pipeR")
install.packages("pacman")

# Document
Document <-
  c("rmarkdown","knitr")

Graphic <-
  c("gridExtra","grid","plotrix",
    "ggrepel","RColorBrewer", "ggraptR", "formattable",
    "webshot",
    "ggbiplot",
    "ggsci")

Data <-
  c("readxl","forcats","magrittr") 

Statistics <-
  c("lme4", "exactRankTests", "tableone", "qvalue",
    "rstatix", "qvalue", "sjPlot", "survival", "randomForest")

Knit <- c("bindrcpp", "carData", "car", "sandwich", "RcmdrMisc", "effects",
          "Rcmdr")

Packages <- c(Document, Graphic, Data, Statistics, Knit)
install.packages(Packages)

install.pa

# devtools
install.packages("devtools")
devtools::install_github("vqv/ggbiplot")
devtools::install_github("sjPlot/devel")

# phyloseq
install.packages('BiocManager')
BiocManager::install("GenomeInfoDb")
BiocManager::install("Rhdf5lib")
BiocManager::install("phyloseq")

library(phyloseq)

# Error
install.packages("webshot")

# Add-hoc
install.packages("devtools")
devtools::install_github("erhard-lab/lfc")
library(lfc)
