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
  c("lme4", "exactRankTests", "tableone", "qvalue")

Knit <- c("bindrcpp", "carData", "car", "sandwich", "RcmdrMisc", "effects",
          "Rcmdr")

Packages <- c(Document, Graphic, Data, Statistics, Knit)
install.packages(Packages)

# devtools
install.packages("devtools")
devtools::install_github("vqv/ggbiplot")

# phyloseq
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("phyloseq")


# 2019/07/31
install.packages('BiocManager')
