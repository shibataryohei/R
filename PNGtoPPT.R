library(tidyverse)

PATH <- "210427_JSPS2" # PNGファイルのディレクトリ

# 
setwd(PATH) 
dir() %>% 
  file.rename(., gsub("(?<![0-9])([0-9])(?![0-9])",
                      "0\\1",
                      .,
                      perl = TRUE))  # Slide1 -> Slide01
  
dir() %>%
  paste0("![](",PATH,"/", ., ")") %>%
  noquote %>% # patse()の結果から"を除去する
  cat(., sep="\n\n") # \nは改行を表す

---
  output: 
  powerpoint_presentation:
  reference_doc: Template_Rmd.pptx
---