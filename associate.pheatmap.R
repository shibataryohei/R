require(microbiome)
require(tidyverse)
require(pheatmap)

associate.pheatmap <- function(df1, df2){
  intersect(rownames(df1),
            rownames(df2)) -> mutualid
  
  microbiome::associate(df1[mutualid,],
                        df2[mutualid, ],
                        method = "spearman",
                        p.adj.method = "fdr",
                        p.adj.threshold = 0.05) %>% 
    as.tibble %>% 
    dplyr::rename(Q.value = p.adj) -> associate_tbw
  
  associate_tbw %>% 
    dplyr::select(-Q.value) %>% 
    spread(X2, Correlation) %>% 
    c2r("X1") -> correlation_df
  
  associate_tbw %>% 
    dplyr::select(-Correlation) %>% 
    mutate(Q.value = addsd(Q.value)) %>% 
    spread(X2, Q.value) %>% 
    c2r("X1") -> stars_df
  
  stars_df[is.na(stars_df)] <- " "
  
  list(associate_tbw = associate_tbw,
       correlation_df = correlation_df,
       stars_df = stars_df)}