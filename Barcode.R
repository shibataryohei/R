
library(tidyverse)
data.frame( R1 = rep(c(paste("SA", rep(501:508, each=12), sep=""),
                       paste("SB", rep(501:508, each=12), sep="" )) ,
                     2) ,
            R2 = c(paste("SA", rep(701:712, 8), sep=""),
                   paste("SB", rep(701:712, 16), sep="" ),
                   paste("SA", rep(701:712, 8), sep="")) ,
            MiSeqID = formatC(1:384, width=3, flag="0")) %>%
  tbl_df() -> Barcode