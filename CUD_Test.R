library(tidyverse)
library(readxl)
source("/Users/shibataryohei/Git/bgstats/bgstats.R")

read_excel("/Users/shibataryohei/Dropbox/Database/CUD/ACHMC_Mitrofanoff_Monti.xlsx",
           sheet = "Paper2") %>% 
  dplyr::select(!matches("詳細")) %>% 
  dplyr::select(-ID, -RandomID, -手術日) %>% 
  mutate(症例 = as.factor(症例)) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(合併症 = fct_recode(合併症,
                             "合併症あり" = "あり",
                             "合併症なし" = "なし")) %>% 
  mutate(導尿困難 = fct_recode(導尿困難,
                             "導尿困難あり" = "あり",
                             "導尿困難なし" = "なし")) %>% 
  mutate(性別 = fct_relevel(性別, "女性"),
         脊髄異常 = fct_relevel(脊髄異常, "あり"),
         移動 = fct_relevel(移動, "車椅子"),
         Flap = fct_relevel(Flap, "VR"))   -> Data_tbw

Data_tbw %>% 
  median_percent_table(.,
                       omit = "症例",
                       digits = 1)

Data_tbw %>% 
  wilcox_fisher_table(.,
                  group = "導尿困難",
                  digits = 1,
                  omit = c("症例", "合併症")) %>% 
  dplyr::rename(`p-value2` = P.value) %>% 
  dplyr::select(-SD) -> WF_Stenosis_tbw

Data_tbw %>% 
  wilcox_fisher_table(.,
                      group = "合併症",
                      digits = 1,
                      omit = c("症例", "導尿困難")) %>% 
  dplyr::rename(`p-value1` = P.value) %>% 
  dplyr::select(-SD)  -> WF_Complication_tbw

inner_join(WF_Complication_tbw, WF_Stenosis_tbw) %>% 
  .[c(7,4,2,10,9,11,3,1,6), ] %>% 
  sjPlot::tab_df(.,
                 file = "CUD/Table.doc")


Data_tbw %>% 
  dplyr::select(!is.numeric, -合併症) %>% 
  fisher_CI_table(.,
                  group = "導尿困難",
                  digits = 1,
                  omit = "症例")

Data_tbw %>% 
  dplyr::select(is.numeric, 導尿困難) %>% 
  wilcox_CI_table(.,
                      group = "導尿困難",
                      digits = 1)

Data_tbw %>% 
  dplyr::select(!is.numeric, -合併症) %>% 
  fisher_CI_table(.,
                  group = "導尿困難",
                  digits = 1,
                  omit = "症例")

Data_tbw %>% 
  group_by(Group, Complication) %>% 
  dplyr::summarise(Count = n()) %>% 
  filter(Complication == "Complication+") %>% 
  inner_join(Data_tbw %>% 
               group_by(Group) %>% 
               dplyr::summarise(Follow_PersonYear = sum(Follow))) %>% 
  mutate(`Case/PersonYear` = Count/Follow_PersonYear)
  
