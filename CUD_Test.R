library(tidyverse)
library(readxl)
source("/Users/shibataryohei/Git/bgstats/bgstats.R")

read_excel("/Users/shibataryohei/Dropbox/Database/CUD/ACHMC_Mitrofanoff_Monti.xlsx",
           sheet = "Paper") %>% 
  dplyr::rename(Group = 群,
                Age = 手術時年齢,
                Follow = 術後観察期間,
                Sex = 性別,
                Complication = 合併症) %>% 
  mutate(Complication = if_else(Complication == "なし",
                                "Complication-", "Complication+")) %>% 
  mutate(Complication = fct_relevel(Complication, "Complication+")) %>% 
  dplyr::select(RandomID, Group, Age, Sex, Follow, Complication) %>%
  mutate(`Complication/PersonYear` = if_else(Complication == ""))
  mutate_if(is.character, as.factor) -> Data_tbw

Data_tbw %>% 
  # dplyr::select(RandomID, 群, 移動, 太さ, 手術時年齢, 部位, Flap, 術後観察期間) %>% 
  wilcox_fisher_table(.,
                      group = "Group",
                      digits = 3,
                      omit = "RandomID")


Data_tbw %>% 
  group_by(Group, Complication) %>% 
  dplyr::summarise(Count = n()) %>% 
  filter(Complication == "Complication+") %>% 
  inner_join(Data_tbw %>% 
               group_by(Group) %>% 
               dplyr::summarise(Follow_PersonYear = sum(Follow))) %>% 
  mutate(`Case/PersonYear` = Count/Follow_PersonYear)
  
