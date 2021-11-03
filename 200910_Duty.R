# Preparation
library(tidyverse)
library(readxl)

# Inport data
read_excel("~/Desktop/当直表 202104-202203確定.xlsx",
           sheet = "印刷",
           col_names = TRUE,
           col_types = NULL,
           na='') %>% 
  dplyr::rename(`Start Date` = `...2`,
                Day = `...3`,
                DayShift = 出番,
                Oncall_Day = 待機,
                NightShift = 当直,
                Oncall_Night = 副直) %>% 
  dplyr::select(`Start Date`, 
                DayShift, Oncall_Day, NightShift, Oncall_Night) %>% 
  filter(NightShift !=0 & NightShift != "当直") %>% 
  gather(Subject, Name, -`Start Date`) %>%
  filter(!is.na(Name)) %>% 
  mutate(`Start Date` = as.Date(`Start Date`)) %>% 
  filter(`Start Date` > "2020-09-30") %>% 
  mutate(`Start Time` = if_else(grepl("Day", Subject), "9:00", "17:00")) %>% 
  mutate(`End Time` = if_else(grepl("Day", Subject), "17:00", "9:00")) %>%
  mutate(`End Date` = if_else(grepl("Day", Subject), `Start Date`, `Start Date`+1)) %>% 
  mutate_if(is.character, as.factor)  -> Duty_tbw


Duty_tbw %>% 
  .$Name %>% levels -> Members

purrr::map(Members, function(x){ Duty_tbw %>% 
    filter(Name == x) %>% 
    mutate(Subject = fct_recode(Subject,
                                "出番" = "DayShift",
                                "待機" = "Oncall_Day",
                                "当直" = "NightShift",
                                "副直" = "Oncall_Night")) %>% 
    dplyr::select(-Name) %>% 
    readr::write_excel_csv(.,
              paste(x,"Duty2021_H2.csv",sep="_"))})

Duty_tbw %>% 
    filter(Name == "柴田") %>% 
    dplyr::select(-Name) %>% 
    readr::write_excel_csv(.,
                           paste("Duty2021_H2.csv"))
