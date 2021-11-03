# Preparation
library(rmarkdown,knitr,ggplot2)
library(knitr)
library(readxl)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(plotrix)
library(multcomp)
library(knitr)
library(googlesheets)
library(dplyr)
library(tidyr)

theme_set(theme_bw())

# Inport data
Duty_RAW <- as.data.frame(read_excel("/Users/Ryohei/Downloads/当直表 201704-201803.xlsx",
                                  "印刷",
                                  col_names=TRUE, col_types=NULL, na='', skip=0))

# Form
colnames(Duty_RAW) <- c("A","Start_Date","Day","DayShift","Oncall_Day","NightShift","Oncall_Night","B","C")
Duty_RAW$Start_Date <- as.Date(Duty_RAW$Start_Date)

Duty_L <-
  Duty_RAW %>%
  select(Start_Date,DayShift,Oncall_Day,NightShift,Oncall_Night) %>%
  mutate(Start_Date = as.Date(Start_Date)) %>%
  filter(NightShift!=0 & NightShift!="当直") %>% 
  gather(Duty,Name,-Start_Date) %>%
  filter(Start_Date > "2017-09-30" )


# Install gsubfn
install.packages("gsubfn")
library(gsubfn)
Start_Time <- gsubfn("DayShift|NightShift|Oncall_Day|Oncall_Night",
                     list(DayShift="9:00",NightShift="17:00",Oncall_Day="9:00",Oncall_Night="17:00"),
                     as.character(Duty_L$Duty))
End_Time <- gsubfn("DayShift|NightShift|Oncall_Day|Oncall_Night",
                   list(DayShift="17:00",NightShift="07:30",Oncall_Day="17:00",Oncall_Night="07:30"),
                   as.character(Duty_L$Duty))
End_Date <- c(subset(Duty_L,Duty=="DayShift"|Duty=="Oncall_Day")$Start_Date,
              subset(Duty_L,Duty=="NightShift"|Duty=="Oncall_Night")$Start_Date+1)
Duty <- cbind.data.frame(Duty_L,Start_Time,End_Time,End_Date)
Chiba_PS <- levels(as.factor(Duty$Name))


for(i in 1:length(Chiba_PS)){
  PS <- levels(as.factor(Duty$Name))[i]
  Duty_One <- subset(Duty,Name==PS)[,-3]
  colnames(Duty_One) <- c("Start Date", "Subject", "Start Time", "End Time", "End Date")
  write.csv(Duty_One, file=paste(PS,"Duty2017_H2.csv",sep="_"), fileEncoding = "CP932")
}

# Google calenderとCalendar.appを同期
http://pc-karuma.net/sync-mac-app-calendar-google/