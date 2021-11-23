# Preparation
library(rmarkdown)
library(knitr)
library(readxl)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(plotrix)
library(multcomp)
library(knitr)
library(googlesheets)
library(exactRankTests)
theme_set(theme_bw())

# Inport data
Shift <- as.data.frame(read_excel("/Users/Ryohei/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/F5A3A3AF-DB6F-4025-A5A2-CF1072B3DBAE/??? 201604-201 608 final.xlsx",
           "印刷",
           col_names=TRUE, col_types=NULL, na='', skip=0))

# Form
colnames(Shift) <- c("A","Start_Date","Day","Dayshift","Oncall_Day","Nightshift","Oncall_Night","B","C")
Shift_1 <- Shift[,c(2,4,5,6,7)]
Shift_1$Start_Date <- as.Date(Shift_1$Start_Date)
Shift_2 <- subset(Shift_1,(Nightshift!=0)&(Nightshift!="当直"))

# Transform to Longdata
Shift_L <- melt(Shift_2,id.vars = c("Start_Date"),variable.name = "Subject",value.name = "Name")
head(Shift_L)
# Install gsubfn
library(gsubfn)
Start_Time <- gsubfn("Dayshift|Nightshift|Oncall_Day|Oncall_Night",
                   list(Dayshift="9:00",Nightshift="17:00",Oncall_Day="9:00",Oncall_Night="17:00"),
                   as.character(Shift_L$Subject))
End_Time <- gsubfn("Dayshift|Nightshift|Oncall_Day|Oncall_Night",
                     list(Dayshift="17:00",Nightshift="07:30",Oncall_Day="17:00",Oncall_Night="07:30"),
                     as.character(Shift_L$Subject))
End_Date <- c(subset(Shift_L,Subject=="Dayshift"|Subject=="Oncall_Day")$Start_Date,
              subset(Shift_L,Subject=="Nightshift"|Subject=="Oncall_Night")$Start_Date+1)

Shift <- cbind.data.frame(Shift_L,Start_Time,End_Time,End_Date)
Shift_Shibata <- subset(Shift,Name=="柴田")[,-3]
colnames(Shift_Shibata) <- c("Start Date", "Subject", "Start Time", "End Time", "End Date")
write.csv(Shift_Shibata,file="Shift2016_H1.csv")

# Google calenderとCalendar.appを同期
http://pc-karuma.net/sync-mac-app-calendar-google/
  
Shift <- cbind.data.frame(Shift_L,Start_Time,End_Time,End_Date)
Shift_Harada <- subset(Shift,Name=="原田")[,-3]
Shift_Harada_JulAug <- subset(Shift_Harada,Start_Date >= as.Date("2016-07-01"))
write.csv(Shift_Harada_JulAug,file="Shift_Harada_JulAug.csv")
  





