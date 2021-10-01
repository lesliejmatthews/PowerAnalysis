library(RODBC)
library(goft)
library(tidyverse)
library(dplyr)
library(broom)
library(pwr)
library(EnvStats)
library(scales)
library(ggpubr)
library(car)
library(lubridate)
library(plotly)


channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
lmp_d <- sqlQuery(channel, "SELECT LakeID, VisitDate, Year(VisitDate) as 'Year', Phos, DaysSampled_Phos  FROM 
                      dbo.LayData_DailyMeans WHERE Phos is not NULL AND LakeStationNo='1'")
odbcClose(channel)

lmp <- lmp_d %>% mutate(logPhos=log(Phos))

curtis <- lmp %>% filter(LakeID=="CURTIS") %>% arrange(VisitDate)

# Box plots by year
ggboxplot(curtis, x = "Year", y = "logPhos", 
          color = "Year", ylab = "logPhos", xlab = "Year")

# Mean plots
ggline(curtis, x = "Year", y = "logPhos", 
       add = c("mean_se", "jitter"), 
       ylab = "logPhos", xlab = "Year")

ggline(curtis, x="VisitDate", y="logPhos")

plot_ly(curtis) %>%
    add_trace(x=~VisitDate, y=~logPhos, mode='lines+markers')

curtis_annual <- curtis %>% group_by(Year) %>% summarize(meanPhos=mean(Phos))

plot(meanPhos ~ Year, data=curtis_annual)

