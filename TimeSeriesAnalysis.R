library(RODBC)
library(tidyverse)
library(lubridate)
library(zoo)
library(lme4)
library(EnvStats)

# set strings as factors to false
options(stringsAsFactors = FALSE)

channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
lmp_d <- sqlQuery(channel, "SELECT LakeID, VisitDate, Year(VisitDate) as 'Year', Phos, DaysSampled_Phos  FROM 
                      dbo.LayData_DailyMeans WHERE Phos is not NULL AND LakeStationNo='1'")
odbcClose(channel)

curtis <- lmp_d %>% filter(LakeID=="CURTIS") %>% mutate(Year=as.factor(Year),
                    Month=as.factor(month(VisitDate)), Week=as.factor(week(VisitDate)), logPhos=log(Phos)) %>% 
                    add_count(Week) %>% filter(n >= 8)
 
curtis.annmeans <- curtis %>% group_by(Year) %>% summarize(AnnMean = mean(Phos))

sd(curtis.annmeans$AnnMean)  #3.45
mean(curtis.annmeans$AnnMean) #18

sd(curtis$Phos) # 6.28

library(stats)


power.t.test(n=25, sd=3.45, sig.level=0.05, delta=1, alternative="one.sided", type="one.sample") #41%

power.t.test(n=3295, sd=6.28, sig.level=0.05, delta=1, alternative="one.sided", type="one.sample")

hist(curtis$Phos)
hist(log(curtis.annmeans$AnnMean))

kruskal.test(data=curtis, Phos ~Year)
head(curtis)

ggplot(curtis, aes(x=VisitDate, y=logPhos)) +
  geom_point() +
  facet_wrap(~Week)

ggboxplot(curtis, x="Week", y="logPhos", varwidth=T)

ggboxplot(curtis, x="Year", y="logPhos", varwidth=T)

lm1 <- lm(logPhos~Year, data=curtis)
anova(lm1)
