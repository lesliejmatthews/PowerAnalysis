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

channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
lmp_d <- sqlQuery(channel, "SELECT LakeID, VisitDate, Year(VisitDate) as 'Year', Phos, DaysSampled_Phos  FROM 
                      dbo.LayData_DailyMeans WHERE Phos is not NULL AND LakeStationNo='1'")
odbcClose(channel)

# which lakes are normally distributed, log normally distributed, or neither
# normal distribution
lmp_d_n3 <- lmp_d %>% mutate(logPhos=log(Phos)) %>%  add_count(LakeID) %>% filter(n>=3)

# are data normally distributed?
lmp_d_normal_or_not <- lmp_d_n3 %>% group_by(LakeID) %>% summarize(n=n(), mean=mean(Phos), 
    sd = sd(Phos), normal=list(tidy(shapiro.test(Phos)))) %>% unnest_wider(c(normal)) %>% 
    mutate(cv=round(sd/mean,2)) %>% mutate(normal.p.value=round(p.value,4)) %>% 
    dplyr::select(LakeID, n, mean, sd, cv, normal.p.value) %>% 
    mutate(normal=ifelse(normal.p.value<.05, "NOT NORMAL", "normal"))  # almost all are not

# try again with log transformed
lmp_d_normal_or_not_log <- lmp_d_n3 %>% group_by(LakeID) %>% summarize(n=n(), mean=mean(logPhos), 
    sd = sd(logPhos), normal=list(tidy(shapiro.test(logPhos)))) %>% unnest_wider(c(normal)) %>% 
    mutate(cv=round(sd/mean,2)) %>% mutate(normal.p.value=round(p.value,4)) %>% 
    dplyr::select(LakeID, n, mean, sd, cv, normal.p.value) %>% 
    mutate(normal=ifelse(normal.p.value<.05, "NOT NORMAL", "normal"))  # almost all are not

# Curtis has total of 295 samples and log-transformed is normally distributed; try it as example
curtis <- lmp_d %>% filter(LakeID=="CURTIS") %>% mutate(Year=as.factor(Year), 
                        Month=as.factor(month(VisitDate)), logPhos=log(Phos))  
levels(curtis$Year)
head(curtis)

# Curtis summary stats
curtis %>% group_by(Year) %>% summarize(mean=mean(logPhos), sd=sd(logPhos))

# Box plots by year
ggboxplot(curtis, x = "Year", y = "logPhos", 
          color = "Year", ylab = "logPhos", xlab = "Year")

# Mean plots
ggline(curtis, x = "Year", y = "logPhos", 
       add = c("mean_se", "jitter"), 
       ylab = "logPhos", xlab = "Year")

curtis.aov <- aov(logPhos ~ Year, data=curtis)
summary(curtis.aov) # there are significant differences between years

# assumptions
# homogeneity of variances assumption
plot(curtis.aov, 1) # three outliers but overall not too bad
# using Levene's test
library(car)
leveneTest(logPhos~Year, data=curtis) # p-value is 0.04 so not great but might be ok

# relax the homogeneity of variance assumption
# Welch one way test
curtis.welch <- oneway.test(logPhos ~ Year, data=curtis)

# normality assumption
plot(curtis.aov, 2) # not terribly bad?

# shapiro-wilkes test
curtis.aov.resid <- residuals(curtis.aov)
shapiro.test(curtis.aov.resid) # barely passes




# kruskal wallis test
kruskal.test(logPhos~Year, data=curtis)

######################################
# Smeltzer paper

# 1. Not necessary (this was for multiple samples per day or visit)

# 2. Tranform the data to natural logs

# 3. Conduct a one-way AOV with groups defined based upon sampling year
# - mean squared deviation among years
# - mean squared deviation within years



# mean squared deviation among years







