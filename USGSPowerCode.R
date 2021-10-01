library(RODBC)
library(tidyverse)
library(magrittr)
library(pwr)
library(MKpower)
source("Scripts/power.WMW.R")

channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
lmp_daily <- sqlQuery(channel, "SELECT LakeID, VisitDate, Year(VisitDate) as 'Year', Phos, DaysSampled_Phos  FROM 
                      dbo.LayData_DailyMeans WHERE Phos is not NULL AND LakeStationNo='1'")
odbcClose(channel)

# select Curtis pond and calcuate annual means and standard deviations
curtis <- lmp_daily %>% filter(LakeID=="CURTIS") %>% group_by(Year) %>% mutate(annual.mean=mean(Phos), annual.sd=sd(Phos)) %>%
  ungroup()

# normality?
shapiro.test(curtis$Phos) # highly significant
shapiro.test(log(curtis$Phos)) # not significant

# create vector of distinct years in curtis
years <- curtis %>% distinct(Year)

# sample 5 years and then subsample one summer data point per year
# optional - set seed for reproducible sample
set.seed(123)

years.sample <- as.data.frame(sample(years$Year, size=5, replace=FALSE)) %>% set_names("Year")
sample.data <- years.sample %>% inner_join(curtis, by="Year") 
sample.data.5.1 <- sample.data %>% group_by(Year) %>% sample_n(1)

# normality?
shapiro.test(sample.data.5.1$annual.mean)$p.value
shapiro.test(sample.data.5.1$Phos)$p.value
# sometimes yes, sometimes no

# non-parametric power test, one-sample, one-sided
# first what is the current all time mean?
mean(curtis$Phos) # 18.35

# so lets see how many samples we need to see that it's not greater than 19
# geo mean for existing data in sample 
ln.sample <- log(sample.data.5.1$Phos)
ln.target <- log(20)
GMratio <- exp(ln.target-mean(ln.sample)) # 1.18

# non parametric power
source("Scripts/power.WMW.one.sample.R")
power.WMW.one.sample(y=ln.sample, target=ln.target, gmratio=GMratio)

# parametric equivalent (using a sample that happens to be normally distributed using set.seed(123))
pwr.t.test()

