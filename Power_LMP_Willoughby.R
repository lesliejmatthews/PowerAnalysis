library(RODBC)
library(tidyverse)
library(pwr)
library(stats)

channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
lmp_daily <- sqlQuery(channel, "SELECT LakeID, VisitDate, Year(VisitDate) as 'Year', Phos, DaysSampled_Phos  FROM 
                      dbo.LayData_DailyMeans WHERE Phos is not NULL AND LakeStationNo='1'")
odbcClose(channel)

# select willo and calcuate annual means and standard deviations
willo <- lmp_daily %>% filter(LakeID=="WILLOUGHBY") %>% group_by(Year) %>% mutate(annual.mean=mean(Phos), annual.sd=sd(Phos)) %>%
  ungroup()

# normality?
shapiro.test(willo$Phos) # highly significant
shapiro.test(log(willo$Phos)) # highly significant

# create vector of distinct years in willo
years <- willo %>% distinct(Year)

# sample 5 years and then subsample one summer data point per year
# optional - set seed for reproducible sample
set.seed(123)
pvalues <- data.frame(SubsampleNo=numeric(), pvalue=numeric())
for (i in 1:10) {
  for (SubsampleNo in 1:5) {
    as.data.frame(sample(years$Year, size=5, replace=FALSE)) %>% set_names("Year")
    sample.5 <- years.sample %>% inner_join(willo, by="Year") 
    sample.5.1 <- sample.5 %>% group_by(Year) %>% sample_n(SubsampleNo)
    p <- shapiro.test(sample.5.1$annual.mean)$p.value
    pvalues <- rbind(pvalues, c(SubsampleNo, p))
  }
  i= i + 1
}
  
# normality?
shapiro.test(sample.5.2$annual.mean)$p.value # normal annual means - sample of years
shapiro.test(sample.5.2$Phos)$p.value # normal phos - sample of dates

# power one sample, one sided t test for sampled dates
n <- length(sample.5.2$Phos)
mean <- mean(sample.5.2$Phos)
sd <- sd(sample.5.2$Phos)
d <- abs(mean-11)/sd
sig.level <- 0.05
pwr.t.test(n=n, d=d, sig.level=sig.level, type="one.sample", alternative="greater") # 0.2513147

# same thing by simulation
replicate(100, t.test(sample.5.2$Phos, mu=9)$p.value)

mean(replicate(10000, wilcox.test(sample.5.1$Phos, mu=12)$p.value<0.05))

head(willo)

willo_aov <- willo %>% select(Year, Phos) %>% mutate(Year=as.factor(Year))

a <- anova(Phos ~ Year, willo_aov)
summary(a)
BMS <- summary(a)[[1]][[3]][[1]]
WMS <- summary(a)[[1]][[3]][[2]]
