library(RODBC)
library(goft)
library(tidyverse)
library(broom)
library(pwr)
library(EnvStats)
library(scales)

channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
lmp <- sqlQuery(channel, paste("with cte (LakeID, VisitDate, Phos, [Year], YearC, [Month], [Day], [Period]) as
(SELECT [LakeID]
	,[VisitDate]
    ,CASE
		WHEN [Phos] < 5 THEN 5
		ELSE Phos End as 'Phos'
	,year(VisitDate) as 'Year'
	,year(VisitDate)-1980 as 'YearC'
	,month(VisitDate) as 'Month'
	,day(VisitDate) as 'Day'
	,CASE 
		WHEN (month(VisitDate)=6 AND day(VisitDate) <= 15) OR (month(VisitDate)=5) THEN 1
		WHEN month(VisitDate)=6 AND day(VisitDate) > 15 THEN 2
		WHEN month(VisitDate)=7 AND day(VisitDate) <= 15 THEN 3
		WHEN month(VisitDate)=7 AND day(VisitDate) > 15 THEN 4
		WHEN month(VisitDate)=8 AND day(VisitDate) <= 15 THEN 5
		WHEN month(VisitDate)=8 AND day(VisitDate) > 15 THEN 6
		WHEN month(VisitDate)=9 THEN 7
		ELSE 100 END AS 'Period'
  FROM [WQData].[dbo].[LayData_DailyMeans]
  where phos is not null and lakestationno='1' and year(VisitDate) >= 1980
  --and LakeID='WILLOUGHBY'
  )
SELECT LakeID, [Year], YearC, [Period], round(avg(Phos),1) as'Phos'
FROM cte
--WHERE Period >= 2 AND Period <= 6 
GROUP BY LakeID, [Year], YearC, [Period]
ORDER BY LakeID, [Year], [Period]
"))
odbcClose(channel)

# faceted box plots by period
periodLabels <- c("Spring","Late May-Early June", "Late June", "Early July", "Late July", "Early August", "Late August", "September")

ggplot(data=lmp, aes(x=as.factor(Period), y=(Phos), group=as.factor(Period))) +
  geom_boxplot(varwidth=T) +
  scale_x_discrete(labels=periodLabels) +
  scale_y_log10(breaks=c(5,seq(10,50,10),100,300)) +
  xlab("Seasonal Period") +
  ylab("Phos ug/L") +
  theme_bw() +
  theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust=1))

# calculate annual means, minimum three years data
lmp_annual <- lmp %>% group_by(LakeID, YearC) %>% summarize(annual.mean=mean(Phos)) %>% add_count(LakeID) %>% filter(n>=3) %>% ungroup()

# which lakes are normally distributed, log normally distributed, or neither
# normal distribution
lmp_normal_or_not <- lmp_annual %>% group_by(LakeID, n) %>% summarize(mean=mean(annual.mean), sd = sd(annual.mean), normal=list(tidy(shapiro.test(annual.mean)))) %>% unnest_wider(c(normal)) %>% mutate(cv=round(sd/mean,2)) %>% mutate(normal.p.value=round(p.value,4)) %>% dplyr::select(LakeID, n, mean, sd, cv, normal.p.value) %>% mutate(normal=ifelse(normal.p.value<.05, "NOT NORMAL", "normal"))

# select only lakes that are normally distributed
lmp_normal <- lmp_normal_or_not %>% filter(normal=="normal")
lmp_not_normal <- lmp_normal_or_not %>% filter(normal=="not normal")

# what is the average coefficient of variation for these lakes?
lmp_normal_cv <- mean(lmp_normal$cv)

# what is the average standard deviation for these lakes?
lmp_normal_sd <- mean(lmp_normal$sd)

# what is the 95% confidence interval for the standard deviation?
lm.fit <- lm(annual.mean~YearC, data=lmp_annual)
summary(lm.fit.miles)
sqrt(enorm(lm.fit.miles$residuals, ci = TRUE, ci.param = "variance")$interval$limits)

# plot number of years versus standard deviation
ggplot(data=lmp_normal, aes(x=n, y=sd)) +
  geom_point() +
  geom_smooth(method="lm") +
  ylab("Standard Deviation") +
  xlab("Number of Years Sampled") +
  theme_bw()

# plot Total Phosphorus mean versus standard deviation
ggplot(data=lmp_normal, aes(x=mean, y=sd)) +
  geom_point() +
  geom_smooth(method="lm") +
  ylab("Standard Deviation") +
  xlab("Total Phosphorus, ug/L") +
  theme_bw()

# power test, one-sample, one-tailed t-test, MILES example
lmp_annual_miles <- lmp_annual %>% filter(LakeID=="MILES")
lmp_normal_miles <- lmp_normal %>% filter(LakeID=="MILES") # n = 13 years, mean=11.4 ug/L, sd=2.05

# what is the 95% confidence interval for the standard deviation?
lm.fit.miles <- lm(annual.mean~YearC, data=lmp_annual_miles)
summary(lm.fit.miles)
sqrt(enorm(lm.fit.miles$residuals, ci = TRUE, ci.param = "variance")$interval$limits)
# 1.326307 to 3.053164 is the range of standard deviation

# samples needed given, stdev=3.05 (Miles Pond) to detect a change of X ug/L
detect.limit <- seq(.5,5,.5)
samples <- numeric()
for (i in detect.limit) {
   test <- tTestN(power=0.8, delta.over.sigma = i/3.05, alpha=0.05, alternative="greater") 
   samples <- c(samples, test) 
}
samples.miles <- as.data.frame(cbind(detect.limit, samples))

ggplot(data=samples.miles, aes(x=as.factor(detect.limit), y=samples, group=1)) +
  geom_point(color="blue", size=1.5) +
  #geom_line(color="blue", size=1) +
  stat_summary(fun=sum, geom="line", color="blue", size=1) +
  xlab("Minimum Detectable Increase in TP ug/L") +
  ylab("Number of Samples") +
  scale_x_discrete(breaks=seq(0,5,.5)) +
  scale_y_continuous(breaks=seq(0,240, 20)) +
  theme_bw()

# samples needed given detection limit, over a range of std deviations
detect.limit <- seq(1,5,.2)
st.dev <- seq(1,7,1)
output <- data.frame(min.detect=numeric(), st.dev=numeric(), samples=numeric())
for (i in detect.limit) {
  for (j in st.dev) {
   test <- tTestN(power=0.8, delta.over.sigma = i/j, alpha=0.05, alternative="greater") 
   output[nrow(output)+1,] <- c(i,j,test)
  }
}

p <- ggplot(data=output, aes(x=as.factor(min.detect), y=samples, group=as.factor(st.dev))) +
  geom_point(size=1.5, aes(color=as.factor(st.dev))) +
  geom_line(size=1, aes(color=as.factor(st.dev))) +
  xlab("Minimum Detectable Increase in TP ug/L") +
  ylab("Number of Samples") +
  guides(color = guide_legend(reverse=TRUE, title="Standard\nDeviation" )) +
  scale_y_continuous(breaks=seq(0,600,20)) +
  theme_bw() 
p
unique(ggplot_build(p)$data[[1]][1])

#############################################
# Repeat using all 8 samples per summer
#############################################

# which lakes are normally distributed, log normally distributed, or neither
# normal distribution
lmp8_normal_or_not <- lmp  %>% group_by(LakeID) %>% add_count() %>% filter(n>=3) %>% group_by(LakeID, n) %>% summarize(mean=mean(Phos), sd = sd(Phos), normal=list(tidy(shapiro.test(Phos))))  %>% unnest_wider(c(normal)) %>% mutate(cv=round(sd/mean,2)) %>% mutate(normal.p.value=round(p.value,4)) %>% dplyr::select(LakeID, n, mean, sd, cv, normal.p.value) %>% mutate(normal=ifelse(normal.p.value<.05, "NOT NORMAL", "normal"))



# select only lakes that are normally distributed
lmp_normal <- lmp_normal_or_not %>% filter(normal=="normal")
lmp_not_normal <- lmp_normal_or_not %>% filter(normal=="not normal")

# what is the average coefficient of variation for these lakes?
lmp_normal_cv <- mean(lmp_normal$cv)

# what is the average standard deviation for these lakes?
lmp_normal_sd <- mean(lmp_normal$sd)

# what is the 95% confidence interval for the standard deviation?
lm.fit <- lm(annual.mean~YearC, data=lmp_annual)
summary(lm.fit.miles)
sqrt(enorm(lm.fit.miles$residuals, ci = TRUE, ci.param = "variance")$interval$limits)

# plot number of years versus standard deviation
ggplot(data=lmp_normal, aes(x=n, y=sd)) +
  geom_point() +
  geom_smooth(method="lm") +
  ylab("Standard Deviation") +
  xlab("Number of Years Sampled") +
  theme_bw()

# plot Total Phosphorus mean versus standard deviation
ggplot(data=lmp_normal, aes(x=mean, y=sd)) +
  geom_point() +
  geom_smooth(method="lm") +
  ylab("Standard Deviation") +
  xlab("Total Phosphorus, ug/L") +
  theme_bw()

# power test, one-sample, one-tailed t-test, MILES example
lmp_annual_miles <- lmp_annual %>% filter(LakeID=="MILES")
lmp_normal_miles <- lmp_normal %>% filter(LakeID=="MILES") # n = 13 years, mean=11.4 ug/L, sd=2.05

# what is the 95% confidence interval for the standard deviation?
lm.fit.miles <- lm(annual.mean~YearC, data=lmp_annual_miles)
summary(lm.fit.miles)
sqrt(enorm(lm.fit.miles$residuals, ci = TRUE, ci.param = "variance")$interval$limits)
# 1.326307 to 3.053164 is the range of standard deviation

# samples needed given, stdev=3.05 (Miles Pond) to detect a change of X ug/L
detect.limit <- seq(.5,5,.5)
samples <- numeric()
for (i in detect.limit) {
  test <- tTestN(power=0.8, delta.over.sigma = i/3.05, alpha=0.05, alternative="greater") 
  samples <- c(samples, test) 
}
samples.miles <- as.data.frame(cbind(detect.limit, samples))

ggplot(data=samples.miles, aes(x=as.factor(detect.limit), y=samples, group=1)) +
  geom_point(color="blue", size=1.5) +
  #geom_line(color="blue", size=1) +
  stat_summary(fun=sum, geom="line", color="blue", size=1) +
  xlab("Minimum Detectable Increase in TP ug/L") +
  ylab("Number of Samples") +
  scale_x_discrete(breaks=seq(0,5,.5)) +
  scale_y_continuous(breaks=seq(0,240, 20)) +
  theme_bw()

# samples needed given detection limit, over a range of std deviations
detect.limit <- seq(1,5,.2)
st.dev <- seq(1,7,1)
output <- data.frame(min.detect=numeric(), st.dev=numeric(), samples=numeric())
for (i in detect.limit) {
  for (j in st.dev) {
    test <- tTestN(power=0.8, delta.over.sigma = i/j, alpha=0.05, alternative="greater") 
    output[nrow(output)+1,] <- c(i,j,test)
  }
}

p <- ggplot(data=output, aes(x=as.factor(min.detect), y=samples, group=as.factor(st.dev))) +
  geom_point(size=1.5, aes(color=as.factor(st.dev))) +
  geom_line(size=1, aes(color=as.factor(st.dev))) +
  xlab("Minimum Detectable Increase in TP ug/L") +
  ylab("Number of Samples") +
  guides(color = guide_legend(reverse=TRUE, title="Standard\nDeviation" )) +
  scale_y_continuous(breaks=seq(0,600,20)) +
  theme_bw() 
p
unique(ggplot_build(p)$data[[1]][1])


# can we do better by including more samples - use a mixed effects model with month as a random effect and year as a fixed effect
# graph of data faceted by Period
lmp_mean <- lmp %>% group_by(LakeID) %>% summarize(mean=mean(Phos) ) 
lmp_graph <- dplyr::inner_join(lmp, lmp_mean, by="LakeID") %>% filter(mean<40) %>% mutate(group=ifelse(mean<10, "5-10", ifelse(mean>=10 & mean<15, "10-15", ifelse(mean>=15 & mean<20, "20-30", "30-40"))))
lmp_graph <- within(lmp_graph, group <- ordered(group, levels =  c("30-40", "20-30", "10-20", "5-10")))
ggplot(data=lmp_graph, aes(x=Year, y=Phos, group=as.factor(LakeID))) +
  geom_point(size=1.5, aes(color=as.factor(LakeID))) +
  geom_smooth(method="lm", se=F, aes(color=as.factor(LakeID))) +
  xlab("Year") +
  ylab("TP ug/L") +
  scale_x_continuous(breaks=c(1980, 2000, 2020)) +
  scale_y_log10(breaks=seq(10,100, 10)) +
  facet_wrap(~Period, ncol=7) +
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.line=element_line())







lmp_P2to6 <- lmp %>% group_by(LakeID) %>% add_count() %>% mutate(HasP1=any(Period==1), HasP2=any(Period==2), HasP3=any(Period==3), HasP4=any(Period==4), HasP5=any(Period==5), HasP6=any(Period==6), HasP7=any(Period==7))

