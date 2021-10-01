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

channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
lmp_month <- sqlQuery(channel, "with cte (LakeID, VisitDate, Phos, [Year], YearC, [Month], [Day], [Period]) as
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
		WHEN month(VisitDate)=5 THEN 'May'
		WHEN month(VisitDate)=6 THEN 'June'
		WHEN month(VisitDate)=7 Then 'July'
		WHEN month(VisitDate)=8 Then 'August'
		WHEN month(VisitDate)=9 THEN 'Sept'
		ELSE '999' END AS 'Month'
  FROM [WQData].[dbo].[LayData_DailyMeans]
  where phos is not null and lakestationno='1' and year(VisitDate) >= 1980
  --and LakeID='WILLOUGHBY'
  )
SELECT LakeID, [Year], YearC, [Period], round(avg(Phos),1) as'Phos'
FROM cte
--WHERE Period >= 2 AND Period <= 6 
GROUP BY LakeID, [Year], YearC, [Period]
ORDER BY LakeID, [Year], [Period]
")
spr <- sqlQuery(channel, "
  )
SELECT [LakeID]
    ,CASE
		WHEN [SprTP] < 5 THEN 5
		ELSE SprTP End as 'SprTP'
	,Year
	,Year-1980 as 'YearC'
  FROM [WQData].[dbo].[SprData]
  where phos is not null and lakestationno='1' and year(VisitDate) >= 1980")
channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
lmp_annual <- sqlQuery(channel, "SELECT LakeID, Year, Year-1980 as 'YearC', Phos FROM dbo.LayData_AnnualMeans_Sta1 WHERE dayssampled_phos>=8")
odbcClose(channel)

channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
lmp_daily <- sqlQuery(channel, "SELECT LakeID, VisitDate, Year(VisitDate) as 'Year', Phos, DaysSampled_Phos  FROM 
                      dbo.LayData_DailyMeans WHERE Phos is not NULL AND LakeStationNo='1'")
odbcClose(channel)


# # faceted box plots by period
# periodLabels <- c("Spring","Late May-Early June", "Late June", "Early July", "Late July", "Early August", "Late August", "September")
# 
# ggplot(data=lmp, aes(x=as.factor(Period), y=(Phos), group=as.factor(Period))) +
#   geom_boxplot(varwidth=T) +
#   scale_x_discrete(labels=periodLabels) +
#   scale_y_log10(breaks=c(5,seq(10,50,10),100,300)) +
#   xlab("Seasonal Period") +
#   ylab("Phos ug/L") +
#   theme_bw() +
#   theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust=1))

# annual means, minimum three years data
lmp_annual_min3 <- lmp_annual %>% group_by(LakeID, YearC) %>% summarize(annual.mean=round(Phos,1)) %>% 
  add_count(LakeID) %>% filter(n>=3) %>% ungroup()

# which lakes are normally distributed, log normally distributed, or neither
# normal distribution
lmp_normal_or_not <- lmp_annual_min3 %>% group_by(LakeID, n) %>% summarize(mean=mean(annual.mean), 
    sd = sd(annual.mean), normal=list(tidy(shapiro.test(annual.mean)))) %>% 
  unnest_wider(c(normal)) %>% mutate(cv=round(sd/mean,2)) %>% mutate(normal.p.value=round(p.value,4)) %>% 
  dplyr::select(LakeID, n, mean, sd, cv, normal.p.value) %>% mutate(normal=ifelse(normal.p.value<.05, "NOT NORMAL", "normal"))

# select only lakes that are normally distributed
lmp_normal <- lmp_normal_or_not %>% filter(normal=="normal")
lmp_not_normal <- lmp_normal_or_not %>% filter(normal=="not normal")

# what is the average coefficient of variation for these lakes?
mean(lmp_normal$cv)

# what is the average standard deviation for these lakes?
mean(lmp_normal$sd)

# plot annual mean Total Phosphorus versus standard deviation
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
lm.fit.miles <- lm(Phos~YearC, data=lmp_annual_miles)
summary(lm.fit.miles)
sqrt(enorm(lm.fit.miles$residuals, ci = TRUE, ci.param = "variance")$interval$limits)
# 1.116735 to 2.676585 is the range of standard deviation

# samples needed given, stdev=2.68 (Miles Pond - pessimistic top of sd conf. int.) to detect a change of X ug/L
detect.limit <- seq(.5,5,.5)
samples <- numeric()
for (i in detect.limit) {
   test <- tTestN(power=0.8, delta.over.sigma = i/2.68, alpha=0.05, alternative="greater") 
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
# get colors used in plot
unique(ggplot_build(p)$data[[1]][1])

# probability of detecting that Miles exceeds 12 ug/L
sample.number <- seq(2,10,1)
output.miles.power <- tTestPower(n.or.n1=sample.number, delta.over.sigma = 12/2.68, alpha=0.05, alternative="greater")
plot.data <- as.data.frame(cbind(sample.number, output.miles.power))
ggplot(data=plot.data, aes(x=sample.number, y=output.miles.power)) +
  geom_point(size=1.5) +
  geom_line(size=1) +
  ylab("Power") +
  xlab("Number of Samples") +
  theme_bw() 

# probability of detecting that a lake is greater than 12 over a range of sample number and std deviation
sample.number <- seq(2,10,1)
st.dev <- seq(1,7,1)
output <- data.frame(sample.number=numeric(), st.dev=numeric(), power=numeric())
for (i in sample.number) {
  for (j in st.dev) {
    test <- tTestPower(n.or.n1=i, delta.over.sigma = 12/j, alpha=0.05, alternative="greater") 
    output[nrow(output)+1,] <- c(i,j,test)
  }
}

p <- ggplot(data=output, aes(x=as.factor(sample.number), y=power, group=as.factor(st.dev))) +
  geom_point(size=1.5, aes(color=as.factor(st.dev))) +
  geom_line(size=1, aes(color=as.factor(st.dev))) +
  xlab("Sample Number (Years)") +
  ylab("Power") +
  guides(color = guide_legend(title="Standard\nDeviation" )) +
  theme_bw() 
p


#############################################
# Repeat using all 8 samples per summer
#############################################

# which lakes are normally distributed, log normally distributed, or neither
# normal distribution
lmp_daily %>% mutate(logPhos=logPhos)

lmp_daily_normal_or_not <- lmp_daily %>% group_by(LakeID) %>% summarize(count=n(), mean=mean(Phos), 
                          sd = sd(Phos)), normal=list(tidy(shapiro.test(Phos)))) %>% 
  unnest_wider(c(normal)) %>% mutate(cv=round(sd/mean,2)) %>% mutate(normal.p.value=round(p.value,4)) %>% 
  dplyr::select(LakeID, n, mean, sd, cv, normal.p.value) %>% mutate(normal=ifelse(normal.p.value<.05, "NOT NORMAL", "normal"))

miles <- lmp_daily %>% filter(LakeID=="MILES") %>% mutate(Year=as.factor(Year), logPhos=log(Phos))  
miles %>% group_by(Year) %>% summarise(count=n(), mean=mean(Phos), sd=sd(Phos))
levels(miles$Year)

ggboxplot(miles, x="Year", y="Phos")
ggboxplot(miles, x="Year", y="logPhos")

miles.aov <- aov(Phos~Year, data=miles)
summary(miles.aov)

# homogeneity of variances plot
plot(miles.aov, 1)

# normality test plot
plot(miles.aov, 2)

# test of Homogeneity of Variance
leveneTest(Phos~Year, data=miles)

# Welch test, no need for equal variance assumption
oneway.test(Phos~Year, data=miles)

# shapiro test
miles.aov.residuals <- residuals(object=miles.aov)
shapiro.test(x=miles.aov.residuals)  # definitely not normal!!!!

# all of the above only with logPhos
miles.aov.log <- aov(log(Phos)~Year, data=miles)
summary(miles.aov.log)

# homogeneity of variances plot
plot(miles.aov.log, 1) # ugh - can't use this model



# normality test plot
plot(miles.aov.log, 2)

# test of Homogeneity of Variance
leveneTest(log(Phos)~Year, data=miles)

# Welch test, no need for equal variance assumption
oneway.test(log(Phos)~Year, data=miles)

# shapiro test
miles.aov.residuals.log <- residuals(object=miles.aov.log)
shapiro.test(x=miles.aov.residuals.log)  # still not normal

# kruskal-wallis test


