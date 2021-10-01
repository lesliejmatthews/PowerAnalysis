library(RODBC)
library(broom)
library(purrr)
library(nlme)
library(lmtest)
library(emon)
library(nortest)
library(plotrix)
library(EnvStats)
library(tidyverse)

channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
df_phos <- sqlQuery(channel, "SELECT LakeID, Year, Phos as Value, 'lmpPhos' as 'param' FROM dbo.LayData_AnnualMeans_Sta1 WHERE dayssampled_phos>=8")
df_secc <- sqlQuery(channel, "SELECT LakeID, Year, Secc as Value, 'lmpSecc' as 'param' FROM dbo.LayData_AnnualMeans_Sta1 WHERE dayssampled_secc>=8")
df_chla <- sqlQuery(channel, "SELECT LakeID, Year, Chla as Value, 'lmpChla' as 'param' FROM dbo.LayData_AnnualMeans_Sta1 WHERE dayssampled_chla>=8")
df_spr <- sqlQuery(channel, "SELECT LakeID, Year, SprTP as Value, 'SprTP' as 'param' FROM dbo.SprData_AnnualMeans_Sta1 WHERE SprTP is not null")
odbcClose(channel)
head(df_phos)
head(df_secc)
head(df_chla)
head(df_spr)

# df <- rbind(df_phos, df_secc, df_chla, df_spr) %>%
#   mutate(LakeID=as.character(LakeID)) %>% mutate(param=as.character(param)) %>% mutate(YearC=Year-1980) %>%
#   nest_by(LakeID, param) %>%
#   mutate(nYears=list(nrow(data))) %>% filter(nYears>=5) %>%
#   mutate(mean=list(round(mean(data$Value),1))) %>% mutate(sd=list(round(sd(data$Value),1))) %>%
#   mutate(pvalue=list(round(pvalue(data$Value, data$YearC),4))) %>% mutate(cv=round(sd/mean,3)) %>%
#   mutate(cv_mean=round(sd/(sqrt(sd)*mean),3)) %>%
#   mutate(lm=list(lm(data=data, Value~YearC))) %>%
#   mutate(dw=list(dwtest(lm))) %>% mutate(dw.pvalue=as.numeric(dw[4])) %>%
#   mutate(normality=list(shapiro.test(data$Value))) %>% mutate(normality.pvalue=as.numeric(normality[2])) %>%
#   mutate(lognormality=list(shapiro.test(log(data$Value)))) %>% mutate(lognormality.pvalue=as.numeric(lognormality[2])) %>%
#   mutate(adnormality=(ifelse(nYears>7,list(ad.test(data$Value)), list(-1)))) %>% mutate(adnormality.pvalue=as.numeric(adnormality[2])) %>%
#   mutate(adlognormality=(ifelse(nYears>7,list(ad.test(log(data$Value))), list(-1)))) %>% mutate(adlognormality.pvalue=as.numeric(adlognormality[2])) %>%
#   mutate(cor.test.kendall=list(cor.test(data$Value, data$YearC, alternative="two.sided", exact=FALSE, method="kendall"))) %>%
#   mutate(kendall.pvalue=as.numeric(cor.test.kendall[3])) %>%
#   mutate(emon.mk=list(mannkendall(data$YearC, data$Value, nsims.mk=1000))) %>% mutate(emon.mk.pvalue=as.numeric(emon.mk[1]))
# save(df, file="df.Rdata")
load("df.Rdata")

# significance of tests:  kendall using base r, kendall using emon (iterative), durbin-watson for serial correlation
df_sign <- df[c("LakeID","param", "nYears", "kendall.pvalue", "emon.mk.pvalue", "dw.pvalue", "normality.pvalue", "lognormality.pvalue", "adnormality.pvalue", "adlognormality.pvalue")] %>% mutate(kendall.sign=ifelse(kendall.pvalue<.05, "sign", "n.s.")) %>% mutate(emon.mk.sign=ifelse(emon.mk.pvalue<.05, "sign", "n.s.")) %>% mutate(dw.sign=ifelse(dw.pvalue<.05, "sign", "n.s.")) %>% mutate(normality.sign=ifelse(normality.pvalue<.05, "sign", "n.s.")) %>% mutate(lognormality.sign=ifelse(lognormality.pvalue<.05, "sign", "n.s.")) %>% mutate(adnormality.sign=ifelse(adnormality.pvalue<.05, "sign", "n.s.")) %>% mutate(adlognormality.sign=ifelse(adlognormality.pvalue<.05, "sign", "n.s."))

# counts of lakes with significant distribution tests
nrow(df_sign)  # 437 lake data sets
nrow(subset(df_sign[df_sign$normality.sign=="sign",])) # 82 are not normal shapiro test
nrow(subset(df_sign[df_sign$lognormality.sign=="sign",])) # 46 are not normal shapiro test
nrow(subset(df_sign[df_sign$adnormality.sign=="sign",])) # 174 are not normal anderson-darling test
nrow(subset(df_sign[df_sign$adlognormality.sign=="sign",])) # 154 are not normal

# counts of lmpPhos lakes with significant distribution tests
lmpPhosSign <- subset(df_sign[df_sign$param=="lmpPhos",])  # 61 lakes shapiro test
nrow(subset(lmpPhosSign[lmpPhosSign$normality.sign=="sign",])) # 13 are not normal shapiro test
nrow(subset(lmpPhosSign[lmpPhosSign$lognormality.sign=="sign",])) # 7 are not normal shapiro test
nrow(subset(lmpPhosSign[lmpPhosSign$adnormality.sign=="sign",])) # 22 are not normal anderson-darling test
nrow(subset(lmpPhosSign[lmpPhosSign$adlognormality.sign=="sign",])) # 19  are not normal anderson-darling test

# compare standard deviations of log(lmpPhos) with annual mean versus repeat summer sampling
channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
lmp <- sqlQuery(channel, paste("with cte (LakeID, VisitDate, Phos, [Year], [Month], [Day], [Period]) as
(SELECT [LakeID]
	,[VisitDate]
    ,CASE
		WHEN [Phos] < 5 THEN 5
		ELSE Phos End as 'Phos'
	,year(VisitDate) as 'Year'
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
SELECT LakeID, [Year], [Period], round(avg(Phos),1) as'Phos'
FROM cte
--WHERE Period >= 2 AND Period <= 6 
GROUP BY LakeID, [Year], [Period]
ORDER BY LakeID, [Year], [Period]
"))
odbcClose(channel)

# all data (for comparing with Eric)
lmp %>% mutate(logPhos=log10(Phos)) %>%  summarize(mean=mean(Phos), sd=sd(Phos), se=std.error(Phos), logmean=mean(logPhos),  logsd=sd(logPhos), logse=std.error(logPhos))

# summary statistics grouped by LakeID
lmp_stats <- lmp  %>% mutate(logPhos=log10(Phos)) %>% group_by(LakeID) %>% summarize(mean=mean(Phos), sd=sd(Phos), se=std.error(Phos), logmean=mean(logPhos),  sdlog=sd(logPhos), selog=std.error(logPhos), cv=sd(Phos)/mean(Phos), cvmean=std.error(Phos)/mean(Phos))

# normality of residuals
lmp_annual <- lmp %>% mutate(YearC=Year-1980) %>% group_by(LakeID, YearC) %>% summarize(annual.mean=mean(Phos)) %>% add_count(LakeID) %>% filter(n>=5) %>% ungroup()

normal_residuals <- function(x) { 
  model <- lm(log10(annual.mean)~YearC, data=x)
  shapiro <- shapiro.test(model$residuals)
  pvalue <- shapiro[2]
  return(as.numeric(pvalue))
}
normal_residuals(lmp_annual)

lmp_residuals <- lmp_annual %>% filter(n>=5) %>% nest(-LakeID) %>% group_by(LakeID) %>% mutate(pvalue=purrr::map(data,normal_residuals)) %>% unnest(cols=c(data, pvalue)) %>% select(LakeID, pvalue) %>% distinct() %>% filter(pvalue<.05)

# faceted box plots
periodLabels <- c("Spring","Late May-Early June", "Late June", "Early July", "Late July", "Early August", "Late August", "September")

boxplots <- df_spr %>% inner_join(., lmp) %>% dplyr::select(LakeID, Year, Value) %>% mutate(Phos=Value) %>% mutate(Period=0) %>% dplyr::select(LakeID, Year, Period, Phos) %>% rbind(., lmp)

ggplot(data=boxplots, aes(x=as.factor(Period), y=log10(Phos), group=as.factor(Period))) +
  geom_boxplot(varwidth=T) +
  scale_x_discrete(labels=periodLabels) +
  xlab("Seasonal Period") +
  ylab("log10 Phos (ug/L))") +
  theme_bw() +
  theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust=1))

# one tailed one sampled t test for Willoughby 
# test symmetry
lmp_willo <- lmp_annual %>% filter(LakeID=="WILLOUGHBY") 
t.test(log10(lmp_willo$annual.mean), mu=12, alternative="greater")




plot(x)
    
##############################################        
                       boxplot(data=df, log(Phos)~Period, varwidth=T)




df_nelson <- df[df$LakeID=="FOREST (CALAIS)",]
df_nelson

par(mfrow=c(1,2))
hist(df_phos[df_phos$LakeID=="FOREST (CALAIS)",]$Value, breaks=10)
hist(df_spr[df_spr$LakeID=="FOREST (CALAIS)",]$Value, breaks=10)

df_carmi <- df[df$LakeID=="CARMI",]
df_carmi

par(mfrow=c(1,2))
hist(df_phos[df_phos$LakeID=="CARMI",]$Value, breaks=10)
hist(df_spr[df_spr$LakeID=="CARMI",]$Value, breaks=10)

hist(df_chla[df_chla$LakeID=="CARMI",]$Value, breaks=10)

# power.trend for Nelson pond spring p

df_pwr <- df_nelson %>% filter(param=="SprTP") %>% unnest_wider(data) 
xvalues <- unlist(df_pwr$YearC)
meanvalues <- unlist(df_pwr$Value)
sd <- as.numeric(df_pwr$sd)
method <- "mk"
alpha <- 0.05
nsims <- 1000
nsims.mk <- 1000
distribution <- "Normal"

power.trend(xvalues=xvalues, meanvalues=meanvalues, sd=1, method=method, distribution=distribution, alpha=alpha, nsims=nsims, nsims.mk=nsims.mk)

# Generic
myTrend1 <- generate.trend(nyears=10, mu1=10, change=2, type="linear", change.type="A") # A is for additive change type
plot(myTrend1$i, myTrend1$mu, type="o", pch=16, xlab='Time', ylab='Y', ylim=c(0,20))

df_spr_output <- df %>% filter(param=="SprTP") %>% mutate(mean=unlist(mean)) %>% mutate(sd=unlist(sd)) %>% 
  mutate(pvalue=cor.test.kendall[3]) %>%
dplyr::select(LakeID, mean, sd, pvalue)
min(df_spr_output$sd)
max(df_spr_output$sd)
median(df_spr_output$sd)

power.trend(xvalues=myTrend1$i, meanvalues=myTrend1$mu, sd=3, method=method, distribution=distribution, alpha=alpha, nsims=nsims, nsims.mk=nsims.mk)





####################################
# lmp data with all dates

channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
lmp <- sqlQuery(channel, "SELECT LakeID, LakeStationNo, year(VisitDate) as 'Year', VisitDate, Phos as 'lmpPhos' FROM dbo.LayData_DailyMeans WHERE dayssampled_phos>=8 AND Phos is not null")
odbcClose(channel)

lmp_phos <- lmp %>% mutate(LakeID=as.character(LakeID)) %>% nest_by(LakeID, LakeStationNo, Year) %>% group_by(LakeID,LakeStationNo) %>% add_count(LakeID, LakeStationNo) %>% ungroup() %>% filter(n>=5) %>% unnest(data) %>% rename(nYears=n) %>% nest_by(LakeID, LakeStationNo, nYears) %>% mutate(lm=list(lm(data=data, lmpPhos~VisitDate))) %>% mutate(dw=list(dwtest(lm)))  %>% mutate(dw.pvalue=as.numeric(dw[4])) 

print(lmp_phos[c("LakeID", "LakeStationNo", "dw.pvalue")] %>% filter(dw.pvalue<.05) %>% arrange(LakeID), n=100)







