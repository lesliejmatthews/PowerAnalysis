library(RODBC)
library(tidyverse)
library(broom)
library(purrr)
library(nlme)
library(lmtest)

channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
df <- sqlQuery(channel, paste("with cte (LakeID, VisitDate, Phos, [Year], [Month], [Day], [Period]) as
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
  and LakeID='Seymour'
  )
SELECT LakeID, [Year], [Period], round(avg(Phos),1) as'Phos'
FROM cte
--WHERE Period >= 2 AND Period <= 6 
GROUP BY LakeID, [Year], [Period]
ORDER BY LakeID, [Year], [Period]
"))
odbcClose(channel)
head(df)                      

boxplot(data=df, log(Phos)~Period, varwidth=T)

df_annual <- df %>%  mutate(YearC=Year-1980) %>% group_by(LakeID, Year, YearC)  %>% summarize( nPhos=n(),meanPhos = round(mean(Phos),1), sdPhos = round(sd(Phos),1)) %>% mutate(cv=round(100*sdPhos/meanPhos,1)) 

hist(df_annual$meanPhos, breaks=10)
hist(log(df_annual$meanPhos), breaks=10)

df_corr <- df_annual %>% group_by(LakeID) %>% summarize(n=n(), r=round(cor(log(meanPhos), Year),2)) 

df_lm <- df_annual %>% group_by(LakeID) %>%  do(model = lm(log(meanPhos) ~ YearC, data = .))
 
model <- lm(data=df_annual, log(meanPhos)~YearC)
glance(model)
tidy(model)

############################################################
# cumulative distribution of CV

channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
df_phos <- sqlQuery(channel, "SELECT LakeID, Year, Phos as Mean, 'lmpPhos' as 'param' FROM dbo.LayData_AnnualMeans_Sta1 WHERE dayssampled_phos>=8")
df_secc <- sqlQuery(channel, "SELECT LakeID, Year, Secc as Mean, 'lmpSecc' as 'param' FROM dbo.LayData_AnnualMeans_Sta1 WHERE dayssampled_secc>=8")
df_chla <- sqlQuery(channel, "SELECT LakeID, Year, Chla as Mean, 'lmpChla' as 'param' FROM dbo.LayData_AnnualMeans_Sta1 WHERE dayssampled_chla>=8")
df_spr <- sqlQuery(channel, "SELECT LakeID, Year, SprTP as Mean, 'SprTP' as 'param' FROM dbo.SprData_AnnualMeans_Sta1 WHERE SprTP is not null")
odbcClose(channel)
head(df_phos)
head(df_secc)
head(df_chla)
head(df_spr)

# kendall tau pvalue
pvalue <-function(x, y) {
  corr=(cor.test(x, y, alternative="two.sided", exact=FALSE, method="kendall"))
  pval <-if(is.na(corr$p.value)) {1} else {corr$p.value}
  return(pval)
}

df <- rbind(df_phos, df_secc, df_chla, df_spr) %>% mutate(LakeID=as.character(LakeID)) %>% mutate(param=as.character(param)) %>% mutate(YearC=Year-1980) %>% nest_by(LakeID, param) %>% mutate(nYears=list(nrow(data))) %>% filter(nYears>=5) %>% mutate(mean=list(round(mean(data$Mean),1))) %>% mutate(sd=list(round(sd(data$Mean),1))) %>% mutate(pvalue=list(round(pvalue(data$Mean, data$YearC),4))) %>% mutate(cv=round(sd/mean,3)) %>% mutate(cv_mean=round(sd/(sqrt(sd)*mean),3)) %>% mutate(lm=list(lm(data=data, Mean~YearC))) %>% mutate(dw=list(dwtest(lm))) %>% mutate(dw.pvalue=as.numeric(dw[4])) 

print(df[c("LakeID", "param", "dw.pvalue")] %>% filter(dw.pvalue<.05) %>% arrange(param, LakeID), n=100)

channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
lmp <- sqlQuery(channel, "SELECT LakeID, LakeStationNo, year(VisitDate) as 'Year', VisitDate, Phos as 'lmpPhos' FROM dbo.LayData_DailyMeans WHERE dayssampled_phos>=8 AND Phos is not null")
odbcClose(channel)

####################################
# lmp data with all dates

lmp_phos <- lmp %>% mutate(LakeID=as.character(LakeID)) %>% nest_by(LakeID, LakeStationNo, Year) %>% group_by(LakeID,LakeStationNo) %>% add_count(LakeID, LakeStationNo) %>% ungroup() %>% filter(n>=5) %>% unnest(data) %>% rename(nYears=n) %>% nest_by(LakeID, LakeStationNo, nYears) %>% mutate(lm=list(lm(data=data, lmpPhos~VisitDate))) %>% mutate(dw=list(dwtest(lm)))  %>% mutate(dw.pvalue=as.numeric(dw[4])) 

print(lmp_phos[c("LakeID", "LakeStationNo", "dw.pvalue")] %>% filter(dw.pvalue<.05) %>% arrange(LakeID), n=100)







