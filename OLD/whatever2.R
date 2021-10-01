#functions
TSslope<-function(x, y) {
  m <- zyp.sen(y~x)
  slope <- m$coefficients[2] #get ts slope
  return(slope)
}

TSintercept<-function(x, y) {
  m <- zyp.sen(y~x)
  intercept <- m$coefficients[1] #get ts intercept
  return(intercept)
}

estimate <-function(x, y) {
  corr=(cor.test(x, y, alternative="two.sided", exact=FALSE, method="kendall"))
  est=if(is.na(corr$estimate)) {1} else {corr$estimate}
  return(est)
}

pvalue <-function(x, y) {
  corr=(cor.test(x, y, alternative="two.sided", exact=FALSE, method="kendall"))
  pval <-if(is.na(corr$p.value)) {1} else {corr$p.value}
  return(pval)
}


myLakeID <- "RAPONDA"
yearStart <- 1980
yearEnd <- 2020

channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
lmp.daily <- sqlQuery(channel, paste0("SELECT LakeID, VisitDate, Year(VisitDate) as 'Year', Phos, DaysSampled_Phos  FROM 
                    dbo.LayData_DailyMeans WHERE Phos is not NULL AND LakeStationNo='1' 
                    AND year(VisitDate) >= ", yearStart, " AND year(VisitDate) <=", yearEnd,
                                      " AND LakeID='", myLakeID, "'"))
odbcClose(channel)
#cat("\n", is.numeric(lmp.daily$Year))

# select needed variables and compute annual means and standard deviation
lmp.df <- lmp.daily %>% select(Year, Phos, DaysSampled_Phos) %>% 
  group_by(Year) %>% mutate(annmean=mean(Phos), annsd=sd(Phos))
#cat("\n", is.numeric(lmp.df$Year))

lmp.df.annmean <- lmp.df %>% group_by(Year) %>% summarize(annmean=first(annmean)) 

# calculate Theil-Sen slope and intercept
#df$YearC <- df$Year-startYear
lmp.df.annmean$Year <- as.numeric(lmp.df.annmean$Year)
lmp.df.annmean$YearC <- lmp.df.annmean$Year - yearStart
slope <- as.numeric(TSslope(lmp.df.annmean$YearC, lmp.df.annmean$annmean))
intercept <- as.numeric(TSintercept(lmp.df.annmean$YearC, lmp.df.annmean$annmean))
#endPoint = slope*(2020-2002) + intercept


ggplot() +
  geom_point(data=lmp.df, aes(x=Year, y=Phos), color='#4b2e39', shape=1, size=3, alpha=0.8) +
  geom_point(data=lmp.df.annmean, aes(x=Year, y=annmean), size=3, fill='#3C8DBC', color='#4b2e39', shape=21, alpha=0.8) + 
  geom_abline(mapping=aes(intercept=intercept, slope=slope), color="red", size=2, linetype="solid") +
  ylab("Phos ug/L") +
  ylim(0, max(lmp.df$Phos)) +
  theme_bw() +
  theme(panel.border=element_blank(), legend.position = "top")

            
if (input$lmpyscale=="Log") {
  p <- p + scale_y_log10()
}

p <- ggplotly(p)