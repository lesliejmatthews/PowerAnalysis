library(RODBC)
library(tidyverse)
library(pwr)
library(kableExtra)

LakeID <- "WILLOUGHBY"
LakeID <- "CARMI"
LakeID <- "RAPONDA"


channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
lmp_daily <- sqlQuery(channel, paste0("SELECT LakeID, VisitDate, Year(VisitDate) as 'Year', Phos, DaysSampled_Phos  FROM 
                      dbo.LayData_DailyMeans WHERE Phos is not NULL AND LakeStationNo='1' 
                      AND year(VisitDate) >= 2010
                      AND LakeID='", LakeID, "'"))
odbcClose(channel)

# select needed variables and compute annual means and standard deviation
df <- lmp_daily %>% select(Year, Phos, DaysSampled_Phos) %>% 
  mutate(Year=as.factor(Year)) %>% group_by(Year) %>% mutate(annmean=mean(Phos), annsd=sd(Phos))

#######################################################
# estimate r (intraclass correlation)
# one way anova
a <- aov(Phos ~ Year, df)
summary(a)

# between and within mean squares from anova model
BMS <- as.numeric(summary(a)[[1]][[3]][[1]])
WMS <- as.numeric(summary(a)[[1]][[3]][[2]])

# compute average number of replicates to use for m
n.years <- length(unique(df$Year))
m.avg <- df %>% group_by(Year) %>% summarize(m.avg=first(DaysSampled_Phos)) %>% summarize(mean(m.avg)) %>% as.numeric()

# compute r
r <- (BMS-WMS) / (BMS + (m.avg-1)*WMS)

######################################################
# values needed for power analysis
alpha <- 0.05 # alpha for p-value
mu <- 12 # target phosphorus for effect size
alltime.mean <- mean(df$Phos)
alltime.sd <- sd(df$Phos)

# annual means
df.annmean <- df %>% group_by(Year) %>% summarize(annmean=first(annmean)) 

#mean of annual means
annmean.mean <- mean(df.annmean$annmean)

# standard deviation of annual means
annmean.sd <- sd(df.annmean$annmean)

######################################################
# normality and t-test of annual means
normal.p <- df.annmean %>% summarize(shapiro=list(shapiro.test(annmean)$p.value)) %>% 
  unlist() %>% as.numeric()
# willoughby annual means are not normally distributed - use MKpower for simulation wilcox test
wilcox.test(df.annmean$annmean, alternative="greater", mu=mu)

######################################################
# calculate nsub1 which is the one-rep / year equivalent of each combination of year and samples/year
# this use equation 4c reversed in Goulet and Cousineau
# create a dataframe of nsubm values which are the total number of samples i.e. years*reps/year
years <- c(1:10) # nsubm
reps <- c(1:10) # m
#r <- # 0.228 this r for Goulet and Cousineau
d <- abs(alltime.mean - mu) / alltime.sd
#d <- 0.36 # this is the effect size in Goulet and Cousineau

# calculate n1 for each cell using equation 4c reverse
f <- function (m, nsubm) {
  n1 <- (((nsubm - 1) * m) / (1 + ((m - 1) * r))) + 1
  return(n1)
}

m <- outer(reps, years, f)

f.power <- function(n1) {
  pwr <- pwr.t.test(n = n1, d = d, sig.level = alpha, type = "one.sample", alternative = "greater")
  return( round(pwr$power, 2))
}

f.power.nonpara <- function(n1) {
  n.total <- nrow(df)
  n.positive <- nrow(df[df$Phos>=mu,])
  p <- n.positive / n.total
  zalpha <- pnorm(alpha)
  d.nonpara <- abs(2 * (p - 0.5))
  zbeta <- -zalpha + d.nonpara * sqrt(n1)
  power <- 1 - (pnorm(zbeta, lower.tail=F))
}

tab <- as.data.frame(apply(m[,2:10], 1:2, f.power))
colnames(tab) <- c(2:10)
tab$samples <- c(1:10)
tab <- tab[,c(10,2:9)]

tab.nonparametric <- as.data.frame(apply(m[,2:10], 1:2, f.power.nonpara))
colnames(tab.nonparametric) <- c(2:10)
tab.nonparametric$samples <- c(1:10)
tab.nonparametric <- tab.nonparametric[,c(10,2:9)]


tab %>%                 
  kbl(format = "html", align="c") %>%
  column_spec(2, background = ifelse(tab[2] >= .80, "yellow", "white")) %>%
  column_spec(3, background = ifelse(tab[3] >= .80, "yellow", "white")) %>%
  column_spec(4, background = ifelse(tab[4] >= .80, "yellow", "white")) %>%
  column_spec(5, background = ifelse(tab[5] >= .80, "yellow", "white")) %>%
  column_spec(6, background = ifelse(tab[6] >= .80, "yellow", "white")) %>%
  column_spec(7, background = ifelse(tab[7] >= .80, "yellow", "white")) %>%
  column_spec(8, background = ifelse(tab[8] >= .80, "yellow", "white")) %>%
  column_spec(9, background = ifelse(tab[9] >= .80, "yellow", "white")) %>%
  column_spec(10, background = ifelse(tab[10] >= .80, "yellow", "white")) %>%
  row_spec(1:nrow(tab), extra_css = "border: 1px solid darkblue; padding-top: 5px;", color="darkgreen") %>%
  column_spec(1, width="0m", extra_css = "font-weight: bold;", color="black") %>%
  column_spec(2:10, width="0cm", color="black") %>%
  add_header_above(c(" " = 1, "Years" = ncol(tab) - 1)) %>%
  kable_styling(font_size=12,)

