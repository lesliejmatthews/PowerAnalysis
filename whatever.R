n <- 25
p <- .22
d <- 2 * (p-.5)
zalpha <- qnorm(.05)
zbeta <- -zalpha + d * sqrt(n)
1-pnorm(zbeta)


power = .5
zalpha <- qnorm(.05)
zbeta <- qnorm(1-power)

(zalpha + zbeta)^2 / (4 * (p - .5)^2)



######################################
# Grab data 
myLakeID <- "RAPONDA"
channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
lmp.daily <- sqlQuery(channel, paste0("SELECT LakeID, VisitDate, Year(VisitDate) as 'Year', Phos, DaysSampled_Phos  FROM 
                    dbo.LayData_DailyMeans WHERE Phos is not NULL AND LakeStationNo='1' 
                    AND year(VisitDate) >= ", 1980, " AND year(VisitDate) <=", 2020, " AND LakeID='", myLakeID, "'"))
odbcClose(channel)

df <- lmp.daily %>% select(Year, Phos, DaysSampled_Phos) %>% 
  mutate(Year=as.factor(Year)) %>% group_by(Year) %>% mutate(annmean=mean(Phos), annsd=sd(Phos))

df.annmean <- df %>% group_by(Year) %>% summarize(annmean=first(annmean)) 



normal.p <- df.annmean %>% summarize(shapiro=list(shapiro.test(annmean)$p.value)) %>% 
  unlist() %>% as.numeric()

if (normal.p < .05) {normal <- FALSE} else {normal <- TRUE}





years <- c(1:10) # nsubm
samples <- c(1:10) # m
r <- .2
alpha <- 0.05
mu <- 12
df <- lmp.daily

# calculate n1 for each cell using equation 4c reverse
f <- function (m, nsubm) {
  n1 <- (((nsubm - 1) * m) / (1 + ((m - 1) * r))) + 1
  return(n1)
}

matrix.n1 <- outer(samples, years, f)

# non parametric power test on matrix.n1
f.power.nonparametric <- function(n1) {
  p <- nrow(df[df$Phos>=mu,]) / nrow(df)
  d.nonparametric <- 2 * (p - 0.5)
  zalpha <- qnorm(alpha)
  zbeta <- (-1 * zalpha) + d.nonparametric * sqrt(n1)
  power <- 1 - pnorm(zbeta)
  return( round(power, 2))
}

f.power.nonparametric(33.14)

# this is power for the non parametric sign test based on Noether and the USGS book
tab.nonparametric <- as.data.frame(apply(matrix.n1[,2:10], 1:2, f.power.nonparametric))


#tab.nonparametric[1,] <- as.character(c(2:10))
# tab.nonparametric[,-1] <- rownames(tab.nonparametric)
#tab.nonparametric <- colnames(tab.nonparametric)
# # tab.nonparametric$samples <- c(1:10)
# # tab.nonparametric <- tab.nonparametric[,c(10,1:9)]
# # colnames(tab.nonparametric) <- c("samples", c(2:10))



sketch <- htmltools::withTags(table(
  class = 'display',
  thead(
      tr(
        th(colspan=1, ''),
        th(colspan=10, 'Years')
      ),
      th(colspan=1, "Samples"),     
      th(colspan=1, "2"),
      th(colspan=1, "3"),
      th(colspan=1, "4"),
      th(colspan=1, "5"),
      th(colspan=1, "6"),
      th(colspan=1, "7"),
      th(colspan=1, "8"),
      th(colspan=1, "9"),
      th(colspan=1, "10"),
    )
  )
)
print(sketch)



datatable(tab.nonparametric, container = sketch, class="compact", rownames=T, colnames=F, options = list(dom = "t", ordering=F, scrollX=T,
                                                                               columnDefs = list(list(className = 'dt-center', targets="_all")))) %>% 
  formatStyle(c(2:10), backgroundColor=styleInterval(c(0.8), c("white", "yellow")))


####################

