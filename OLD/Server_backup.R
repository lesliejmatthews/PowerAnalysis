server <- function(input, output, session) {
  
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
  
  listData <- reactive({
    
    myLakeID <- input$LakeID
    yearStart <-input$yearRange[1]
    yearEnd <- input$yearRange[2]
    
    # Grab data 
    channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
    lmp.daily <- sqlQuery(channel, paste0("SELECT LakeID, VisitDate, Year(VisitDate) as 'Year', Phos, DaysSampled_Phos  FROM 
                    dbo.LayData_DailyMeans WHERE Phos is not NULL AND LakeStationNo='1' 
                    AND year(VisitDate) >= ", yearStart, " AND year(VisitDate) <=", yearEnd,
                                          " AND LakeID='", myLakeID, "'"))
    odbcClose(channel)
    
    # select needed variables and compute annual means and standard deviation
    df.allData <- lmp.daily %>% select(Year, Phos, DaysSampled_Phos) %>% 
      mutate(Year=as.factor(Year)) %>% group_by(Year) %>% mutate(annmean=mean(Phos), annsd=sd(Phos))
    
    
    #######################################################
    # calcualte intraclass correlation (r)
    #one way anova
    a <- aov(Phos ~ Year, df.allData)
    
    # between and within mean squares from anova model
    BMS <- as.numeric(summary(a)[[1]][[3]][[1]])
    WMS <- as.numeric(summary(a)[[1]][[3]][[2]])
    
    # compute average number of replicates to use for m
    n.years <- length(unique(df.allData$Year))
    m.avg <- df.allData %>% group_by(Year) %>% summarize(m.avg=first(DaysSampled_Phos)) %>% summarize(mean(m.avg)) %>% as.numeric()
    
    # compute r
    r <- (BMS-WMS) / (BMS + (m.avg-1)*WMS)
    
    
    
    
    list(df.allData = df.allData, r = r)
    
  })
  
  listPower <- reactive({
    
    df <- listData()$df.allData
    r <- listData()$r
    
    # values needed for power analysis
    alpha <- 0.05 # alpha for p-value
    mu <- input$mu # target phosphorus for effect size
    alltime.mean <- mean(df$Phos)
    alltime.sd <- sd(df$Phos)
    
    ######################################################
    # calculate nsub1 which is the one-rep / year equivalent of each combination of year and samples/year
    # this use equation 4c reversed in Goulet and Cousineau
    # create a dataframe of nsubm values which are the total number of samples i.e. years*samples/year
    years <- c(1:10) # nsubm
    samples <- c(1:10) # m
    #r <- # 0.228 this r for Goulet and Cousineau
    d.cohen <- abs(alltime.mean - mu) / alltime.sd
    #d <- 0.36 # this is the effect size in Goulet and Cousineau
    
    # calculate n1 for each cell using equation 4c reverse
    f <- function (m, nsubm) {
      n1 <- (((nsubm - 1) * m) / (1 + ((m - 1) * r))) + 1
      return(n1)
    }
    
    matrix.n1 <- outer(samples, years, f)
    
    # parametric power text on matrix.n1
    f.power.parametric <- function(n1) {
      pwr <- pwr.t.test(n = n1, d = d.cohen, sig.level = alpha, type = "one.sample", alternative = "greater")
      return( round(pwr$power, 2))
    }
    
    # non parametric power test on matrix.n1
    f.power.nonparametric <- function(n1) {
      p <- nrow(df[df$Phos>=mu,]) / nrow(df)
      d.nonparametric <- 2 * (p - 0.5)
      zalpha <- qnorm(alpha)
      zbeta <- (-1 * zalpha) + d.nonparametric * sqrt(n1)
      power <- 1 - pnorm(zbeta)
      return( round(power, 2))
    }
    
    # this is power for the parametric t test 
    tab.parametric <- as.data.frame(apply(matrix.n1[,2:10], 1:2, f.power.parametric))
    
    # this is power for the non parametric sign test based on Noether and the USGS book
    tab.nonparametric <- as.data.frame(apply(matrix.n1[,2:10], 1:2, f.power.nonparametric))
    
    # data for isotherm plot - parametric test
    # power = 0.8
    df.iso.parametric <- data.frame(years=numeric(), samples=numeric(), power=numeric())
    for (i in c(2:10)) { # i is the number of reps
      for(z in seq(.5,.9, .1)) {
        n1 <- pwr.t.test(power=z, d = d.cohen, sig.level = alpha, type = "one.sample", alternative = "greater")$n
        yrs <- r * n1 + (1 - r) * ((n1 - 1) / i + 1)
        df.iso.parametric[nrow(df.iso.parametric)+1,] <- c(ceiling(yrs), i, z)
      }
    }
    
    # test normality of annual means
    # annual means
    df.annmean <- df %>% group_by(Year) %>% summarize(annmean=first(annmean)) 
    
    normal.p <- df.annmean %>% summarize(shapiro=list(shapiro.test(annmean)$p.value)) %>% 
      unlist() %>% as.numeric()
    
    if (normal.p < .05) {normal <- FALSE} else {normal <- TRUE}
    
    list(tab.parametric = tab.parametric, tab.nonparametric = tab.nonparametric,
         normal.p=round(normal.p,4), normal=normal, df.iso.parametric=df.iso.parametric)  
    #, tab.nonparametric = tab.nonparametric)
    
  })
  
  # box color based on normality
  output$normality <- renderUI({
    
    if (listPower()$normal == F) {
      
      tagList(
        box(title="Normality", width=NULL, solidHeader=T, status="danger", height=85,
            p(paste0("Annual means are not normally distributed, Shapiro-Wilkes p-value = ", listPower()$normal.p))
        )
      )
    } else {
      tagList(
        box(title="Normality", width=NULL, solidHeader=T, status="primary", height=85,
            p(paste0("Annual means are normally distributed, Shapiro-Wilkes p-value = ", listPower()$normal.p))
        )
      )
    }
    
  })
  
  # daily data by year
  output$dailyByYear <- renderPlotly({
    
    df <- listData()$df.allData
    
    p <- plot_ly(data = df, x = ~Year, y = ~Phos, type= "scatter", mode = "markers", marker = list(size = 10,
                                                                                                   color = '#FFFFFF', line = list(color = 'rgba(152, 0, 0, .8)',  width = 2))) 
    p <- p %>% layout(yaxis=list(title="Phos ug/L"))
    
    if (input$lmpyscale=="Log") {
      p <- p %>% layout(yaxis=list(type="log"))
    } 
    
    p
    
  })
  
  # plot power  parametric
  output$powerParametricPlot <- renderPlotly({
    
    df <- listPower()$df.iso
    
    df %>% group_by(power) %>%
      plot_ly(x=~years, y=~samples, type="scatter", color=~as.factor(power), mode="lines+markers") %>%
      layout(legend=list(title=list(text='<b> Power </b>')), yaxis = list(range=c(11,1)))
    
  })
  
  # table power  parametric
  output$powerTableParametric <- renderDT({
    
    df <- listPower()$tab.parametric
    
    datatable(df, container = sketch, class="compact", rownames=T, 
              colnames=F, options = list(dom = "t", ordering=F, scrollX=T,
                                         columnDefs = list(list(className = 'dt-center', targets="_all")))) %>% 
      formatStyle(c(2:10), backgroundColor=styleInterval(c(0.8), c("white", "yellow")))
    
    
  })
  
  # plot power  nonparametric
  output$powerNonParametricPlot <- renderPlotly({
    
    df <- listPower()$df.iso.nonparametric
    
    df %>% group_by(power) %>%
      plot_ly(x=~years, y=~samples, type="scatter", color=~as.factor(power), mode="lines+markers") %>%
      layout(legend=list(title=list(text='<b> Power </b>')), yaxis = list(range=c(11,1)))
    
  })
  
  #  table power non parametric
  output$powerTableNonParametric <- DT::renderDT({
    
    df <- listPower()$tab.nonparametric
    
    datatable(df, container = sketch, class="compact", rownames=T, 
              colnames=F, options = list(dom = "t", ordering=F, scrollX=T,
                                         columnDefs = list(list(className = 'dt-center', targets="_all")))) %>% 
      formatStyle(c(2:10), backgroundColor=styleInterval(c(0.8), c("white", "yellow")))
    
  })
  
}
