library(tidyverse)
library(shiny)
library(shinydashboard)
library(RODBC)
library(plotly)

channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
lakes <- sqlQuery(channel, "SELECT LakeID FROM lake.Lakes WHERE LakeArea >= 10")
odbcClose(channel)
lakeList <- sort(unique(lakes$LakeID))
#selectInput("LakeID", label = "LakeID", choices=lakeList, selected="RAPONDA"

ui <- dashboardPage(
  dashboardHeader(title = "Vermont Lake Data and Power Analysis", titleWidth = 400),
  dashboardSidebar(width=300,
    sidebarMenu(
      menuItem("Data", tabName = "data"),
      menuItem("Power", tabName = "power")
      #sidebarMenuOutput("sidebarSelection")
    ),
    selectInput("LakeID", label = "Lake ID", choices=lakeList, selected="RAPONDA"
    ),
    sliderInput("yearRange", label = "Year Range", min = 1980, 
                  max = 2020, sep="", value = c(1980, 2020)
    )
  ),

  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "data",
        fluidRow(
          box(title="Lay Monitoring Total Phosphorus - All Data", width=8, solidHeader = T, status="primary",
            plotlyOutput("trend") # make this box plots?
          ),
          column(width=4,
            box(title="Statistics", width=NULL, solidHeader = T, status="warning",
                br(), "This will be a table of statistical results associated with plot on the left",
                htmlOutput("statsAllData") # This will be a table of statistical results
            ),
            box(title="Statistics", width=NULL, solidHeader = T, status="primary",
                br(), "This will be a table of statistical results associated with plot on the left",
              htmlOutput("statsAllData") # This will be a table of statistical results
            ),
          )
        )
      ),
      # Second tab content
      tabItem(tabName = "power",
        fluidRow(
          box(title="Multiple Samples Per summer - Parametric Test", width=6, 
                  solidHeader=T, status="warning", 
              htmlOutput("powerTableParametric"
              ),
          ),
          box(title="Multiple Samples Per summer - Non-Parametric Test", width=6, 
                solidHeader=T, status="warning",
              htmlOutput("powerTableNonParametric"),
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  
  output$powerTableParametric <- renderText({
    
    html("HELP")
    cat("help")
    cat("what the fuck")
  })
}

shinyApp(ui, server)
  
  # listDataAll <- reactive({
  #   
  #   myLakeID <- input$LakeID
  #   yearStart <-input$yearRange[1]
  #   yearEnd <- input$yearRange[2]
    
    # Grab data 
    #channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
    #lmp.daily <- sqlQuery(channel, paste0("SELECT LakeID, VisitDate, Year(VisitDate) as 'Year', Phos, DaysSampled_Phos  FROM 
    #                  dbo.LayData_DailyMeans WHERE Phos is not NULL AND LakeStationNo='1' 
    #                  AND year(VisitDate) >= ", yearStart, " AND year(VisitDate) <=", yearEnd,
    #                  " AND LakeID='", myLakeID, "'"))
    #odbcClose(channel)
    
    # select needed variables and compute annual means and standard deviation
    # df.allData <- lmp.daily %>% select(Year, Phos, DaysSampled_Phos) %>% 
    #   mutate(Year=as.factor(Year)) %>% group_by(Year) %>% mutate(annmean=mean(Phos), annsd=sd(Phos))
    # cat(df.allData[1,1], "/n")
    
    #######################################################
    # calcualte intraclass correlation (r)
    # one way anova
    # a <- aov(Phos ~ Year, df.allData)
    # cat(summary(a)[1,1], "/n")
    # 
    # between and within mean squares from anova model
    # BMS <- as.numeric(summary(a)[[1]][[3]][[1]])
    # WMS <- as.numeric(summary(a)[[1]][[3]][[2]])
    
    # compute average number of replicates to use for m
    # n.years <- length(unique(df.allData$Year))
    # m.avg <- df.allData %>% group_by(Year) %>% summarize(m.avg=first(DaysSampled_Phos)) %>% summarize(mean(m.avg)) %>% as.numeric()
    # cat(n.years, "/n")
    
    # compute r
    # r <- (BMS-WMS) / (BMS + (m.avg-1)*WMS)
    
    # list(df.allData = df.allData, r = r)
    
  #})

  #powerAnalysis <- reactive({
  #   
  #   df <- listDataAll()$df_allData
  #   r <- listDataAll()$r
  #   cat("/n", r)
  #   
  #   # values needed for power analysis
  #   alpha <- 0.05 # alpha for p-value
  #   mu <- 12 # target phosphorus for effect size
  #   alltime.mean <- mean(df$Phos)
  #   alltime.sd <- sd(df$Phos)
  #   
  #   ######################################################
  #   # calculate nsub1 which is the one-rep / year equivalent of each combination of year and samples/year
  #   # this use equation 4c reversed in Goulet and Cousineau
  #   # create a dataframe of nsubm values which are the total number of samples i.e. years*reps/year
  #   years <- c(1:10) # nsubm
  #   reps <- c(1:10) # m
  #   #r <- # 0.228 this r for Goulet and Cousineau
  #   d <- abs(alltime.mean - mu) / alltime.sd
  #   #d <- 0.36 # this is the effect size in Goulet and Cousineau
  #   
  #   # calculate n1 for each cell using equation 4c reverse
  #   f <- function (m, nsubm) {
  #     n1 <- (((nsubm - 1) * m) / (1 + ((m - 1) * r))) + 1
  #     return(n1)
  #   }
  #   
  #   # 
  #   matrix.n1 <- outer(reps, years, f)
  #   
  #   # parametric power text on matrix.n1
  #   f.power.parametric <- function(n1) {
  #     pwr <- pwr.t.test(n = n1, d = d, sig.level = alpha, type = "one.sample", alternative = "greater")
  #     return( round(pwr$power, 2))
  #   }
  #   
  #   # non parametric power text on matrix.n1
  #   # base this on the Noether paper
  #   # f.power.parametric <- function(n1) {
  #   #   pwr <- pwr.t.test(n = n1, d = d, sig.level = alpha, type = "one.sample", alternative = "greater")
  #   #   return( round(pwr$power, 2))
  #   # }
  #   
  #   tab.parametric <- as.data.frame(apply(matrix.n1 , 1:2, f.power))
  #   colnames(tab) <- c("Reps", c(2:10))
  #   tab[,1] <- c(1:10)
  #   
  #   # this will be for the non parametric test based on Noether and the USGS book
  #   tab.nonparametric <- as.data.frame(apply(matrix.n1 , 1:2, f.power))
  #   colnames(tab) <- c("Reps", c(2:10))
  #   tab[,1] <- c(1:10)
    
  #})
  
  # output$trend <- renderPlotly({
  # 
  #   df <- listDataAll()$df.allData
  #   cat(as.character(df$Year))
  #   
  #   p <- plot_ly(data = df, x = ~Year, y = ~Phos, type= "scatter", mode = "markers", marker = list(size = 10,
  #             color = '#FFFFFF', line = list(color = 'rgba(152, 0, 0, .8)',  width = 2)))
  #   p
  #   
  # })


  



