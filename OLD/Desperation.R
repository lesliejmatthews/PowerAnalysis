library(shiny)
library(shinydashboard)

channel <- odbcDriverConnect('driver={SQL Server};server=anrgdb;database=WQdata;trusted_connection=true')
lakes <- sqlQuery(channel, "SELECT LakeID FROM lake.Lakes WHERE LakeArea >= 10")
odbcClose(channel)
lakeList <- sort(unique(lakes$LakeID))

ui <- dashboardPage(
  dashboardHeader(title = "Vermont Lake Data and Power Analysis", titleWidth = 400),
  dashboardSidebar(width=300,
    sidebarMenu(
      menuItem("Data", tabName = "data"),
      menuItem("Power", tabName = "power")
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
              box(title="scale", width=NULL, solidHeader = T, status="warning",
                   "This will be input for changing y axis scale",
                  htmlOutput("plotAllDataScale") # This will be a table of statistical results
              ),
              box(title="statistics", width=NULL, solidHeader = T, status="primary",
                   "This will be a table of statistical results associated with plot on the left",
                  htmlOutput("statsAllData") # This will be a table of statistical results
              )
           )
        )
      ),
      # Second tab content
      tabItem(tabName = "power",
        fluidRow(
          box(title="Multiple Samples Per summer - Parametric Test", width=6,
                solidHeader=T, status="warning",
            htmlOutput("powerTableParametric"
            )
          ),
          box(title="Multiple Samples Per summer - Non-Parametric Test", width=6,
                    solidHeader=T, status="warning",
            htmlOutput("powerTableNonParametric"
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$powerTableParametric <- renderText({
    
    text <- input$LakeID
    cat(text)
    text
    
  })

}

shinyApp(ui, server)



