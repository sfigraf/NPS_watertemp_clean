library(shiny)
library(shinycssloaders)
library(plotly)
library(tidyverse)
library(lubridate)
library(readxl)
library(shinythemes)
library(DT)
library(plotly)
#to do: update date range and slider based on file input
#proxies to remove data
#download funcitonality of new data
#slider decimal stuff


options(shiny.maxRequestSize=600*1024^2)
source("functions/clean_temp_dates_function.R")
# Define UI for application that draws a histogram
ui <- fluidPage(

  navbarPage(title = "River Mile Temp Data",
             theme = shinytheme("united"), #end of navbar page arguments; what follow is all inside it
             
             tabPanel("Temp Data Clean",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput('file1', 'Insert File', accept = c(".csv")),
                          dateRangeInput("drange1",
                                         "Date you want to take off",
                                         start = "2021-06-01",
                                         end = "2021-07-10"),
                          sliderInput("slider1", "Temp range to exclude between selected date range above",
                                      min = 0,
                                      max = 50,  
                                      value = c(0,30),
                                      step = 1),

                        actionButton("button1",
                                     "Render/Update Data")
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotlyOutput("plot1")
                        )
                      ) #end of sidebar layout
                    ) #end of clean temp data tab panel
  )#end of navbar page
) #end of fluidpage

# Define server logic required to draw a histogram
server <- function(input, output, session) {

# Upload logic ------------------------------------------------------------
  
  input_file <- reactive({
    if (is.null(input$file1)) {
      return("")
    }
    
    # actually read the file
    read_csv(file = input$file1$datapath, 
             col_types = cols(`Coupler Attached (LGR S/N: 20338010)` = col_character(), 
                              `Host Connected (LGR S/N: 20338010)` = col_character(), 
                              `End Of File (LGR S/N: 20338010)` = col_character()), 
             skip = 1)
  })
  
  # raw_data <- reactive({
  #   if (!is.null(input$file1) && 
  #       (input$file1sheet %in% sheets_name())) {
  #     data <- read_excel(input$file1$datapath, 
  #                        sheet = input$file1sheet,
  #                        col_types = c("date", 
  #                                      "text", "text", "text", "numeric", 
  #                                      "numeric", "numeric", "numeric", 
  #                                      "numeric", "numeric", "numeric", 
  #                                      "numeric", "numeric", "numeric", 
  #                                      "text", "text", "text", "text", "text", 
  #                                      "text", "text", "text", "text", "text", 
  #                                      "text", "text", "text", "text", "text", 
  #                                      "text", "text", "text", "text", "text"), 
  #                        
  #                        skip = 1)
  #     
  #     return(data)
  #   } else {
  #     return(NULL)
  #   }
  # })

# update UI logic ---------------------------------------------------------

  
  observeEvent(input$file1, {
    updateDateRangeInput(session, "drange1",
                         start = min(clean_dates()$datetime1) -1,
                         end = max(clean_dates()$datetime1) + 1)
    
    updateSliderInput(session, "slider1",
                      min = min(clean_dates()$`Temp, °C (LGR S/N: 20338010, SEN S/N: 20338010)`) - 1,
                      max = max(clean_dates()$`Temp, °C (LGR S/N: 20338010, SEN S/N: 20338010)`) + 1,
                      value = c(min(clean_dates()$`Temp, °C (LGR S/N: 20338010, SEN S/N: 20338010)`),max(clean_dates()$`Temp, °C (LGR S/N: 20338010, SEN S/N: 20338010)`)),
                      step = 1
                      #round = TRUE

                      )
  }
    # come back to this

  )

# Temp_wrangle ------------------------------------------------------------
clean_dates <- reactive({
  
  clean_dates1 <- clean_dates_function(input_file())
})

# Temp Plot ---------------------------------------------------------------


    output$plot1 <- renderPlotly({
      
      req(input_file())
        # generate bins based on input$bins from ui.R
      plot <- clean_dates() %>%
        ggplot(aes(x = datetime1, y = `Temp, °C (LGR S/N: 20338010, SEN S/N: 20338010)`)) +
        geom_line() +
        theme_classic() +
        #make this "from x date to x date
        labs(title = "Temp Data")
      
      ggplotly(plot)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
