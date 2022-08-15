library(shiny)
library(shinycssloaders)
library(plotly)
library(tidyverse)
library(lubridate)
library(readxl)
library(shinythemes)
library(DT)
library(plotly)
#to do: 
#proxies to remove data
#download funcitonality of new data



options(shiny.maxRequestSize=600*1024^2)
source("functions/clean_temp_dates_function.R")
# Define UI for application that draws a histogram


# UI  ---------------------------------------------------------------------


ui <- fluidPage(

  navbarPage(title = "River Mile Temp Data",
             theme = shinytheme("readable"), #end of navbar page arguments; what follow is all inside it
             
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
                          tabsetPanel(
                            tabPanel("Original Data",
                                     plotlyOutput("plot1"),
                                     DT::dataTableOutput("table1")
                                     ),
                            tabPanel("Cleaned Data",
                                     plotlyOutput("plot2"),
                                     DT::dataTableOutput("table2"),
                                     downloadButton(outputId = "download1", label = "Save this data as CSV"),
                                     
                            ),
                          ) #end of tabset panel
                         
                        ) #end of mainpanel
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
  

# update UI logic ---------------------------------------------------------

  
  observeEvent(input$file1, {
    updateDateRangeInput(session, "drange1",
                         start = min(clean_dates()$datetime1) -1,
                         end = max(clean_dates()$datetime1) + 1)
    
    updateSliderInput(session, "slider1",
                      min = round(min(clean_dates()$`Temp, °C (LGR S/N: 20338010, SEN S/N: 20338010)`) - 1, digits = 0),
                      max = round(max(clean_dates()$`Temp, °C (LGR S/N: 20338010, SEN S/N: 20338010)`) + 1, digits = 0),
                      value = c(round(min(clean_dates()$`Temp, °C (LGR S/N: 20338010, SEN S/N: 20338010)`)), round(max(clean_dates()$`Temp, °C (LGR S/N: 20338010, SEN S/N: 20338010)`))),
                      step = 1
                      )
  }
    # come back to this

  )

# Temp_wrangle ------------------------------------------------------------
clean_dates <- reactive({
  validate(
    need(!is.null(input_file() ), "Please upload a data set")
  )
  clean_dates1 <- clean_dates_function(input_file())
})

# creates reactive values to be modified; they'll be assigned dataframes

  temp_mod_df <- reactiveValues(cleaned = NULL)
  
  # 
  # #assigns each reactive values a dataframe
  # observe({
  #   #initially assigned the original, cleaned data
  # 
  #   temp_mod_df$cleaned <- clean_dates()
  # })
  # 
  # #creates proxies to use for datatables
  temp_cleaned_proxy <- DT::dataTableProxy('table2')
  # 
  # #if button is pressed to modify data, then use those filters in sidebar
  observeEvent(input$button1,{

    #makes df of the rows you don't want
    problem_rows <- clean_dates() %>%
      filter(
        `Temp, °C (LGR S/N: 20338010, SEN S/N: 20338010)` >= input$slider1[1] & `Temp, °C (LGR S/N: 20338010, SEN S/N: 20338010)` <= input$slider1[2],
        (datetime1 >= input$drange1[1] & datetime1 <= input$drange1[2])
      )
    #replace reactive val dataframe with new rows
    temp_mod_df$cleaned <- anti_join(clean_dates(), problem_rows)

    #put in the new filtered df with the proxy
    DT::replaceData(temp_cleaned_proxy, temp_mod_df$cleaned)
    
  })
  
  # observe({
  #   
  # })

# Temp Outputs ---------------------------------------------------------------


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
    
    output$table1 <- renderDT({
      req(input_file())
      datatable(clean_dates(), editable = TRUE)
    })
    #cleaned data plot
    output$plot2 <- renderPlotly({
      
      req(input_file())
      # generate bins based on input$bins from ui.R
      plot <- temp_mod_df$cleaned %>%
        ggplot(aes(x = datetime1, y = `Temp, °C (LGR S/N: 20338010, SEN S/N: 20338010)`)) +
        geom_line() +
        theme_classic() +
        #make this "from x date to x date
        labs(title = "Temp Data")
      
      ggplotly(plot)
    })
    
    #table 2 output
    output$table2 <- renderDT({
      #req(input_file())
      isolate(temp_mod_df$cleaned)
      datatable(temp_mod_df$cleaned)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
