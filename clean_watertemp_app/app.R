library(shiny)
library(shinycssloaders)
library(plotly)
library(tidyverse)
library(lubridate)
library(readxl)
library(shinythemes)
library(DT)
library(plotly)
#library(shinyTime)
#to do: 
# csv automatic naming: remove".csv" superfluous
# get update daterange to work with files older than 2018
#catch error of csv read in "skip" argument so it doesn't crash when it's wrong

# textInputRow<-function (inputId, label, value = "") 
# {
#   div(style="display:inline-block",
#       tags$label(label, `for` = inputId), 
#       tags$input(id = inputId, type = "text", value = value,class="input-small"))
# }

options(shiny.maxRequestSize=600*1024^2)
source("functions/2018_present_temp_dates_function.R")
source("functions/pre_2018_clean_temp_function.R")
# Define UI for application that draws a histogram


# UI  ---------------------------------------------------------------------


ui <- fluidPage(

  navbarPage(title = "River Mile Temp Data",
             theme = shinytheme("readable"), #end of navbar page arguments; what follow is all inside it
             
             tabPanel("Temp Data Clean",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput('file1', 'Insert File', accept = c(".csv")),
                          selectInput("select1", label = "Rows to skip when reading in",choices = c(0,1,2,3), selected = 1),
                          selectInput("select2", label = "Rows to take off in file",choices = c(0,1,2,3), selected = 1),
                          checkboxInput("check1", "Pre-2018 Data?"),
                          # sliderInput("slider.5", 
                          #             "Date you want to take off",
                          #             min = parse_date_time("2021-06-01 00:00:00", "ymd_HMS"),
                          #             max = parse_date_time("2021-07-10 00:00:00", "ymd_HMS"), 
                          #             value = c(parse_date_time("2021-06-01 00:00:00", "ymd_HMS"), parse_date_time("2021-07-10 00:00:00", "ymd_HMS"))
                          #             ),#end of slider.5 input
                          dateRangeInput("drange1",
                                         "Date you want to take off",
                                         start = "2021-06-01",
                                         end = "2021-07-10"),
                          splitLayout(textInput("text1", "Lower-bound hour range (excluded)", value = "0"),
                                      textInput("text2", "Upper-bound hour range (included)", value = "0")
                                      ),
                          
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
    
    #Warning: Error in UseMethod: no applicable method for 'filter' applied to an object of class "character"
    #solved because i was using is.null() to check checkbox input when really needed to use TrRUE or FALSE
     if (input$check1 == FALSE && !is.null(input$file1)) {
      read_csv(file = input$file1$datapath, 
               col_names = c("row_number1", "Datetime_GMT_0800", "Temp_Celsius", 
                             "Coupler_Detached", "Coupler_Attached", "Host_Connected","End_of_File"),
               col_types = cols(row_number1 = col_number(),
                                Datetime_GMT_0800 = col_character(),
                                Temp_Celsius = col_number(),
                                Coupler_Attached = col_character(), 
                                Host_Connected = col_character(), 
                                End_of_File = col_character()), 
               skip = as.numeric(input$select1)
               
      )
    }
    
    else if (input$check1 == TRUE && !is.null(input$file1)) {
      read_csv(input$file1$datapath,locale=locale(encoding="latin1"),
               col_names = c("Date", "Time_GMT_0800", "Temp_Celsius"),
               skip = as.numeric(input$select1)
               )
    }
    
    else {
      return("")
    }
    # # actually read the file
    # #can assign colum names here if 
    # read_csv(file = input$file1$datapath, 
    #          col_names = c("row_number1", "Datetime_GMT_0800", "Temp_Celsius", 
    #                        "Coupler_Detached", "Coupler_Attached", "Host_Connected","End_of_File"),
    #          col_types = cols(row_number1 = col_number(),
    #                           Datetime_GMT_0800 = col_character(),
    #                           Temp_Celsius = col_number(),
    #                           Coupler_Attached = col_character(), 
    #                           Host_Connected = col_character(), 
    #                           End_of_File = col_character()), 
    #          skip = as.numeric(input$select1)
    #          
    #          )
             
  })
  

# update UI logic ---------------------------------------------------------

  
  observeEvent(input$file1, {
    updateDateRangeInput(session, "drange1",
                         start = min(clean_dates()$datetime1) -1,
                         end = max(clean_dates()$datetime1) + 1)
    
    updateSliderInput(session, "slider1",
                      min = round(min(clean_dates()$Temp_Celsius) - 1, digits = 0),
                      max = round(max(clean_dates()$Temp_Celsius) + 1, digits = 0),
                      value = c(round(min(clean_dates()$Temp_Celsius)), round(max(clean_dates()$Temp_Celsius))),
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
  input_file2 <- input_file() %>%
    filter(!row_number() %in% c(input$select2))
  
  if (input$check1 == FALSE) {
    x <- clean_dates_post_2018_function(input_file2)
  }
  else {
    x <- clean_dates_pre_2018_function(input_file2)
  }
  
  return(x)
})

# creates reactive values to be modified; they'll be assigned dataframes
# need to set this to null at first; for some reason the reactiveValues() function doesn't work super well with reactives 
# that's why mod_df$x is set/altered below in an observe context
  temp_mod_df <- reactiveValues(cleaned = NULL)
  
  # 
  # #assigns each reactive values a dataframe
  
  observe({
    
    #error:Warning: Error in UseMethod: no applicable method for 'mutate' applied to an object of class "character"
    #was causing ap to crash, fixed by adding req(input_file()) because it was trying to apply the function with mutate in it when there was no data
    req(input_file())
    #initially assigned the original, cleaned data
    temp_mod_df$cleaned <- clean_dates()
  })
  # 
  # #creates proxies to use for datatables
  temp_cleaned_proxy <- DT::dataTableProxy('table2')
  # 
  # #if button is pressed to modify data, then use those filters in sidebar
  observeEvent(input$button1,{

    #makes df of the rows you don't want
    ###IMPORTANT to continuously remove values and prevent values from being added back to the table:
    # need to take off more rows continuously from temp_mod_df$cleaned: before, code read problem_rows <- clean_dates() %>%
    problem_rows <- temp_mod_df$cleaned %>%
      filter(
        Temp_Celsius >= input$slider1[1] & Temp_Celsius <= input$slider1[2],
        datetime1 >= parse_date_time(paste0(input$drange1[1]," ", input$text1, ":00:00"),"ymd_HMS") &
        datetime1 <= parse_date_time(paste0(input$drange1[2]," ", input$text2, ":00:00"),"ymd_HMS")
        #(datetime1 >= input$drange1[1] & datetime1 <= input$drange1[2])
      )
    #replace reactive val dataframe with new rows
    # to CONTINUOUSLY remove values and prevent values from being added back to the table: anti join with temp_mod_df_cleaned
    #before, code read temp_mod_df$cleaned <- anti_join(clean_dates(), problem_rows)
    temp_mod_df$cleaned <- anti_join(temp_mod_df$cleaned, problem_rows)

    #put in the new filtered df with the proxy
    DT::replaceData(temp_cleaned_proxy, temp_mod_df$cleaned)
    
  })
  

# Temp Outputs ---------------------------------------------------------------


    output$plot1 <- renderPlotly({
      
      req(input_file())
        # generate bins based on input$bins from ui.R
      plot <- clean_dates() %>%
        ggplot(aes(x = datetime1, y = Temp_Celsius)) +
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
        ggplot(aes(x = datetime1, y = Temp_Celsius)) +
        geom_line() +
        theme_classic() +
        #make this "from x date to x date
        labs(title = "Temp Data")
      
      ggplotly(plot)
    })
    
    #table 2 output
    output$table2 <- renderDT({
      req(input_file())
      #isolate(temp_mod_df$cleaned)
      datatable(temp_mod_df$cleaned)
    })
    

# Download Handler --------------------------------------------------------

    output$download1 <- downloadHandler(
      filename = 
        function() {
          paste0(as.character(input$file1$name), "_cleaned.csv")
        }
      ,
      content = function(file) {
        write_csv(temp_mod_df$cleaned, file)
        
        
      }
    ) #end of download1    
}

# Run the application 
shinyApp(ui = ui, server = server)
