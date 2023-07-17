library(shiny)
library(tidyverse)
library(ggthemes)
library(ggpubr)

# Load student data
all_cohorts <- read.csv("./data/real_data_V1.csv")
all_cohorts <- as.data.frame(all_cohorts)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    fluidRow(
      titlePanel("Data Exploration")
      )
    ,

    # AHHHH
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          
          fileInput("upload", NULL, buttonLabel = "Upload...", 
                    multiple = TRUE, accept = c(".xlsx", "csv")),
          actionButton("update", "Combine Data"),
          
          checkboxGroupInput(
            "cohorts", 
            "Which Cohorts to Include:",
            choices = c(unique(all_cohorts$Cohort)),
            selected = unique(all_cohorts$Cohort)),
          
          radioButtons(
            "skillOfInterest",
            "Select Skill of Interest",
            c("Empathy" = "Empathy_Skills",
              "Advocacy" = "Advocacy_Skills",
              "Purpose" = "Purpose")
          ),
          
          sliderInput(
            "nForTables",
            "Top/Bottom N Students",
            min = 1,
            max = 5,
            step = 1,
            value = 1
          )
          
        ),

        # Show a Box Plot
        mainPanel(
           plotOutput("boxPlot")
        )
      )
    ),
    
    fluidRow(
      plotOutput("lineGraph")
    ),
    
    fluidRow(
      column(p("Top N Students per Cohort and Grade" ), width = 6),
      tableOutput('topN'),
      column(p("Bottom N Students per Cohort and Grade" ), width = 6),
      tableOutput('bottomN')
    ),
    
    fluidRow(
      column(3,
          radioButtons(
            "skillOfInterestScatterOne",
            "Select Skill of Interest, Reflection or Feedback of Interest",
            c("Empathy" = "Empathy_Skills",
              "Advocacy" = "Advocacy_Skills",
              "Purpose" = "Purpose"),
          ),
          
          radioButtons(
            "skillOfInterestScatterTwo",
            "Select Skill of Interest, Reflection or Feedback of Interest",
            c("Empathy" = "Empathy_Skills",
              "Advocacy" = "Advocacy_Skills",
              "Purpose" = "Purpose"),
          )
        ),
      column(8,
          plotOutput("Scatter")
      
          )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    selected <- reactive(
      all_cohorts %>%
        filter(Cohort %in% input$cohorts)
    )
    
    
    data <- reactive({
      req(input$upload)
      
      ext <- tools::file_ext(input$upload$name)
      switch(ext,
             # csv = read.csv(input$upload$datapath),
             csv = vroom::vroom(input$upload$datapath, delim = ","),
             # tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
             validate("Invalid file type; Please upload a .csv file")
      )
    })
    
    observeEvent(input$update,{
      csv = as.data.frame(data())
      datam<-rbind.data.frame(all_cohorts, data())
      print('here')
      write.csv(datam, "./data/real_data_V1.csv", row.names=FALSE)
      print('here')
      all_cohorts <- read.csv("./data/real_data_V1.csv")
      all_cohorts <<- as.data.frame(all_cohorts) # two << makes it available globally
      
      # updated check box with new data
      updateCheckboxGroupInput(session, 
                               "cohorts", 
                               "Which Cohorts to Include:",
                               choices = c(unique(all_cohorts$Cohort)),
                               selected = unique(all_cohorts$Cohort))
    })

    output$boxPlot <- renderPlot({
        # create the box plots for the selected cohorts 
      all_cohorts %>%
          filter(Cohort %in% input$cohorts) %>%
          ggplot(aes(x=factor(Grade), color=factor(Cohort))) +
          geom_boxplot(aes_string(y=input$skillOfInterest)) +
          ggtitle("Boxplot") +
          xlab("Cohort") +
          ylab(input$skillOfInterest) +
          theme_economist() +
          coord_flip()
    })
    
    output$lineGraph <- renderPlot({
      all_cohorts %>%
        filter(Cohort %in% input$cohorts) %>%
        ggplot(aes(x=Grade, color=factor(Cohort))) +
        # geom_line(aes_string(y=input$skillOfInterest)) +
        geom_jitter(aes_string(y=input$skillOfInterest)) +
        # stat_cor(aes_string(y=input$skillOfInterest), 
        #          p.accuracy = 0.001, r.accuracy = 0.01) +
        theme_economist()
    })
    
    output$topN <- renderTable(
      selected() %>%
        group_by(Cohort, Grade) %>%
        slice_max(order_by = !! rlang::sym(input$skillOfInterest),
                  n = input$nForTables)
    )
    
    output$bottomN <- renderTable(
      selected() %>%
        group_by(Cohort, Grade) %>%
        slice_min(order_by =  !! rlang::sym(input$skillOfInterest), 
                  n = input$nForTables)
    )
    
    output$Scatter <- renderPlot({
      selected() %>%
        ggplot(aes(color=factor(Cohort))) +
        geom_jitter(aes_string(x=input$skillOfInterestScatterOne,
                               y=input$skillOfInterestScatterTwo)) +
        facet_wrap(~ Grade)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
