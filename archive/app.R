library(shiny)
library(tidyverse)
library(ggthemes)
library(ggpubr)

# Load student data
all_cohorts <- read.csv("./data/synthetic_data_V1.csv")
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
              "Purpose" = "Purpose",
              "Number of Social Impact Classes" = "Number_of_Social_Impact_Classes",
              "Empathy Year Growth" = "Empathy_Year_Delta",
              "Advocacy Year Growth" = "Advocacy_Year_Delta",
              "Purpose Year Growth" = "Purpose_Delta",
              "Teacher Feedback" = "Teacher_feedback_on_the_above_areas_of_growth",
              "Students Reflection of Confidence" = "Students_reflection_on_feeling_more_confident"),
          ),
          
          radioButtons(
            "skillOfInterestScatterTwo",
            "Select Skill of Interest, Reflection or Feedback of Interest",
            c("Empathy" = "Empathy_Skills",
              "Advocacy" = "Advocacy_Skills",
              "Purpose" = "Purpose",
              "Number of Social Impact Classes" = "Number_of_Social_Impact_Classes",
              "Empathy Year Growth" = "Empathy_Year_Delta",
              "Advocacy Year Growth" = "Advocacy_Year_Delta",
              "Purpose Year Growth" = "Purpose_Delta",
              "Teacher Feedback" = "Teacher_feedback_on_the_above_areas_of_growth",
              "Students Reflection of Confidence" = "Students_reflection_on_feeling_more_confident"),
          )
        ),
      column(8,
          plotOutput("Scatter")
      
          )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    selected <- reactive(
      all_cohorts %>%
        filter(Cohort %in% input$cohorts)
    )

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
      selected() %>%
        ggplot(aes(x=Grade, color=factor(Cohort))) +
        geom_smooth(aes_string(y=input$skillOfInterest)) +
        stat_cor(aes_string(y=input$skillOfInterest), 
                 p.accuracy = 0.001, r.accuracy = 0.01) +
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
