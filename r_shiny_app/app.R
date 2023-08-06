library(shiny)
library(tidyverse)
library(ggthemes)
library(ggpubr)

# Load student data
all_cohorts <- read.csv("./data/real_data_V1.csv")
all_cohorts <- as.data.frame(all_cohorts)

# navigation bar


# Define UI for application that draws a histogram
ui <- navbarPage(title = "Hockaday Project",

    # Data Exploration page
    tabPanel(title = "Data Exploration",

    # Row
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
    ),
    tabPanel(title = "Statistics",
             
             fluidRow(
               column(3,
                      radioButtons(
                        "statSkillOfInterest",
                        "Select Skill of Interest",
                        c("Empathy" = "Empathy_Skills",
                          "Advocacy" = "Advocacy_Skills",
                          "Purpose" = "Purpose"),
                      ),
                      
                      radioButtons(
                        "statCohort",
                        "Select Cohort",
                        c(unique(all_cohorts$Cohort)),
                      ),
                      
                      radioButtons(
                        "statTimeOne",
                        "Select First Time Period to Compare",
                        c(sort(unique(all_cohorts$Grade))),
                      ),
                      
                      radioButtons(
                        "statTimeTwo",
                        "Select Second Time Period to Compare",
                        c(sort(unique(all_cohorts$Grade)), decreasing = TRUE),
                      )
                      
               )
             ),
             
             column(8,
                    plotOutput("stat_box")
                    
             ),
             
             column(8,
                    plotOutput("stat_hist")
                    
             ),
             
             column(8,
                    plotOutput("stat_qq")
                    
             ),
             
             fluidRow(
               column(width = 6,
                 radioButtons(
                   "assumptionsMet",
                   "Are the assumptions met?",
                   c("Yes, use t-test" = "t_test",
                     "No, use Wilcox Signed Rank Test" = "wilcox"
                     )
                  )
                )
             ),
             
             fluidRow(
               column(width = 6, 
                      verbatimTextOutput("info")
               )
             ) 
             
             
             
             )
)









# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    selected <- reactive(
      all_cohorts %>%
        filter(Cohort %in% input$cohorts)
    )
    
    
    stat_data <- reactive({
      
      grade_before <- paste('Grade', input$statTimeOne, sep = '')
      grade_after <- paste('Grade', input$statTimeTwo, sep = '')
      
      cleaned_data <- all_cohorts %>% 
        replace(is.na(.), 0) %>%
        filter(Cohort == input$statCohort) %>%
        filter(Grade %in% (c(input$statTimeOne, input$statTimeTwo))) %>%
        dplyr::select(c('Cohort', 'Name', 'Grade', input$statSkillOfInterest)) %>%
        pivot_wider(
         names_from = 'Grade',
         names_prefix = 'Grade',
         values_from = input$statSkillOfInterest
        ) %>%
        mutate(skill_of_interest_difference = 
                 !!as.symbol(grade_after) - !!as.symbol(grade_before)) %>%
        drop_na(skill_of_interest_difference) %>%
        dplyr::select(c('Cohort', 'Name', 'skill_of_interest_difference'))
      
      
      cleaned_data <- as.data.frame(cleaned_data)

    })
    
    
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
    
    observe({
      selected_cohort <- input$statCohort
      
      available_grades <- all_cohorts %>%
        filter(Cohort == selected_cohort)
      
      available_grades <- sort(unique(available_grades$Grade))
      
      updateRadioButtons(
        session, 'statTimeOne',
        choices = available_grades
      )
      
    })
    
    observe({
      selected_cohort <- input$statCohort
      selected_time_one <- as.integer(input$statTimeOne)
      # print('one')
      # print(selected_time_one)
      
      available_grades <- all_cohorts %>%
        filter(Cohort == selected_cohort) %>%
        filter(Grade >= selected_time_one)
      
      available_grades <- unique(available_grades$Grade)
      
      # print('two')
      # print(available_grades)
      
      updateRadioButtons(
        session, 'statTimeTwo',
        choices = available_grades
      )
      
    })
    
    
    observeEvent(input$update,{
      csv = as.data.frame(data())
      write.csv(csv, "./data/real_data_V1.csv", row.names=FALSE)
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
    
    ##############
    # STAT PLOTS #
    ##############
    
    output$stat_box <- renderPlot({
      stat_data() %>% 
        ggplot(aes_string(y='skill_of_interest_difference')) +
        geom_boxplot() +
        coord_flip() +
        theme_economist()
        
    })
    
    output$stat_hist <- renderPlot({
      stat_data() %>% 
        ggplot(aes_string(y='skill_of_interest_difference')) +
        geom_histogram() +
        coord_flip() +
        theme_economist()
      
    })
    
    output$stat_qq <- renderPlot({
      stat_data() %>%
        ggplot(aes_string(sample='skill_of_interest_difference')) +
        stat_qq() +
        stat_qq_line() +
        theme_economist()
      
    })
    
    output$info <- renderPrint({
      
      stat_test_data <- stat_data()
      stat_test_data <- stat_test_data$skill_of_interest_difference
      
      if (input$assumptionsMet == "t_test") {
        
        stat_test_resulsts <- t.test(stat_test_data, 
                                     alternative = "two.sided")
      } else {
        stat_test_resulsts <- wilcox.test(stat_test_data, 
                    alternative = "two.sided")
      }
      
      stat_test_resulsts
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
