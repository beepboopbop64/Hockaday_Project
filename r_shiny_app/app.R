library(shiny)
library(tidyverse)
library(ggthemes)
library(ggpubr)
library(plotly)
library(DT)
library(shinythemes)


# Load student data
all_cohorts <- read.csv("./data/real_data_V2.csv")
all_cohorts <- as.data.frame(all_cohorts)

# Define UI for application
ui <- 
  fluidPage(
    ## Custom style
    tagList(
      # CSS for navbar elements
      tags$style(
        HTML('.navbar {font-size: 17px;}',
             '.navbar {background-color: #003F23;}',
             '.navbar {height: 60px;}')
      ),
    ), # end of style block
  
  navbarPage(
  
  title = div(HTML('<img src="https://upload.wikimedia.org/wikipedia/en/3/31/Hockaday.png" height="30" alt="Hockaday Logo">'), "Hockaday Project"),
  theme = shinytheme("flatly"),
  
  tabPanel(
    title = "Data Exploration",
    fluidRow(sidebarLayout(
      sidebarPanel(
        actionButton("update", "Combine Data"),
        checkboxGroupInput(
          "cohorts",
          "Which Cohorts to Include:",
          choices = c(unique(all_cohorts$Cohort)),
          selected = unique(all_cohorts$Cohort)
        ),
        radioButtons(
          "skillOfInterest",
          "Select Skill of Interest",
          c(
            "Empathy" = "Empathy_Skills",
            "Advocacy" = "Advocacy_Skills",
            "Purpose" = "Purpose"
          )
          
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
      mainPanel(plotOutput("boxPlot"))
    )),
    
    fluidRow(plotlyOutput("lineGraph")),
    
    fluidRow(
      column(p("Top N Students per Cohort and Grade"), width = 6),
      tableOutput('topN'),
      column(p("Bottom N Students per Cohort and Grade"), width = 6),
      tableOutput('bottomN')
    ),
    
    fluidRow(
      column(
        3,
        radioButtons(
          "skillOfInterestScatterOne",
          "Select Skill of Interest, Reflection or Feedback of Interest",
          c(
            "Empathy" = "Empathy_Skills",
            "Advocacy" = "Advocacy_Skills",
            "Purpose" = "Purpose"
          ),
        ),
        
        radioButtons(
          "skillOfInterestScatterTwo",
          "Select Skill of Interest, Reflection or Feedback of Interest",
          c(
            "Empathy" = "Empathy_Skills",
            "Advocacy" = "Advocacy_Skills",
            "Purpose" = "Purpose"
          ),
        )
      ),
      column(8,
             plotOutput("Scatter"))
    )
  ),
  tabPanel(
    title = "Data Upload",
    fluidPage(
      fluidRow(
        column(
          width = 2,  # Decreased width to 5
          div(
            style = "border: 1px solid #ccc; padding: 10px;",
            h4("Upload Data Files", style = "color: #333;"),
            p("Upload your data files here for analysis.", style = "color: #666;"),
            br(),
            fileInput("upload", label = "Upload Data (CSV or XLSX)", multiple = TRUE, accept = c(".xlsx", "csv")),
            br(),
            fileInput("uploadEmpathyQuotient", "Upload Empathy Quotient Students (CSV or XLSX)", multiple = TRUE, accept = c(".xlsx", "csv")),
            br(),
            fileInput("uploadFellowsSurvey", "Upload Social Impact Fellows Survey (CSV or XLSX)", multiple = TRUE, accept = c(".xlsx", "csv")),
            br(),
            fileInput("uploadExitSurvey", "Upload Social Impact Senior Exit Survey (CSV or XLSX)", multiple = TRUE, accept = c(".xlsx", "csv")),
            br(),
            fileInput("uploadMarkers", "Upload Social Impact Student Markers (CSV or XLSX)", multiple = TRUE, accept = c(".xlsx", "csv"))
          )
        ),
        column(
          width = 9,
          div(
            style = "border: 1px solid #ccc; padding: 10px;",
            h4("Uploaded Data Preview", style = "color: #333;"),
            p("This table displays the first and last 5 records of the data you just uploaded.", style = "color: #666;"),
            # Add a dataTableOutput element to display data
            dataTableOutput("uploadedDataPreview")
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          verbatimTextOutput("uploadStatus")
        )
      )
    )
  ),
  tabPanel(
    title = "Statistics",
    
    fluidRow(
      column(
        3,
        radioButtons(
          "statSkillOfInterest",
          "Select Skill of Interest",
          c(
            "Empathy" = "Empathy_Skills",
            "Advocacy" = "Advocacy_Skills",
            "Purpose" = "Purpose"
          ),
        ),
        
        radioButtons("statCohort",
                     "Select Cohort",
                     c(unique(all_cohorts$Cohort)),),
        
        radioButtons("statTimeOne",
                     "Select First Time Period to Compare",
                     c(sort(
                       unique(all_cohorts$Grade)
                     )),),
        
        radioButtons(
          "statTimeTwo",
          "Select Second Time Period to Compare",
          c(sort(unique(all_cohorts$Grade)), decreasing = TRUE),
        )
        
      )
    ),
    
    column(8,
           plotOutput("stat_box")),
    
    column(8,
           plotOutput("stat_hist")),
    
    column(8,
           plotOutput("stat_qq")),
    
    fluidRow(column(
      width = 6,
      radioButtons(
        "assumptionsMet",
        "Are the assumptions met?",
        c(
          "Yes, use paired t-test" = "t_test",
          "No, use Wilcox Signed Rank Test" = "wilcox"
        )
      )
    )),
    
    fluidRow(column(width = 6,
                    verbatimTextOutput("info")))
    
    
    
  )
)
)



# Define server logic
server <- function(input, output, session) {
  uploadedFiles <- reactiveValues(
    empathyQuotient = NULL,
    fellowsSurvey = NULL,
    exitSurvey = NULL,
    markers = NULL
  )
  
  # Data upload handlers
  observeEvent(input$uploadEmpathyQuotient, {
    file <- input$uploadEmpathyQuotient
    if (!is.null(file)) {
      uploadedFiles$empathyQuotient <- read.csv(file$datapath)
      output$uploadStatus <- renderPrint({
        "Empathy Quotient Students uploaded successfully."
      })
    }
  })
  
  observeEvent(input$uploadFellowsSurvey, {
    file <- input$uploadFellowsSurvey
    if (!is.null(file)) {
      uploadedFiles$fellowsSurvey <- read.csv(file$datapath)
      output$uploadStatus <- renderPrint({
        "Social Impact Fellows Survey uploaded successfully."
      })
    }
  })
  
  observeEvent(input$uploadExitSurvey, {
    file <- input$uploadExitSurvey
    if (!is.null(file)) {
      uploadedFiles$exitSurvey <- read.csv(file$datapath)
      output$uploadStatus <- renderPrint({
        "Social Impact Senior Exit Survey uploaded successfully."
      })
    }
  })
  
  observeEvent(input$uploadMarkers, {
    file <- input$uploadMarkers
    if (!is.null(file)) {
      uploadedFiles$markers <- read.csv(file$datapath)
      output$uploadStatus <- renderPrint({
        "Social Impact Student Markers uploaded successfully."
      })
    }
  })
  
  selected <- reactive(all_cohorts %>%
                         filter(Cohort %in% input$cohorts))
  
  
  stat_data <- reactive({
    grade_before <- paste('Grade', input$statTimeOne, sep = '')
    grade_after <- paste('Grade', input$statTimeTwo, sep = '')
    
    cleaned_data <- all_cohorts %>%
      replace(is.na(.), 0) %>%
      filter(Cohort == input$statCohort) %>%
      filter(Grade %in% (c(
        input$statTimeOne, input$statTimeTwo
      ))) %>%
      dplyr::select(c('Cohort', 'Name', 'Grade', input$statSkillOfInterest)) %>%
      pivot_wider(
        names_from = 'Grade',
        names_prefix = 'Grade',
        values_from = input$statSkillOfInterest
      ) %>%
      mutate(
        skill_of_interest_difference =
          !!as.symbol(grade_after) -!!as.symbol(grade_before)
      ) %>%
      drop_na(skill_of_interest_difference) %>%
      dplyr::select(c('Cohort', 'Name', 'skill_of_interest_difference'))
    
    
    cleaned_data <- as.data.frame(cleaned_data)
    
    
  })
  
  
  data <- reactive({
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    switch(
      ext,
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
    
    updateRadioButtons(session, 'statTimeOne',
                       choices = available_grades)
    
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
    
    updateRadioButtons(session, 'statTimeTwo',
                       choices = available_grades)
    
  })
  
  
  observeEvent(input$update, {
    csv = as.data.frame(data())
    write.csv(csv, "./data/real_data_V1.csv", row.names = FALSE)
    all_cohorts <- read.csv("./data/real_data_V1.csv")
    all_cohorts <<-
      as.data.frame(all_cohorts) # two << makes it available globally
    
    # updated check box with new data
    updateCheckboxGroupInput(
      session,
      "cohorts",
      "Which Cohorts to Include:",
      choices = c(unique(all_cohorts$Cohort)),
      selected = unique(all_cohorts$Cohort)
    )
  })
  
  output$boxPlot <- renderPlot({
    # create the box plots for the selected cohorts
    all_cohorts %>%
      filter(Cohort %in% input$cohorts) %>%
      ggplot(aes(x = factor(Grade), color = factor(Cohort))) +
      geom_boxplot(aes_string(y = input$skillOfInterest)) +
      ggtitle("Boxplot") +
      xlab("Cohort") +
      ylab(input$skillOfInterest) +
      theme_economist() +
      coord_flip()
  })
  
  output$lineGraph <- renderPlotly({
    # Filter data based on input
    filtered_data <- all_cohorts %>%
      filter(Cohort %in% input$cohorts)
    
    # Create ggplot
    p <- ggplot(filtered_data, aes_string(x = "Grade", y = input$skillOfInterest, color = "factor(Cohort)", text = "Name")) +
      geom_jitter() +
      theme_economist()
    
    print(head(filtered_data))
    
    # Convert ggplot to plotly for interactive tooltips
    ggplotly(p, tooltip = c("Name", "Grade", input$skillOfInterest))
  })
  
  output$topN <- renderTable(
    selected() %>%
      group_by(Cohort, Grade) %>%
      slice_max(
        order_by = !!rlang::sym(input$skillOfInterest),
        n = input$nForTables
      )
  )
  
  output$bottomN <- renderTable(
    selected() %>%
      group_by(Cohort, Grade) %>%
      slice_min(
        order_by =  !!rlang::sym(input$skillOfInterest),
        n = input$nForTables
      )
  )
  
  output$Scatter <- renderPlot({
    selected() %>%
      ggplot(aes(color = factor(Cohort))) +
      geom_jitter(
        aes_string(
          x = input$skillOfInterestScatterOne,
          y = input$skillOfInterestScatterTwo
        )
      ) +
      facet_wrap( ~ Grade)
  })
  
  ##############
  # STAT PLOTS #
  ##############
  
  output$stat_box <- renderPlot({
    stat_data() %>%
      ggplot(aes_string(y = 'skill_of_interest_difference')) +
      geom_boxplot() +
      coord_flip() +
      theme_economist()
    
  })
  
  output$stat_hist <- renderPlot({
    stat_data() %>%
      ggplot(aes_string(y = 'skill_of_interest_difference')) +
      geom_histogram() +
      coord_flip() +
      theme_economist()
    
  })
  
  output$stat_qq <- renderPlot({
    stat_data() %>%
      ggplot(aes_string(sample = 'skill_of_interest_difference')) +
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
  
  # Display the first and last 5 rows of uploaded data
  uploadedData <- reactive({
    if (!is.null(input$upload)) {
      data <- read.csv(input$upload$datapath)
      head_and_tail <- rbind(head(data, 5), tail(data, 5))
      return(head_and_tail)
    }
    return(NULL)
  })
  
  output$uploadedDataPreview <- renderDataTable({
    datatable(uploadedData(), options = list(lengthMenu = c(5, 10, 20), pageLength = 5))
  })
  
  
  
}



# Run the application
shinyApp(ui = ui, server = server)
