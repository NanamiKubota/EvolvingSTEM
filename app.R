#
# This is a Shiny web application for the EvolvingSTEM project.
#
# Author: Nanami Kubota

library(shiny)
library(shinyvalidate) #validate responses
# library(shinythemes) #color schemes
library(ggplot2) #plot
library(googlesheets4) #save input to googlesheet
# library(gargle)
# library(googledrive)
library(dplyr)
library(bslib)
library(DT)

#save responses to this googlesheet
# run this once in RStudio locally:
# gs4_auth(cache = ".secrets")
# then run this afterwards
options(gargle_oauth_client_type = "installed")
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)

# google sheet link
ss <- "https://docs.google.com/spreadsheets/d/1QAq9l5iNXGPDR_uOoB7MGq4b1bvMLYTnFV40nMZSiOw/edit?usp=sharing"

# lists
choices_df <- read.csv("survey_choices.csv",stringsAsFactors = FALSE, na.strings = "")

hours <- c("72", "96", "120")

past_data_df <- read.csv("combined_data_cleanR.csv")
past_data_df$submission <- "other people"
past_data_df$hour <- as.numeric(past_data_df$hour)
past_data_df$timestamp <- NA

#user interface
ui <- page_navbar(
  title = "EvolvingSTEM",
  theme = bs_theme(preset = "flatly"),
  footer = "test",
  nav_panel(
    title = "Graph",
    page_sidebar(
      sidebar = sidebar(
        title = "Input your data!",
        width = 300,
        selectizeInput("school", label = "School:", 
                       choices = na.omit(choices_df$school),
                       options = list(placeholder = 'Please select an option below', onInitialize = I('function() { this.setValue(""); }'))),
        selectizeInput("teacher", label = "Teacher:", 
                       choices = na.omit(choices_df$teacher), 
                       options = list(placeholder = 'Please select an option below', onInitialize = I('function() { this.setValue(""); }'))),
        selectizeInput("class", label = "Class period:", 
                       choices = as.character(seq(1, 12)),
                       options = list(placeholder = 'Please select an option below', onInitialize = I('function() { this.setValue(""); }'))),
        textInput("name", "Your name:"),
        dateInput("date", "Date of measurement:",format = "yyyy-mm-dd"),
        selectizeInput("strain", "Strain:", 
                       choices = na.omit(choices_df$strain),
                       options = list(placeholder = 'Please select an option below', onInitialize = I('function() { this.setValue(""); }'))),
        selectizeInput("hour", "Hours since starting experiment:", hours, options = list(placeholder = 'Please select an option below', onInitialize = I('function() { this.setValue(""); }'))),
        numericInput("replicate", "Biological replicate number", min = 1, max = NA, value = NULL, step = 1),
        numericInput("area", "Dicty area cleared (cm²):", min = 1, max = 56.75, value = NULL, step = 0.01),
        
        actionButton("submitbutton", "Submit", class = "btn btn-danger ", width = '100%')
      ),
      verbatimTextOutput('contents'),
      navset_card_underline(
        title = "Dicty clearing",
        nav_panel("Timecourse", plotOutput(outputId = "dicty_clearing_plot")),
        nav_panel("Day 3 vs Day 5", plotOutput(outputId = "clearing_day3day5_plot")),
        nav_panel("Difference", plotOutput(outputId = "clearing_diff_plot"))
      )
    ),
  ),
  nav_panel(
    title = "Table",
    navset_card_underline(
      title = "Dataset",
      nav_panel("All", DT::dataTableOutput("data_table_all")),
      nav_panel("Day 3 and Day 5 Difference", DT::dataTableOutput("data_table_diff"))
    )
  ),
  nav_panel(
    title = "Explore",
    page_sidebar(
      sidebar = sidebar(
        title = "Filter data by school",
        width = 300,
        selectInput("school_filter", label = "School:", 
                       choices = c("All schools", "Cooper Lab", na.omit(choices_df$school)), selected = "All schools")),
    navset_card_underline(
      title = "Explore all aggregated data",
      nav_panel("Timecourse", plotOutput(outputId = "combined_timecourse_plot")),
      nav_panel("Day 3 vs Day 5", plotOutput(outputId = "combined_day3day5_plot")),
      nav_panel("Difference", plotOutput(outputId = "combined_diff_plot")),
      nav_panel("Table", DT::dataTableOutput("combined_data_table"))
    ))
  ),
  nav_spacer(),
  nav_item(tags$div(
    tags$span("Made by Nanami Kubota using R Shiny")
  ))
)

server <- function(input, output) {
  # bs_themer() #play around with themes
  #Input validator
  iv <- InputValidator$new()
  iv$add_rule("school", sv_required())
  iv$add_rule("teacher", sv_required())
  iv$add_rule("class", sv_required())
  iv$add_rule("name", sv_required())
  iv$add_rule("date", sv_required())
  iv$add_rule("strain", sv_required())
  iv$add_rule("hour", sv_required())
  iv$add_rule("replicate", sv_required())
  iv$add_rule("replicate", sv_integer())
  iv$add_rule("area", sv_required())
  iv$add_rule("area", sv_between(1, 56.75))
  iv$enable()

  #load past data
  reactive_data <- reactiveValues(data = past_data_df)
    
  #error message placeholder
  error_msg <- reactiveVal("")
    
  #add new data when action button clicked
  observeEvent(input$submitbutton, {
    #reset the error message
    error_msg("")
  
    #check that inputs pass validation
    if (!iv$is_valid()) {
      
      #set error message
      error_msg("All fields must be filled before adding the data.")
      
    } else {
      #add the new data to the existing data frame
      new_row <- data.frame(school = input$school,
                            teacher = input$teacher,
                            class = input$class,
                            name = input$name,
                            date = as.character(as.Date(input$date)),
                            strain = input$strain, 
                            hour = as.numeric(input$hour),
                            replicate = input$replicate,
                            area = as.numeric(input$area), 
                            submission="your data")
      new_row$timestamp <- as.character(Sys.time())
      reactive_data$data <- rbind(new_row, reactive_data$data)
      
      #read googlesheet
      values <- read_sheet(ss, sheet = "main")
  
      #check to see if googlesheet has any existing data.
      #if not, write to it and set up column names. 
      #otherwise, append to it.
      if (nrow(values) == 0) {
        sheet_write(data = new_row,
                    ss,
                    sheet = "main")
      } else {
        sheet_append(data = new_row,
                     ss,
                     sheet = "main")
      }
    }
  })

  output$contents <- renderPrint({
    # When the submit button hasn't been pressed yet
    if (input$submitbutton == 0) { 
      isolate("Submit your data to add to the graph.")
      
    } else {
      # Check if there are no errors (error_msg() is empty)
      if (error_msg() == "") {
        return("Submission complete.")
        
      } else {
        # Return the error message if there's an issue
        isolate(error_msg())
      }
    }
  })
    
  #timecourse graph
  output$dicty_clearing_plot <- renderPlot({
    ggplot(reactive_data$data, aes(x = as.numeric(hour), y = area, fill = submission, group = submission)) +
      geom_point(size=3, shape=21, color="black", alpha=0.5) +
      stat_summary(geom = "line", fun.y = mean, color="black") +
      facet_wrap(vars(strain))+
      scale_fill_manual(values = c("grey","red"))+
      labs(title = "Plot of strain vs area", x = "Hours", y = bquote("Area"~(cm^2)), fill = "Submission") +
      theme_bw(base_size = 16)
  })
  
  #day 3 vs day 5 graph
  #subset reactive data based on hour column
  filtered_data <- reactive({
    reactive_data$data %>%
      dplyr::filter(hour == 72 | hour == 120)  # filter day 3 and day 5
  })
  
  output$clearing_day3day5_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = as.factor(hour), y = area, fill = submission)) +
      geom_boxplot(position=position_dodge(width=0.9))+
      geom_point(size=3, shape=21, color="black",position=position_jitterdodge(dodge.width=0.9)) +
      facet_wrap(vars(strain))+
      scale_fill_manual(values = c("grey","red"))+
      labs(title = "Plot of strain vs area", x = "Hours", y = bquote("Area"~(cm^2)), fill = "Submission") +
      theme_bw(base_size = 16) 
  })
    
  # Subset the reactive data if strains have both 72h and 120h data
  filtered_diff_data <- reactive({
    reactive_data$data %>%
      dplyr::filter(hour %in% c(72, 120)) %>%
      group_by(school, teacher, class, name, date, strain, replicate) %>% # Group by the 'strain' column
      dplyr::filter(all(c(72, 120) %in% hour)) %>% # Keep only replicates with both 72h and 120h
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = hour, values_from = area, names_glue = "hour_{hour}") %>%
      dplyr::mutate(area_difference = hour_120 - hour_72)
  })
  
  #graph difference between Day 3 and Day 5
  output$clearing_diff_plot <- renderPlot({
    ggplot(filtered_diff_data(), aes(x = strain, y = area_difference, fill = submission)) +
      geom_boxplot(position=position_dodge(width=0.9))+
      geom_point(size=3, shape=21, color="black",position=position_jitterdodge(dodge.width=0.9)) +
      scale_fill_manual(values = c("grey","red"))+
      labs(title = "Difference between Day 3 and Day 5", x = "Strains", y = bquote("Difference in Area between Day 3 and 5"~(cm^2)), fill = "Submission") +
      theme_bw(base_size = 16) 
  })
    
  #tables
  output$data_table_all <- DT::renderDataTable(
    server = FALSE,
    DT::datatable({
      reactive_data$data  # Call the reactive expression to get the data
    },
    extensions = 'Buttons',
    
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip',
      buttons = list('copy', 'print', list(
        extend = 'collection',
        buttons = c('csv', 'excel'),
        text = 'Download'))
    ),
    
    class = "display"))
  
  output$data_table_diff <- DT::renderDataTable(
    server = FALSE,
    DT::datatable({
      filtered_diff_data()  # Call the reactive expression to get the data
    },
    extensions = 'Buttons',
    
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip',
      buttons = list('copy', 'print', list(
        extend = 'collection',
        buttons = c('csv', 'excel'),
        text = 'Download'))
    ),
    
    class = "display"
    ))
  
  #Explore tab - get data from google sheet and remove some columns
  sheet_data <- read_sheet(ss, sheet = "main")
  
  #combine google sheet with past data
  combined_data <- rbind(past_data_df, sheet_data) 
  
  #make day 3 and day 5 subset dataframe
  combined_data_day3day5 <- combined_data %>%
    dplyr::filter(hour == 72 | hour == 120)  # filter day 3 and day 5
  
  #make day 5 and day 3 difference dataframe
  combined_data_diff <- combined_data %>%
    dplyr::filter(hour %in% c(72, 120)) %>%
    group_by(school, teacher, class, name, date, strain, replicate) %>%
    dplyr::filter(all(c(72, 120) %in% hour)) %>% # Keep only replicates with both 72h and 120h
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = hour, values_from = area, names_glue = "hour_{hour}") %>%
    dplyr::mutate(area_difference = hour_120 - hour_72) %>%
    select(-name, -class, -submission)
  
  #reactive expression to filter school
  filtered_school <- reactive({
    #start with the full dataset
    df1 <- combined_data %>% select(-name, -class, -submission)
    df2 <- combined_data_day3day5 
    df3 <- combined_data_diff 
    
    #filter by school if a specific one is selected
    if (input$school_filter != "All schools") {
      df1 <- df1 %>% filter(school == input$school_filter) 
      df2 <- df2 %>% filter(school == input$school_filter)
      df3 <- df3 %>% filter(school == input$school_filter)
    }
    return(list(df1 = df1, df2 = df2, df3 = df3))
  })
  
  output$combined_timecourse_plot <- renderPlot({
    ggplot(filtered_school()$df1, aes(x = as.numeric(hour), y = area, fill = school, group = school, color = school)) +
      geom_point(size=3, shape=21, color="black", alpha=0.5) +
      stat_summary(geom = "line", fun.y = mean) +
      facet_wrap(vars(strain))+
      # scale_fill_manual(values = c("grey","red"))+
      labs(title = "Plot of strain vs area", x = "Hours", y = bquote("Area"~(cm^2)), fill = "Schools") +
      theme_bw(base_size = 16) +
      guides(colour = "none")
  })
  
  output$combined_day3day5_plot <- renderPlot({
    ggplot(filtered_school()$df2, aes(x = as.factor(hour), y = area, fill = school)) +
      geom_boxplot(position = position_dodge2(preserve = "single"))+
      geom_point(size=3, shape=21, color="black", position = position_jitterdodge(jitter.width = 0.25)) +
      facet_wrap(vars(strain))+
      # scale_fill_manual(values = c("grey","red"))+
      labs(title = "Plot of strain vs area", x = "Hours", y = bquote("Area"~(cm^2)), fill = "School") +
      theme_bw(base_size = 16) +
      guides(colour = "none")
  })
  
  #graph difference between Day 3 and Day 5
  output$combined_diff_plot <- renderPlot({
    ggplot(filtered_school()$df3, aes(x = strain, y = area_difference, fill = school)) +
      geom_boxplot(position = position_dodge2(preserve = "single"))+
      geom_point(size=3, shape=21, color="black", position = position_jitterdodge(jitter.width = 0.25)) +
      # scale_fill_manual(values = c("grey","red"))+
      labs(title = "Difference between Day 3 and Day 5", x = "Strains", y = bquote("Difference in Area between Day 3 and 5"~(cm^2)), fill = "School") +
      theme_bw(base_size = 16) +
      guides(colour = "none")
  })
  
  output$combined_data_table <- DT::renderDataTable(
    server = FALSE,
    DT::datatable({
      filtered_school()$df1  # Call the reactive expression to get the data
    },
    extensions = 'Buttons',
    
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip',
      buttons = list('copy', 'print', list(
        extend = 'collection',
        buttons = c('csv', 'excel'),
        text = 'Download'))
    ),
    
    class = "display"
    ))
}


# Run the application 
shinyApp(ui = ui, server = server)
