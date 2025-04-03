#
# This is a Shiny web application for the EvolvingSTEM project.
#
# Author: Nanami Kubota

library(shiny)
library(shinyvalidate) #validate responses
library(htmltools)
library(ggplot2) #plot
library(googlesheets4) #save input to googlesheet
library(dplyr) #tidy
library(bslib) 
library(DT) #table

#save responses to this googlesheet
# run this once in RStudio locally:
# gs4_auth(cache = ".secrets")
# then run this afterwards
options(gargle_oauth_client_type = "installed")
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)

# google sheet link
ss <- readLines("googlesheet_link.txt")

# lists
choices_df <- read.csv("survey_choices.csv",stringsAsFactors = FALSE, na.strings = "")

hours <- c("72", "96", "120")

past_data_df <- read.csv("combined_data_cleanR.csv")
past_data_df$submission <- "other people"
past_data_df$hour <- as.numeric(past_data_df$hour)
past_data_df$timestamp <- NA

#github link
link_github <- tags$a(shiny::icon("github"),"See code on GitHub", href = "https://github.com/NanamiKubota/EvolvingSTEM", target = "_blank")

#user interface
ui <- page_navbar(
  title = "EvolvingSTEM",
  theme = bs_theme(preset = "flatly"),
  tags$head(tags$style(HTML('.bslib-value-box .value-box-value {font-size:18px;}'))), #font size of warning box
  nav_panel(
    title = "Graph",
    page_sidebar(
      sidebar = sidebar(
        title = "Input your data!",
        width = 300,
        selectizeInput("school", label = "School:", 
                       choices = na.omit(choices_df$school),
                       options = list(placeholder = 'Please select an option below', onInitialize = I('function() { this.setValue(""); }'))),
        textInput("other_school", "If you selected (Other), please input school name: "),
        selectizeInput("teacher", label = "Teacher:", 
                       choices = na.omit(choices_df$teacher), 
                       options = list(placeholder = 'Please select an option below', onInitialize = I('function() { this.setValue(""); }'))),
        textInput("other_teacher", "If you selected (Other), please input teacher name: "),
        selectizeInput("class", label = "Class period:", 
                       choices = as.character(seq(1, 12)),
                       options = list(placeholder = 'Please select an option below', onInitialize = I('function() { this.setValue(""); }'))),
        textInput("name", "Your name:"),
        dateInput("date", "Date of entry:",format = "yyyy-mm-dd"),
        selectizeInput("strain", "Strain:", 
                       choices = na.omit(choices_df$strain),
                       options = list(placeholder = 'Please select an option below', onInitialize = I('function() { this.setValue(""); }'))),
        selectizeInput("hour", "Hours since starting experiment:", hours, options = list(placeholder = 'Please select an option below', onInitialize = I('function() { this.setValue(""); }'))),
        numericInput("replicate", "Biological replicate number", min = 1, max = NA, value = NULL, step = 1),
        numericInput("area", "Dicty area cleared (cm²):", min = 1, max = 56.75, value = NULL, step = 0.01),
        numericInput("temperature", "Temperature (Celsius):", min = 10, max = 40, value = NULL, step = 1),
        
        actionButton("submitbutton", "Submit", class = "btn btn-danger ", width = '100%')
      ),
      uiOutput("contents", fill=TRUE),
      checkboxGroupInput("strain_filter", "Select strain(s) to view graph:", choices = na.omit(choices_df$strain), width = '100%', inline = TRUE),
    navset_tab(
      header = br(),
      nav_panel("Timecourse", plotOutput(outputId = "dicty_clearing_plot", height = "600px")),
      nav_panel("Day 3 vs Day 5", plotOutput(outputId = "clearing_day3day5_plot", height = "600px")),
      nav_panel("Difference", plotOutput(outputId = "clearing_diff_plot", height = "600px")))
    ),
  ),
  nav_panel(
    title = "Table",
    uiOutput("strain_message", fill=TRUE),
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
        title = "Filter data",
        width = 300,
        selectInput("school_explore_filter", label = "School:", 
                       choices = c("All schools", "Cooper Lab", na.omit(choices_df$school)), selected = "All schools"),
        selectInput("teacher_explore_filter", label = "Teachers(s):", 
                    choices = c("All teachers", na.omit(choices_df$teacher)), selected = "All teachers", multiple = TRUE),
        selectInput("strain_explore_filter", label = "Strain(s):", 
                    choices = c("All strains", na.omit(choices_df$strain)), selected = "All strains", multiple = TRUE),
        selectInput("class_explore_filter", label = "Class period(s):", 
                    choices = c("All class periods", na.omit(as.character(seq(1, 12)))), selected = "All class periods", multiple = TRUE),
        nav_spacer(),
        "Press the \"Refresh\" button below to grab the most up-to-date data!",
        actionButton("refreshbutton", "Refresh", class = "btn btn-primary ", width = '100%')),
    navset_tab(
      header = br(),
      nav_panel("Timecourse", plotOutput(outputId = "combined_timecourse_plot", height = "600px")),
      nav_panel("Day 3 vs Day 5", plotOutput(outputId = "combined_day3day5_plot", height = "600px")),
      nav_panel("Difference", plotOutput(outputId = "combined_diff_plot", height = "600px")),
      nav_panel("Table", DT::dataTableOutput("combined_data_table"))
    ))
  ),
  nav_spacer(),
  nav_item(tags$div(tags$span("Made by Nanami Kubota using R Shiny"))),
  nav_item(link_github)

)

server <- function(input, output, session) {
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
  iv$add_rule("strain_filter", sv_required())
  iv$enable()

  #load past data
  reactive_data <- reactiveValues(data = past_data_df)
    
  #error message placeholder
  error_msg <- reactiveVal("")
  
  #read googlesheet
  sheet_data <- read_sheet(ss, sheet = "main")
  
  #add new data when action button clicked
  observeEvent(input$submitbutton, {
    #reset the error message
    error_msg("")
  
    #check that inputs pass validation
    if (!iv$is_valid()) {
      
      #set error message
      error_msg("All required fields must be filled before submitting data.")
      
    } else {
      #add the new data to the existing data frame
      new_row <- data.frame(school = input$school,
                            other_school = input$other_school,
                            teacher = input$teacher,
                            other_teacher = input$other_teacher,
                            class = input$class,
                            name = input$name,
                            date = as.character(as.Date(input$date)),
                            strain = input$strain, 
                            hour = as.numeric(input$hour),
                            replicate = input$replicate,
                            area = as.numeric(input$area),
                            temperature = as.numeric(input$temperature),
                            submission="your data")
      new_row$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
      reactive_data$data <- rbind(new_row, reactive_data$data)
  
      #check to see if googlesheet has any existing data.
      #if not, write to it and set up column names. 
      #otherwise, append to it.
      if (nrow(sheet_data) == 0) {
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
  
  contents_value <- reactive({
    if (input$submitbutton == 0){
      "Submit your data to add to the graph. Remember to checkmark your strains below."
    }else{
      # Check if there are no errors (error_msg() is empty)
      if (error_msg() == "") {
        paste0("Submission complete. ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
      } else {
        error_msg() # Return the error message if there's an issue
      }
    }
  })
  
  contents_theme <- reactive({
    if (input$submitbutton == 0){
      "light"
    }else{
      if (error_msg() == ""){
        "success"
      }else{
        "danger"
      }
    }
  })
  
  output$contents <- renderUI({
    value_box(
      title = NULL,
      value = contents_value(),
      theme = contents_theme(),
      fill = FALSE,
      max_height = '15%'
    )
  })
  
  
  strain_message_value <- reactive({
    if (length(input$strain_filter) == 0){
      "Warning: no strains selected. Go to \"Graph\" tab and checkmark your strain(s) to view table."
    }else{
      renderText({
        paste("Strains selected: ", paste(input$strain_filter, collapse = ", "))
      })
    }
  })
  
  strain_message_theme <- reactive({
    if (length(input$strain_filter) == 0){
      "danger"
    }else{
      "info"
    }
  })
  
  output$strain_message <- renderUI({
    value_box(
      title = NULL,
      value = strain_message_value(),
      theme = strain_message_theme(),
      fill = FALSE,
      max_height = '15%'
    )
  })
  
  #reactive expression to filter by strain (graph tab)
  filtered_strain <- reactive({
    req(input$strain_filter)
    
    reactive_data$data %>%
      dplyr::filter(strain %in% input$strain_filter)  # filter day 3 and day 5
  })
    
  #timecourse graph
  output$dicty_clearing_plot <- renderPlot({
    
    #only render plot if there is data (if strains are all unchecked, no plot is shown)
    req(nrow(filtered_strain()) > 0)
    
    ggplot(filtered_strain(), aes(x = as.numeric(hour), y = area, fill = submission, group = submission)) +
      stat_summary(geom = "line", fun.y = mean, aes(color=submission), size=2) +
      geom_point(size=3, shape=21, color="black", alpha=0.5) +
      scale_x_continuous(breaks = c(72,96,120)) +
      facet_wrap(vars(strain), axes = "all_x")+
      scale_color_manual(values = c("grey","red"))+
      scale_fill_manual(values = c("grey","red"))+
      labs(
        title = "Dicty clearing over time",
        x = "Hours", 
        y = bquote("Area"~(cm^2)), 
        fill = "Submission") +
      guides(color = "none")+
      theme_bw(base_size = 18)+
      theme(legend.position = "top",
            plot.title = element_text(hjust = 0.5))
  })
  
  #day 3 vs day 5 graph
  #subset reactive data based on hour column
  filtered_data <- reactive({
    
    req(nrow(filtered_strain()) > 0)
    
    filtered_strain() %>%
      dplyr::filter(hour == 72 | hour == 120)  # filter day 3 and day 5
  })
  
  output$clearing_day3day5_plot <- renderPlot({
    
    #only render plot if there is data (if strains are all unchecked, no plot is shown)
    req(nrow(filtered_strain()) > 0, nrow(filtered_data()) > 0)
    
    ggplot(filtered_data(), aes(x = as.factor(hour), y = area)) +
      geom_boxplot(aes(fill = submission), position = position_dodge(width = 0.9))+
      geom_point(aes(fill = submission), size=3, shape=21, alpha=0.5, color="black", position=position_jitterdodge(dodge.width=0.9, jitter.width = 0.1)) +
      stat_summary(fun=base::mean, aes(group=submission), geom="point", size=3, shape=23, color="black", fill="black", position = position_dodge(width = 0.9)) +
      facet_wrap(vars(strain), axes = "all_x")+
      scale_fill_manual(values = c("grey","red"))+
      labs(
        title = "Amount of Dicty Predation",
        x = "Hours", y = bquote("Area"~(cm^2)), fill = "Submission") +
      theme_bw(base_size = 18) +
      theme(legend.position = "top",
            legend.title = element_text(size = 14),
            plot.title = element_text(hjust = 0.5))
  })
    
  # Subset the reactive data if strains have both 72h and 120h data
  filtered_diff_data <- reactive({
    
    #only render if there is data (if strains are all unchecked, no plot is shown)
    req(nrow(filtered_strain()) > 0)
    
    filtered_strain() %>%
      dplyr::filter(hour %in% c(72, 120)) %>%
      group_by(school, teacher, class, name, date, strain, replicate) %>% # Group by the 'strain' column
      dplyr::filter(all(c(72, 120) %in% hour)) %>% # Keep only replicates with both 72h and 120h
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = hour, values_from = area, names_glue = "hour_{hour}") %>%
      dplyr::mutate(area_difference = hour_120 - hour_72)
  })
  
  #graph difference between Day 3 and Day 5
  output$clearing_diff_plot <- renderPlot({
    
    #only render if there is data (if strains are all unchecked, no plot is shown)
    req(nrow(filtered_strain()) > 0, nrow(filtered_data()) > 0)
    
    ggplot(filtered_diff_data(), aes(x = strain, y = area_difference)) +
      geom_boxplot(aes(fill = submission), position = position_dodge(width = 0.9))+
      geom_point(aes(fill = submission),size=3, shape=21, alpha=0.5, color="black", position=position_jitterdodge(dodge.width=0.9, jitter.width = 0.1)) +
      stat_summary(fun=base::mean, aes(group=submission), geom="point", size=3, shape=23, color="black", fill="black", position = position_dodge(width = 0.9)) +
      scale_fill_manual(values = c("grey","red"))+
      labs(
        title = "Amount of Dicty clearing from Day 3 to Day 5", 
        x = "Strains", y = bquote("Difference in Area between Day 3 and 5"~(cm^2)), fill = "Submission") +
      theme_bw(base_size = 18) +
      theme(legend.position = "top",
            legend.title = element_text(size = 14),
            plot.title = element_text(hjust = 0.5))
  })
    
  #tables
  output$data_table_all <- DT::renderDataTable(
    server = FALSE,
    DT::datatable({
      filtered_strain()  # Call the reactive expression to get the data
    },
    extensions = 'Buttons',
    
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      order = list(11, 'desc'),
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
      order = list(9, 'desc'),
      dom = 'Bfrtip',
      buttons = list('copy', 'print', list(
        extend = 'collection',
        buttons = c('csv', 'excel'),
        text = 'Download'))
    ),
    
    class = "display"
    ))
  
  #Explore tab - get data from google sheet and remove some columns
  
  
  observeEvent(input$refreshbutton,
               ignoreNULL = FALSE,
               {
                 sheet_data <- read_sheet(ss, sheet = "main")
                 
                 #combine google sheet with past data
                 combined_data <- rbind(past_data_df, sheet_data) 
                 
                 #make day 3 and day 5 subset dataframe
                 combined_data_day3day5 <- combined_data %>%
                   dplyr::filter(hour == 72 | hour == 120)  # filter day 3 and day 5
                 
                 #make day 5 and day 3 difference dataframe
                 combined_data_clean <- combined_data %>%
                   dplyr::filter(hour == 72 | hour == 120)
                 
                 #identify duplicate rows that would cause errors when pivot_wider
                 combined_data_clean_dup <- combined_data_clean %>%
                   dplyr::summarise(n = dplyr::n(), .by = c(school, teacher, class, name, strain, replicate, hour)) %>%
                   dplyr::filter(n > 1L) 
                 
                 combined_data_diff <- combined_data_clean %>%
                   dplyr::distinct(school, teacher, class, date, name, strain, hour, replicate, .keep_all = TRUE) %>%
                   dplyr::anti_join(combined_data_clean_dup, by = c("school", "teacher", "class", "name", "strain", "replicate")) %>%
                   tidyr::pivot_wider(names_from = hour, values_from = area, names_prefix = "hour_", id_cols = c("school", "teacher", "class", "name", "strain", "replicate")) %>%
                   dplyr::filter(!is.na(hour_120) & !is.na(hour_72)) %>%
                   dplyr::mutate(area_difference = hour_120 - hour_72) 
                 
                 #reactive expression to filter school
                 filtered_school <- reactive({
                   #start with the full dataset
                   df1 <- combined_data %>% select(-name, -submission)
                   df2 <- combined_data_day3day5 
                   df3 <- combined_data_diff 
                   
                   #filter by school if a specific one is selected
                   if (input$school_explore_filter != "All schools") {
                     df1 <- df1 %>% filter(school == input$school_explore_filter) 
                     df2 <- df2 %>% filter(school == input$school_explore_filter)
                     df3 <- df3 %>% filter(school == input$school_explore_filter)
                   }
                   #filter by strain if a specific one is selected
                   if (!("All strains" %in% input$strain_explore_filter)) {
                     df1 <- df1 %>% filter(strain %in% input$strain_explore_filter) 
                     df2 <- df2 %>% filter(strain %in% input$strain_explore_filter) 
                     df3 <- df3 %>% filter(strain %in% input$strain_explore_filter) 
                   }
                   if (!("All teachers" %in% input$teacher_explore_filter)) {
                     df1 <- df1 %>% filter(teacher %in% input$teacher_explore_filter) 
                     df2 <- df2 %>% filter(teacher %in% input$teacher_explore_filter) 
                     df3 <- df3 %>% filter(teacher %in% input$teacher_explore_filter) 
                   }
                   if (!("All class periods" %in% input$class_explore_filter)) {
                     df1 <- df1 %>% filter(class %in% input$class_explore_filter) 
                     df2 <- df2 %>% filter(class %in% input$class_explore_filter) 
                     df3 <- df3 %>% filter(class %in% input$class_explore_filter) 
                   }
                   return(list(df1 = df1, df2 = df2, df3 = df3))
                 })
                 
                 output$combined_timecourse_plot <- renderPlot({
                   ggplot(filtered_school()$df1, aes(x = as.numeric(hour), y = area, fill = school, group = school, color = school)) +
                     stat_summary(geom = "line", fun.y = mean, size=2) +
                     geom_point(size=3, shape=21, color="black", alpha=0.5) +
                     scale_x_continuous(breaks = c(72,96,120))+
                     facet_wrap(vars(strain), axes = "all_x")+
                     # scale_fill_manual(values = c("grey","red"))+
                     labs(
                       title = "Dicty clearing over time", 
                       x = "Hours", y = bquote("Area"~(cm^2)), fill = "Schools") +
                     theme_bw(base_size = 18) +
                     guides(color = "none",
                            fill = guide_legend(nrow = 1))+
                     theme(legend.position = "top",
                           legend.title = element_blank(),
                           legend.text = element_text(size = 12),
                           plot.title = element_text(hjust = 0.5))
                 })
                 
                 output$combined_day3day5_plot <- renderPlot({
                   ggplot(filtered_school()$df2, aes(x = as.factor(hour), y = area)) +
                     geom_boxplot(aes(fill = school), position = position_dodge(width = 0.9))+
                     geom_point(aes(fill = school), size=3, shape=21, alpha=0.5, color="black", position=position_jitterdodge(dodge.width=0.9, jitter.width = 0.1)) +
                     stat_summary(fun=base::mean, aes(group=school), geom="point", size=3, shape=23, color="black", fill="black", position = position_dodge(width = 0.9)) +
                     facet_wrap(vars(strain), axes = "all_x")+
                     # scale_fill_manual(values = c("grey","red"))+
                     labs(
                       title = "Amount of Dicty Predation", 
                       x = "Hours", y = bquote("Area"~(cm^2)), fill = "School") +
                     theme_bw(base_size = 18) +
                     guides(colour = "none",
                            fill = guide_legend(nrow = 1)) +
                     theme(legend.position = "top",
                           legend.title = element_blank(),
                           legend.text = element_text(size = 12),
                           plot.title = element_text(hjust = 0.5))
                 })
                 
                 #graph difference between Day 3 and Day 5
                 output$combined_diff_plot <- renderPlot({
                   ggplot(filtered_school()$df3, aes(x = as.factor(strain), y = area_difference)) +
                     geom_boxplot(aes(fill = school), position = position_dodge(width = 0.9))+
                     geom_point(aes(fill = school),size=3, shape=21, alpha=0.5, color="black", position=position_jitterdodge(dodge.width=0.9, jitter.width = 0.1)) +
                     stat_summary(fun=base::mean, aes(group=school), geom="point", size=3, shape=23, color="black", fill="black", position = position_dodge(width = 0.9)) +
                     labs(
                       title = "Amount of Dicty clearing from Day 3 to Day 5", 
                       x = "Strains", y = bquote("Difference in Area between Day 3 and 5"~(cm^2)), fill = "School") +
                     theme_bw(base_size = 18) +
                     guides(colour = "none",
                            fill = guide_legend(nrow = 1)) +
                     theme(legend.position = "top",
                           legend.title = element_blank(),
                           legend.text = element_text(size = 12),
                           plot.title = element_text(hjust = 0.5))
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
                     order = list(9, 'desc'),
                     dom = 'Bfrtip',
                     buttons = list('copy', 'print', list(
                       extend = 'collection',
                       buttons = c('csv', 'excel'),
                       text = 'Download'))
                   ),
                   
                   class = "display"
                   ))
               }
               )
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
