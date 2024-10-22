library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(tidyverse)
library(boxr)
library(googlesheets4)


# Load in the tuition data frame

diversity_school <- read.csv("https://raw.githubusercontent.com/Wendy032024/stat-436-HW-2-datasets/refs/heads/main/diversity_school.csv")
salary_potential <- read.csv("https://raw.githubusercontent.com/Wendy032024/stat-436-HW-2-datasets/refs/heads/main/salary_potential.csv")
tuition_cost <- read.csv("https://raw.githubusercontent.com/Wendy032024/stat-436-HW-2-datasets/refs/heads/main/tuition_cost.csv")


# Reshape the dataframe to be more tidy to visualize
tuition_long <- tuition_cost %>%
  pivot_longer(cols = c(in_state_tuition, out_of_state_tuition),
               names_to = "tuition_type", 
               values_to = "tuition") %>%
  filter(!is.na(tuition))


# functions for early career pay--------

# Selected colleges

df1 <- tuition_cost %>% filter(name %in% unique(salary_potential$name))
df2 <- left_join(df1, salary_potential, by = "name") %>%
  filter(type %in% c("Public", "Private"))


earlyscatterPlot <- function(df, selected){
  
  df %>% mutate(selected = selected) %>%
    ggplot(aes(x = out_of_state_tuition, y = early_career_pay, color = type)) +
    geom_point(aes(alpha = as.numeric(selected))) +
    scale_alpha(range = c(0.1, 1)) +  # You can customize the labels if needed
    scale_color_manual(values = c("Public" = "lightblue", "Private" = "pink")) +
    labs(color = "College Type", alpha = "Selection Status", x = "Out of State Tuition", y = "Early Career Pay") +  # Change here
    ggtitle("Relationship between Early Career Pay and Out of State Tuition") +
    theme_bw()
}

# Early career pay distribution
earlyPayDensity <- function(df, selected){
  
  df %>% mutate(selected = selected) %>%
    ggplot(aes(early_career_pay, fill = type)) + 
    geom_density(alpha = 0.5, adjust = 1.5) +
    scale_alpha(range = c(0.3, 1)) +
    labs(title = "Early Career Pay Distribution: Public vs. Private Colleges",
         x = "Early Career Pay", y = "Density",
         fill = "College Type") + theme_minimal()
}


# Functions for diversity statistics---------

filter_df <- function(df, selected_) {
  filter(df, selected_) |>
    select(name, state, type, degree_length,
           out_of_state_tuition, early_career_pay)
}


# Diversity school names
diversity_school_names <- na.omit(unique(diversity_school$name))
diversity_wide <- diversity_school %>%
  group_by(name, total_enrollment, state, category) %>%
  summarise(enrollment = sum(enrollment, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = category, values_from = enrollment) %>%
  mutate(Women = as.numeric(Women), 
         Men = total_enrollment - Women)

# Pie chart function
diversityBarPlot <- function(collegeName){
  diversity_wide %>%
    filter(name %in% collegeName) %>%
    pivot_longer(cols = c(Women, Men), 
                 names_to = "gender", values_to = "enrollment") %>%
    group_by(name) %>%
    mutate(total_enrollment = sum(enrollment),
           proportion = enrollment / total_enrollment) %>%
    ggplot(aes(x = name, y = enrollment, fill = gender)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = scales::percent(proportion, accuracy = 0.1), 
                  y = enrollment + 50), 
              position = position_stack(vjust = 0.5)) +  
    labs(title = "Enrollment by Gender in Different Schools",
         x = "School", 
         y = "Number of Enrolled Students",
         fill = "Gender") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90)) 
}



diversityPieChart <- function(collegeNames){
  # Define the categories of interest (excluding the first category if not needed)
  category_remaining <- unique(diversity_school$category)[2:10]
  
  # Filter data for the selected schools and categories
  diversity_school %>%
    filter(name %in% collegeNames, 
           category %in% category_remaining) %>%
    group_by(name) %>%
    mutate(total_enrollment = sum(enrollment),  
           proportion = enrollment / total_enrollment,
           label = scales::percent(proportion, accuracy = 0.1)) %>%  
    ungroup() %>%
    ggplot(aes(x = "", y = proportion, fill = category)) + 
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = label), 
              position = position_stack(vjust = 0.5)) +  
    facet_wrap(~ name, ncol = 2) +  
    labs(title = "Diversity Distribution in Ethnicity for Selected Universities",
         fill = "Category") +
    theme_void() +
    theme(legend.position = "right")
}



# The start of the R shiny app-------
ui <- navbarPage("The U.S College Statistics by State",
                 tabPanel("College Search",
                          fluidPage(
                            titlePanel("Create a list of schools that are right for you!"),
                            h5("This tab is designed provide users a 
                               comprehensive list for the colleges that meet their specific requirements."),
                            
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("state", "Please select a state where you would like to attend college:",
                                            choices = unique(tuition_cost$state[!is.na(tuition_cost$state)]), selected = "Wisconsin"),
                                selectInput("residency", "Please select your residency relative to that state:",
                                            choices = c("in_state_tuition", "out_of_state_tuition")),
                                sliderInput("tuitionRange", "Please select the range of the tuition you would like to pay:",
                                            min = 0, max = max(tuition_long$tuition, na.rm = TRUE), value = c(10000, 50000)),
                                selectInput("collegeType", "Please select the types of college:",
                                            choices = unique(tuition_cost$type)),
                                selectInput("degreeLength", "Please select degree length:",
                                            choices = unique(tuition_cost$degree_length),
                                            selected = "4 Year")
                              ),
                              
                              mainPanel(
                                DTOutput("table")
                              )
                            )
                          )
                          ),
                 tabPanel("Post-graduation Performance Statistics",
                          fluidPage(
                            titlePanel("More investment into higher education = More return in the salary?"),
                            h5("It is natural to think that higher costs for colleges means 
                            the more valuable knowledge and skills that students can be trained on.
                               But is this logic necessary for a higher salary after graduation?"),
                            h5("You can brush through a range of the points on the scatter plot or brush in the x-direction 
                               on the histogram to look up for corresponding school information for a certain early career pay range."),
                            
                            fluidRow(
                              column(6,
                                     plotOutput("scatterPlotBetweenCostandReturn", brush = "plot_brushed")
                                     ),
                              column(6,
                                     plotOutput("density", brush = brushOpts(id = "plot_brushed", direction = "x"))
                                     ),
                              DTOutput("selectedColleges")

                            )
                          )
                          ),
                 tabPanel("Diversity Statistics",
                          fluidPage(
                            titlePanel("Does your dream college have high diversity?"),
                            h5("This diversity statistics mainly focus on two aspects of diversity,
                               the first one is gender diversity, the second one is ethnicity diversity"),
                            h5("You can enter multiple school names to look up and compare for the diversity!"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                selectizeInput("schoolName", "Please enter the school name you want to look up for:",
                                               choices = diversity_school_names,
                                               selected = "University of Wisconsin at Madison",
                                               multiple = TRUE)
                                
                              ),
                              mainPanel(
                                plotOutput("ethnicityDiversity"),
                                plotOutput("genderdiversity")
                                
                                
                              )
                            )
                            
                            
                            )
                          )
)



server <- function(input, output) {
  # For tab1
  qualifiedTable <- reactive({
    tuition_long %>%
      filter(state == input$state,
             tuition_type == input$residency,
             type == input$collegeType,
             degree_length == input$degreeLength,
             tuition >= input$tuitionRange[1] & 
               tuition <= input$tuitionRange[2]) %>%
      select(name, state, type, degree_length, 
             tuition_type, tuition)
  })
  
  output$table <- renderDataTable({
    qualifiedTable()
  })
  
  # For tab2

  selectedSchools <- reactiveVal(rep(TRUE, nrow(df2)))

  observeEvent(
    input$plot_brushed,
      selectedSchools(brushedPoints(df2, input$plot_brushed, allRows = T)$selected_)


  )


  output$scatterPlotBetweenCostandReturn <- renderPlot({
    earlyscatterPlot(df2, selectedSchools())

  })

  output$density <- renderPlot({
    earlyPayDensity(df2, selectedSchools())

  })

  output$selectedColleges <- renderDT(filter_df(df2, selectedSchools()))
  
  
  
  # For tab3
  
  output$genderdiversity <- renderPlot({
    diversityBarPlot(input$schoolName)
  })

  output$ethnicityDiversity <- renderPlot({
    diversityPieChart(input$schoolName)
  })
  
}

shinyApp(ui, server)





