library(shiny)
library(tidyverse)
library(patchwork)

######################### Data importing and cleaning ################################################

collegedata <- read_csv("./data/MERGED2015_16_PP.csv",na=c("NA", "NULL", "PrivacySuppressed" ))
college_data_set <- collegedata %>%
  # selecting required columns
  dplyr::select(INSTNM, ADM_RATE, TUITIONFEE_IN, AVGFACSAL, DEBT_MDN, CONTROL, ICLEVEL) %>%
  rename("institute" = INSTNM, "admission_rate" = ADM_RATE, "in_state_tuition_fee" = TUITIONFEE_IN, "average_faculty_salary" = AVGFACSAL,"median_debt" = DEBT_MDN, "ownership" = CONTROL, "level" = ICLEVEL)

############################# function to plot histogram #############################################

histogram <- function(nameInstitute,variable_name, value_for_institute){
  
  # plotting histogram
  p<- ggplot() +
    geom_histogram(mapping = aes(x = pull(variable_name) ), fill="orange", color ="red", bins = 30)+
    labs(x =names(variable_name),
         y = "count",
         title = str_replace_all(names(variable_name),"_"," "))+
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16))
  
  m_count <- max((ggplot_build(p)$data[[1]])["count"]) # determining the maximum count or height of hieightest peak in histogram.
  
  # plotting vertical line and name of the institution
  if (!is.na(value_for_institute)){
    p <- p +
      geom_vline(xintercept = value_for_institute, color = "black")+
      annotate("text", x=value_for_institute, y = m_count/2, size=4.5, angle =90, label = nameInstitute)
  }
  
  return(p)
}

################################## function to plot college info ######################################

plot_college <- function(name_of_institute, variables = c("admission_rate","average_faculty_salary","in_state_tuition_fee","median_debt"), match_control = T, match_IC = T){
  # defining a not in operator
  `%!in%` <- Negate(`%in%`)
  
  # checking the user inputs are correct 
  L= length(variables)
  if (name_of_institute %!in% college_data_set$institute){
    stop("Error: The entered name of institution is wrong or not available in the dataset. Please check the name of the institution again!! ")
  }
  for (i in 1:L){
    if (variables[i] %!in% c("admission_rate","average_faculty_salary","in_state_tuition_fee","median_debt")){
      stop('Error: The entered name of variables (',variables,') must be among c("admission_rate","average_faculty_salary","in_state_tuition_fee","median_debt"')}
  }
  
  
  # determining the control and level for the institute 
  C = filter(college_data_set, institute == name_of_institute)[[1,"ownership"]]
  Lev = filter(college_data_set, institute == name_of_institute)[[1,"level"]]
  
  # filtering the data as per control and level of the institution
  college_R <- college_data_set
  if (match_control == T){
    college_R <- college_R %>%
      filter(ownership == C)}
  if (match_IC == T){
    college_R <- college_R %>%
      filter(level == Lev)}
  
  # plotting by calling histogram function
  if (L==1){
    h <- histogram(name_of_institute, college_R[variables[1]], filter(college_R, institute == name_of_institute)[[1,variables[1]]])
    hist_plot <- h
  } else if (L==2){
    h1<- histogram(name_of_institute, college_R[variables[1]], filter(college_R, institute == name_of_institute)[[1,variables[1]]])
    h2<- histogram(name_of_institute, college_R[variables[2]], filter(college_R, institute == name_of_institute)[[1,variables[2]]])
    hist_plot <- (h1 / h2)
  } else if (L==3){
    h1<- histogram(name_of_institute, college_R[variables[1]], filter(college_R, institute == name_of_institute)[[1,variables[1]]])
    h2<- histogram(name_of_institute, college_R[variables[2]], filter(college_R, institute == name_of_institute)[[1,variables[2]]])
    h3<- histogram(name_of_institute, college_R[variables[3]], filter(college_R, institute == name_of_institute)[[1,variables[3]]])
    hist_plot <- (h1 + h2) / (h3 + plot_spacer())
  } else if (L== 4){
    h1<- histogram(name_of_institute, college_R[variables[1]], filter(college_R, institute == name_of_institute)[[1,variables[1]]])
    h2<- histogram(name_of_institute, college_R[variables[2]], filter(college_R, institute == name_of_institute)[[1,variables[2]]])
    h3<- histogram(name_of_institute, college_R[variables[3]], filter(college_R, institute == name_of_institute)[[1,variables[3]]])
    h4<- histogram(name_of_institute, college_R[variables[4]], filter(college_R, institute == name_of_institute)[[1,variables[4]]])
    hist_plot <- ((h1 + h2)  / (h3 + h4))
  }
  
  return( hist_plot + plot_annotation(
    caption = paste("Note: Histograms include institutions- Match Control =", match_control,",", "Match Level =", match_IC, "with",  name_of_institute,"."),theme = theme(plot.caption = element_text(size=12))))
  
}

################################ user interface ###############################################################################
ui <- fluidPage(
  # App title
  titlePanel(strong("College Information App")),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("inst", 
                  label = h3(strong("Institution Name")),
                  choices = college_data_set$institute,
                  selected = "University of Georgia"
                  ),
      helpText("Select or type the name of institution to view its information. If a vertical line is missing in one of the plots, that means the corresponding data is missing for the insitutuion."),
      
      checkboxGroupInput("variables",
                         h3("Select Variables to Plot"),
                         choices = list("Admission Rate"="admission_rate","Average Faculty Salary"="average_faculty_salary", "In-State Tuition Fee"="in_state_tuition_fee", "Median Debt"="median_debt"),
                         selected = c("admission_rate","average_faculty_salary","in_state_tuition_fee","median_debt")),
      helpText("Select any or all of the above variables to see the corresponding plots."),
      radioButtons("match_control",
                   h3("Match Control?"),
                   choices = list("Yes" = TRUE,"No" = FALSE), selected = TRUE),
      helpText("If Match Control is 'Yes', then the histograms will have only those institutions that match ownership or control (public, private-not-for-profit, and private-for-profit) with the target institution."),
      
      radioButtons("match_level",
                   h3("Match Level?"),
                   choices = list("Yes" = TRUE,"No" = FALSE), selected = TRUE),
      helpText("If Match Level is 'Yes', then the histograms will have only those institutions that match level(4-year, 2-year, and less-than-2-year) with the target institution.")
      
    ),
    
    mainPanel(
      plotOutput("collegePlot", height = "800px")
      
    )
  )
)

################################### server ##########################################################################
server <- function(input, output) {
  #updateSelectizeInput(session, "inst", choices = college_data_set$institute, server = TRUE,selected = "University of Georgia")
  output$collegePlot <- renderPlot({plot_college(input$inst, input$variables, input$match_control,input$match_level)})
  
}


################ run app #####################
shinyApp(ui = ui, server = server)
