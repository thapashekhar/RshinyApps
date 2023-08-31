library(shiny)
library(tidyverse)
library(patchwork)

######################### Data importing and cleaning ################################################

gundata <- read_csv("./data/full_data.csv", na ="NA") %>%
  mutate(intent = str_to_lower(intent)) # converting all intent values to lower case


####################### age group dividing function ###############################################

add_age_group <- function(Data_WO_age){
  Data_WO_age<- Data_WO_age %>%
    mutate(age_group = case_when(
      age>=0 & age<=9 ~ "0-9",
      age>=10 & age<=19 ~ "10-19",
      age>=20 & age<=29 ~ "20-29",
      age>=30 & age<=39 ~ "30-39",
      age>=40 & age<=49 ~ "40-49",
      age>=50 & age<=59 ~ "50-59",
      age>=60 & age<=69 ~ "60-69",
      age>=70 & age<=79 ~ "70-79",
      age>=80 & age<=89 ~ "80-89",
      age>=90 ~ "90+"
    ))
  return(Data_WO_age)
}

############################# ploting function ################################################

plot_gun_data <- function(intentval,covar, policevar = "all"){
  
  # filtering data as per police variable
  if (policevar ==1){
    data1 <- gundata %>%
      filter(police==1)}
  else if (policevar==0){
    data1 <- gundata %>%
      filter(police==1)}
  else if (policevar == "all"){
    data1 <- gundata %>%
      filter(police==1 | police==0)}
  
  # converting intent and covar to lower case
  intentval <- str_to_lower(intentval)
  covar <- str_to_lower(covar)
  
  # defining not in operator
  `%!in%` <- Negate(`%in%`)
  
  # checking if intentval and covar is in the dataset
  if (intentval %!in% c("suicide","undetermined","accidental","homicide")){
    stop("Error: The given intentval (", intentval,") is not in the data set.")
  }
  if (covar %!in% c("sex","age","race","place","education")){
    stop("Error: The given covar (", covar,") is not in the data set.")
  }
  
  # if covar is age, then calling add_age_group function to create the age_group column
  if (covar =="age"){
    data1 <- add_age_group(data1)
    covar = "age_group"
  }
  
  # filtering data by intentval and grouping it by covar and then counting the numbers of value in covar
  data1 <- data1%>%
    filter(intent == intentval) %>%
    group_by_at(covar) %>%
    tally()%>%
    drop_na()
  # renaming the columns of the above data1 
  colnames(data1) <-c("name","value")
  
  #plotting
  pb <- ggplot(data1) +
    geom_col(mapping = aes(x = name , y= value), orientation ="x")+
    labs(y ="count",
         x = {covar},
         title = paste("Number of gun",intentval, "victims from 2012-2014 by", covar)
    )+
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 18))+
    scale_y_continuous(n.breaks = 6)+
    coord_flip()
  return(pb)
  
}

################################ user interface ###############################################################################
ui <- fluidPage(
  # App title
  titlePanel(strong("Gundeath Data Info")),
  sidebarLayout(
    sidebarPanel(
      radioButtons("intent",
                   h3("Cause of Gundeath"),
                   choices = list("Homicide" = "homicide","Suicide" = "suicide","Accidental"="accidental","Undetermined"="undetermined"), selected = "homicide"),
      helpText("Select one of the above causes of Gundeath to explore in plot"),
      radioButtons("covar",
                   h3("Select the variable to plot against"),
                   choices = list("Age" = "age","Sex" = "sex","Race"="race","Location of death"="place","Education"="education"), selected = "age"),
      helpText("Select one of the above variables to plot against."),
      radioButtons("police",
                   h3("Police involvement"),
                   choices = list("All"="all","Police involved"=1, "Police not involved"=0), selected = "all"),
      helpText("Select one of the above options to include either all data points or only data points that have police involvement or not involvement.")
      
    ),
    
    mainPanel(
      plotOutput("gunPlot", height = "650px")
      
    )
  )
)

################################### server ##########################################################################
server <- function(input, output) {
  output$gunPlot <- renderPlot({plot_gun_data(input$intent, input$covar, input$police)})
  
}


################ run app #####################
shinyApp(ui = ui, server = server)
