#Building prediction function 
predictfunction <- function(smoker,children,bmi,age){
  if(smoker == "yes" && children == 0 && bmi < 30){
    result = -956.74 + (251.20*age) + (505.18*bmi)
  } else if(smoker == "yes" && children == 0 && bmi >= 30) {
    result = 8120.10 + (292.16*age) + (614.01*bmi)
  } else if(smoker == "no" && children == 0 && bmi < 30){
    result = -3791.02 + (276.56*age) + (22.4*bmi)
  } else if(smoker == "no" && children == 0 && bmi >= 30){
    result = -465.01 + (254.86*age) + (-48.9*bmi)
  } else if(smoker == "yes" && children > 0 && bmi < 30){
    result = 2428.48 + (259.48*age) + (359.27*bmi)
  } else if(smoker == "yes" && children > 0 && bmi >= 30){
    result = 16021.03 + (253.72*age) + (447.91*bmi)
  } else if(smoker == "no" && children > 0 && bmi < 30){
    result = -2835.49 + (247.27*age) + (79.79*bmi)
  } else {
    result = -923.87 + (283.07*age) + (-35.83*bmi)
  }    
  return(result)
}

#call shiny & shinythemes packages
library(shiny)
library(shinythemes)

#Define UI for APP:
#Create navigation bar
ui <- navbarPage(
  theme = shinytheme("slate"),"Medical Cost Estimation",
  
  #Add first tab - "About Page"   
  tabPanel("About Page",
           
           #Create fluidPage in "About Page"
           fluidPage(
             tags$h3("In Estimation Page, please input related information:"),
             tags$h4("â¢ Smoker: Is the customer a smoker or not?"),
             tags$h4("â¢ Children: How many children does the customer have? "),
             tags$h4("â¢ BMI: What is the BMI of customer? BMI = weight(kg)/height(m)^2 or 703*weight(lb)/height(in)^2"),
             tags$h4("â¢ Age: What is the age of customer currently?"),
             tags$h3("After inputting above information, click Enter to calculate the estimated charge per year.")
           )),
  
  #Add second tab - "Estimation Page"
  tabPanel("Estimation Page",
           
           #Create sidebar in "Estimation Page"
           sidebarLayout(
             sidebarPanel(
               
               #Input 1 : smoker
               selectInput(inputId = "smoker",
                           label = 'Smoker',
                           c("yes","no")),
               
               #Input 2 : children
               sliderInput(inputId = "children",
                           label = 'Children',
                           value = 0, min = 0, max = 5),
               
               #Input 3 : BMI
               textInput(inputId = "bmi",
                         label = 'BMI',
                         value = ""),
               
               #Input 4 : Age
               sliderInput(inputId = "age",
                           label = 'Age',
                           value = 25, min = 18, max = 70),
               
               #Action Button to execute
               actionButton(inputId = "go",
                            label = "Enter")
             ),
             
             #Create mainpanel to show outputs
             mainPanel( 
               h4(textOutput(outputId = "smoker")),
               h4(textOutput(outputId = "children")),
               h4(textOutput(outputId = "bmi")),
               h4(textOutput(outputId = "age")),
               h4(textOutput(outputId = "charge"))
             )
           )
  )
)

#Define Server logic for APP
server <- function(input, output){
  
  #Output 1 : smoker
  output$smoker <- renderText(
    paste("smoker:",input$smoker))
  
  #Output 2 : children
  output$children <- renderText(
    paste("children:",input$children))
  
  #Output 3 : BMI
  output$bmi <- renderText(
    paste("bmi:",input$bmi))
  
  #Output 4 : Age
  output$age <- renderText(
    paste("age:",input$age))
  
  #Output 5 Charge
  output$charge <- eventReactive(input$go, {
    a=predictfunction(input$smoker,as.numeric(input$children),as.numeric(input$bmi),as.numeric(input$age))
    paste("charge:",a)
  })
}

# Create  Shiny APP
shinyApp(ui = ui, server = server)