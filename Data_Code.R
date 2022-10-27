####################################
# Muhammad Bilal Jamil                  #
# https://www.linkedin.com/in/muhammadbilaljamil/ #
# https://www.youtube.com/channel/UCjqAw_8eeblbHjpNxM8n5LA/ #
# http://github.com/mbjamil92/  #
####################################


library(ggplot2)
library(shiny)
library(shinydashboard)
library(dplyr)
library(shinythemes)


################# User Interface Code ################# 

ui <- fluidPage(theme = shinytheme("cerulean"),
  tags$head(
    tags$style(HTML("
      body {
      background-color: white !important
      }"))),br(),
             titlePanel(h1("Linear and Logistic Regression Application", align = 'Center')),br(),
  #helpText("", align='Center'),
  sidebarLayout(
    sidebarPanel(
                  actionButton("btn1","Dataset#1 View (UN)", width = '100%'), br(),
                  actionButton("btn2","Linear Regression", width = '100%'), br(),
                  actionButton("btn3","Dataset#2 View (Covid)", width = '100%'),br(),
                  actionButton("btn4","Logistic Regression", width = '100%'),br(),
    ),
    mainPanel(
      tableOutput("Summary"),
      tabsetPanel(id = "tabset1",
                  tabPanel("Data_1", value = "ds1", DT::dataTableOutput("table1")),br(),
                  tabPanel("LG", value = "lg",br(),
                    helpText("Linear Regression Model result:", align='center'),br(),
                    tabPanel("Linear Regression Model result",verbatimTextOutput("table1b")),br(),
                    helpText("Linear Regression Model Plot:", align='center'),br(),
                    tabPanel("Linear Regression Model Plot",plotOutput("plot1")),br(),
                    helpText("Linear Regression Model Conclusion:", align='center'),br(),
                    tabPanel("Linear Regression Model Conclusion",textOutput("table1c")),br(),
                    ),
                  tabPanel("Data_2", value = "ds2", DT::dataTableOutput("table2")),br(),
                  tabPanel("GLM", value = "glm",br(),
                           helpText("Logistic Regression Model result#1:", align='center'),br(),
                           tabPanel("Logistic Regression Model result#1",verbatimTextOutput("table2b")),br(),
                           helpText("Logistic Regression Model result#2:", align='center'),br(),
                           tabPanel("Logistic Regression Model result#2",verbatimTextOutput("table2c")),br(),
                           helpText("Logistic Regression Model Plot:", align='center'),br(),
                           tabPanel("Logistic Regression Model Plot",plotOutput("plot2")),br(),
                           helpText("Logistic Regression Model Conclusion:", align='center'),br(),
                           tabPanel("Logistic Regression Model Conclusion",textOutput("table2d"),br(),)
                           ),
                  
      )
      
    )
  )
)

#################  Server Code ################# 

server = shinyServer(function(input,output,session){
  
  # loading a dataset which has data on gross national income from year 2001
  # fertility = birth rate per 1000 females for the population of year 2000
  data= read.csv("/Users/Bilal/Documents/Bilal Files/Yeshiva/Summer 2021/datasets/UN.txt", header=T, sep="")
  inc= log(data$PPgdp)
  fert = log(data$Fertility)
  
  lg= lm(fert~inc)
  
  #Covid-19 related data for the Philippines.
  data2 = read.csv("/Users/Bilal/Documents/Bilal Files/Yeshiva/Summer 2021/datasets/COVID19_Phili.csv", header = TRUE)
  
  y = ifelse(data2$Status == "Deceased", 1, 0)
  glm = glm(y ~ Age + Sex + Transmission, data2, family = "binomial")
  
  fit_age = glm(y ~ Age, data2, family = "binomial")
  prob = predict(fit_age, type = "response")
  x2 = data2$Age
  o = order(x2)
  
  output$table <- DT::renderDataTable({
    if(is.null(data)){return()}
    DT::datatable(data, options = list(scrollX = T))
  })
  
  output$table <- DT::renderDataTable({
    if(is.null(data2)){return()}
    DT::datatable(data2, options = list(scrollX = T))
  })
  
    submit_data1 <- eventReactive(input$btn1,{
    head(data,20)
  })
  
  # this is the output which we are showing:
  output$table1 <- DT::renderDataTable({
    DT::datatable(distinct(submit_data1()), options = list("pageLength" = 12), rownames = FALSE)
  })
  
  submit_data1b <- eventReactive(input$btn2,{
    summary(lg)
  })
  
  # this is the output which we are showing:
  output$table1b <- renderPrint({
   submit_data1b()
  })
  
  # this is the output which we are showing:
  output$table1c <- renderText({
    "The coefficient estimate is -0.22. It means that, 
    given one percent increase in the per
capita GDP, the fertility rate decreases approximately 
by -0.22 percent. The overall efect on the
fertility rate is negative when income is increased"
  })
  
  plot1 <- eventReactive(input$btn2,{
    ggplot(data, aes(x=log(PPgdp),y=log(Fertility))) + geom_point()+
      geom_smooth(method="lm",se=FALSE, color="red")
  })
    
  output$plot1 <- renderPlot({
    plot1()
  })
  
  submit_data2 <- eventReactive(input$btn3,{
    head(data2,20)  
    })
  
  # this is the output which we are showing:
  output$table2 <- DT::renderDataTable({
    DT::datatable(distinct(submit_data2()), options = list("pageLength" = 12), rownames = FALSE)
  })  
  
  submit_data2b <- eventReactive(input$btn4,{
    summary(glm)
  })
  
  # this is the output which we are showing:
  output$table2b <- renderPrint({
    submit_data2b()
  })
  
  
  submit_data2c <- eventReactive(input$btn4,{
    summary(fit_age)
  })
  
  # this is the output which we are showing:
  output$table2c <- renderPrint({
    submit_data2c()
  })
  
  # this is the output which we are showing:
  output$table2d <- renderText({
    "Give the smallest p-value for 'Age' it appears to be statistically most significant. So, it would
    make sense to generate a logistic model with only 'Age'. After generating another logistic regression 
    model we see that the estimates for the constant and the age coefficient are statistically very significant
    with  practically zero p-values.
    
    So this makes more sense, and as per the plot we see that there is a non-linear but positive
    relationship between the age of people and probability of death. So when a person grows more older,
    they are less likely to recover from Covid-19."
  })
  
  plot2 <-  eventReactive(input$btn4,{
    plot(x2[o], prob[o], ylim=c(0,1), type = 'l', xlab = "Age", ylab = "Probability of Death")
  })
  
  output$plot2 <- renderPlot({
    plot2()
  })
  # capture button click to activate appropriate tab
  observeEvent(input$btn1, {updateTabsetPanel(session, "tabset1", selected = "ds1")})
  observeEvent(input$btn2, {updateTabsetPanel(session, "tabset1", selected = "lg")})               
  observeEvent(input$btn3, {updateTabsetPanel(session, "tabset1", selected = "ds2")})               
  observeEvent(input$btn4, {updateTabsetPanel(session, "tabset1", selected = "glm")})               
  
})


# running the shiny app
shinyApp(ui, server)
