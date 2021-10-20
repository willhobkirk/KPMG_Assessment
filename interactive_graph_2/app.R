#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(htmlTable)
library(DT)

df_raw = readr::read_tsv("housing-prices-ge19.txt") %>% janitor::clean_names()
df = filter(df_raw,test==0)


df_log = df %>%
  mutate(log_price = log(price))
df_log = subset(df_log, select = -c(price))

lm3 = lm(log_price ~  living_area + bathrooms + land_value, df_log)
lm_aic = lm(log_price ~ living_area + land_value + bathrooms + fuel_type + waterfront + age + new_construct + 
  lot_size + central_air + rooms + pct_college, df_log)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("How much is my house worth?"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("model_choice","Select a Model: ",choiceNames = c("Three Variable","AIC Selected"),choiceValues = c("three","aic")),
            checkboxInput("prediction","Prediction Interval"),
            checkboxInput("confidence","Confidence Interval"),
            numericInput("level","Prediction/Confidence Level",min=0.1,max=0.99,value = 0.9),
            numericInput("living_area","Living Area (square feet)",value =1753,min=500,max=10000),
            #numericInput("bedrooms","Number of Bedrooms",value=3,min=1,max=10),
            numericInput("bathrooms","Number of Bathrooms",value =2,min=0,max=10,step=0.5),
            numericInput("land_value","Value of Land ($USD)",value =34536,min=100,max=1000000),
            conditionalPanel(condition = "input.model_choice == 'aic'",
                             
                             numericInput("rooms","Number of Rooms",value=7,min=1,max=15),
                             selectInput("fuel_type","Fuel Type: ",choices=c("Electric","Gas","Oil","Wood","None","Solar","Unknown/Other")),
                             checkboxInput("waterfront","Is it Waterfront"),
                             numericInput("age","Age of House (years)", value = 28,min=0,max=300),
                             checkboxInput("new_construct","New Construct"),
                             numericInput("lot_size","Lot Size (acres)",value=0.5,min=0,max=15),
                             checkboxInput("central_air","Central Air Conditioning"),
                             numericInput("pct_college","Graduated College in Neighbourhood (%)",min = 0,max=100,value=55)
                             
                             )
    
        ),

        # Show a plot of the generated distribution
        mainPanel(
          dataTableOutput("plotout"),
          plotOutput("plotout2")
          
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plotout <- DT::renderDataTable ({
        # generate bins based on input$bins from ui.R
        new_df = data.frame(living_area=input$living_area, 
                            land_value= input$land_value, 
                            bathrooms=input$bathrooms,
                            fuel_type=input$fuel_type, 
                            waterfront=1*input$waterfront,
                            age=input$age,
                            new_construct=1*input$new_construct,
                            lot_size = input$lot_size,
                            central_air = 1*input$central_air,
                            rooms= input$rooms,
                            pct_college = input$pct_college)
        if(input$model_choice == "three"){
          model = lm3
        }
        else{
          model = lm_aic
        }
        
        pred = predict(model,new_df,interval="prediction",level=input$level)
        conf = predict(model,new_df,interval="confidence",level=input$level)
        lwrP = exp(pred[,2])
        lwrC = exp(conf[,2])
        uprP = exp(pred[,3])
        uprC = exp(conf[,3])
        fit = exp(pred[,1])
       # paste("Lower: ",round(lwr),"\nUpper: ",round(upr),"\nFit: ",round(fit))
        
        result = data.frame(Prediction = paste("$",as.integer(round(fit))))
        if(input$prediction){
          result = result %>% mutate("Lower Prediction Bound" = paste("$",as.integer(round(lwrP))),"Upper Prediction Bound"=paste("$",as.integer(round(uprP))))
        }
        if(input$confidence){
          result = result %>% mutate("Lower Confidence Bound" = paste("$",as.integer(round(lwrC))),"Upper Confidence Bound"=paste("$",as.integer(round(uprC))))
        }
        result
        
    })
    output$plotout2 <- renderPlot({
      
      new_df = data.frame(living_area=input$living_area, 
                          land_value= input$land_value, 
                          bathrooms=input$bathrooms,
                          fuel_type=input$fuel_type, 
                          waterfront=1*input$waterfront,
                          age=input$age,
                          new_construct=1*input$new_construct,
                          lot_size = input$lot_size,
                          central_air = 1*input$central_air,
                          rooms= input$rooms,
                          pct_college = input$pct_college)
      if(input$model_choice == "three"){
        model = lm3
      }
      else{
        model = lm_aic
      }
      
      pred = predict(model,new_df,interval="prediction",level=input$level)
      conf = predict(model,new_df,interval="confidence",level=input$level)
      lwrP = exp(pred[,2])
      lwrC = exp(conf[,2])
      uprP = exp(pred[,3])
      uprC = exp(conf[,3])
      fit = exp(pred[,1])
      
      df %>% ggplot(aes(x=price))+
        geom_histogram(fill = "burlywood", alpha = 0.5)+
        geom_vline(xintercept = lwrP,color = "red")+
        geom_vline(xintercept = lwrC,color = "blue")+
        geom_vline(xintercept = uprC,color = "blue")+
        geom_vline(xintercept = uprP,color = "red")+
        geom_vline(xintercept = fit,color = "magenta")+
        xlim(0,1000000)+
        xlab("Price ($USD)")+
        ylab("Frequency in Training Set")+
        labs(title ="Predictions and Intervals Against Training Data Distribution")
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
