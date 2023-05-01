
# Load Packages
library(shiny)
library(shinydashboard)
library(torch)

#ui <- fluidPage() # defines user interface
#server <- function(input, output){} # Funktion mit R-Anweisungen und -Objekten

# Funktion, die ui und server zu einer Shiny Anwendung verknüpft
#shinyApp(ui = ui, server=server)

# Load Model & Labe list
# model <- "www/"
options(scipen=999)

# Define user interface
ui <- dashboardPage(
  skin="blue",
  
  #(1) Header
  
  dashboardHeader(title=tags$h1("Plastic Detection in Rivers",style="font-size: 120%; font-weight: bold; color: white"),
                  titleWidth = 350,
                  tags$li(class = "dropdown"),
                  dropdownMenu(type = "notifications", icon = icon("question-circle", "fa-1x"), badgeStatus = NULL,
                               headerText="",
                               tags$li(a(href = "https://forloopsandpiepkicks.wordpress.com",
                                         target = "_blank",
                                         tagAppendAttributes(icon("question-circle"), class = "info"),
                                         "Created by"))
                  )),
  
  
  #(2) Sidebar
  
  dashboardSidebar(
    width=350,
    fileInput("input_image","File" ,accept = c('.jpg','.jpeg')), 
    tags$br(),
    tags$p("Upload the image here.")
  ),
  
  
  #(3) Body
  
  dashboardBody(
    
    h4("Instruction:"),
    tags$br(),tags$p("1. Take a picture of a bird."),
    tags$p("2. Crop image so that bird fills out most of the image."),
    tags$p("3. Upload image with menu on the left."),
    tags$br(),
    
    fluidRow(
      column(h4("Image:"),imageOutput("output_image"), width=6),
      column(h4("Result:"),tags$br(),textOutput("warntext",), tags$br(),
             tags$p("This bird is probably a:"),tableOutput("text"),width=6)
    ),tags$br()
    
  ))


# Function with R objects and functions
server <- function(input, output) {
  
  image <- reactive({image_load(input$input_image$datapath, target_size = target_size[1:2])})
  
  
  prediction <- reactive({
    if(is.null(input$input_image)){return(NULL)}
    x <- image_to_array(image())
    x <- array_reshape(x, c(1, dim(x)))
    x <- x/255
    pred <- model %>% predict(x)
    pred <- data.frame("Bird" = label_list, "Prediction" = t(pred))
    pred <- pred[order(pred$Prediction, decreasing=T),][1:5,]
    pred$Prediction <- sprintf("%.2f %%", 100*pred$Prediction)
    pred
  })
  
  output$text <- renderTable({
    prediction()
  })
  
  output$warntext <- renderText({
    req(input$input_image)
    
    if(as.numeric(substr(prediction()[1,2],1,4)) >= 30){return(NULL)}
    warntext <- "Warning: I am not sure about this bird!"
    warntext
  })
  
  
  output$output_image <- renderImage({
    req(input$input_image)
    
    outfile <- input$input_image$datapath
    contentType <- input$input_image$type
    list(src = outfile,
         contentType=contentType,
         width = 400)
  }, deleteFile = TRUE)
  
}

shinyApp(ui, server)



