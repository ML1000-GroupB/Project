
library(shiny)
library(arules)
library(dplyr)
library(shinyWidgets)

ui <- fluidPage(
  
  titlePanel("Recommendations based on your shopping cart!"),
  
  fluidRow(
    column(3, 
           selectInput(inputId = "select1", label = "Product 1",
                      choices = list("Organic Hass Avocado" = 1, 
                                      "Organic Tomato Cluster" = 2,
                                      "Organic Lemon" = 3)) 
           ),
    column(3, 
           selectInput(inputId = "select2", label = "Product 2",
                       choices = list("Organic Hass Avocado" = 1, 
                                      "Organic Tomato Cluster" = 2,
                                      "Organic Lemon" = 3)) 
    ),
    column(3, 
           selectInput(inputId = "select3", label = "Product 3",
                       choices = list("Organic Hass Avocado" = 1, 
                                      "Organic Tomato Cluster" = 2,
                                      "Organic Lemon" = 3)) 
    ),
    column(3, 
           selectInput(inputId = "select4", label = "Product 4",
                       choices = list("Organic Hass Avocado" = 1, 
                                      "Organic Tomato Cluster" = 2,
                                      "Organic Lemon" = 3)) 
    )

    ),
  
    br(),
  
    mainPanel(
textOutput("text1")
)
    
)

server <- function(input, output) {
  
  options(shiny.maxRequestSize = 30*1024^2)
  
  
#  input_data_upload = reactive({
    
#    inFile <- input$file1
    
#    if (is.null(inFile)) {
#      input_data_upload <- read.csv("https://raw.githubusercontent.com/ML1000-GroupB/Project/main/orders_TRAIN_products_MERGED_subset for association rule.csv",
#                                    stringsAsFactors = T,header = T)
#      input_data_upload=as.data.frame(input_data_upload)
      
#      input_data_upload$order_id=as.factor(input_data_upload$order_id)
#      input_data_upload$product_id=as.factor(input_data_upload$product_id)
      
#      input_data_upload[,1] = as.factor(input_data_upload[,1])
#      input_data_upload[,2] = as.factor(input_data_upload[,2])
#      input_data_upload
#    }
#  })
  
  X_apri_rule=readRDS("C:/Users/yunan/Downloads/York U/Machine Learning Cert/Assignment 3/data/trained_rules.rds")
  
  rhs1 = reactive ({
    
#    if (is.null(input_data_upload())) {return()} else {
    
#    Order_by_product <- split(input_data_upload()$product_name, input_data_upload()$order_id)
#  X1_trans <- as(Order_by_product, "transactions")
#  X_apri_rule=apriori(X1_trans,parameter=list(supp=0.0015, conf=0.4), control=list(verbose = FALSE))
 
   dynamicrule=subset(X_apri_rule, lhs %in% "Organic Hass Avocado")
  rhs1=unique(dynamicrule@rhs@itemInfo$labels[dynamicrule@rhs@data@i+1])

#    }
    
  })
  
  
 output$text1<-renderPrint(print(rhs1()))

 
}



# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server )

