
library(shiny)
library(DT)
library(collapse)
library(shinyBS)
ui <- navbarPage("Smart Cart",
                 tabPanel("Home",
                          
                          fluidRow(
                            column(12,
                                   div(img(src='https://raw.githubusercontent.com/ML1000-GroupB/Project/main/www/g2.png?raw=true',height = 300, width = 670),style="text-align: center;")
                            )),
                          tabsetPanel(
                            type="tabs",
                            tabPanel("Top Selling",
                                     # mainPanel(DTOutput('table'))),
                                     
                                     mainPanel(
                                       # actionButton("p1Button", "Produce"),
                                       # actionButton("p2Button","Frozen"),
                                       # actionButton("p3Button","Personal Care"),
                                       bsCollapse(id = "collapseExample", open = "Panel 2",
                                                  bsCollapsePanel("Produce", selectInput(inputId = "select1",
                                                                                         label = "your first produce product",
                                                                                         choices = readRDS(url("https://github.com/ML1000-GroupB/Project/blob/main/table1.rds?raw=true"))),
                                                                  actionLink(inputId = "cart1", label = "Add to cart", 
                                                                             icon = icon("shopping-cart")),
                                                                  
                                                                  selectInput(inputId = "select2",
                                                                              label = "your second produce product",
                                                                              choices = readRDS(url("https://github.com/ML1000-GroupB/Project/blob/main/table1.rds?raw=true"))),
                                                                  actionLink(inputId = "cart1", label = "Add to cart", 
                                                                             icon = icon("shopping-cart")),
                                                                  
                                                                  selectInput(inputId = "select3",
                                                                              label = "your third produce product",
                                                                              choices = readRDS(url("https://github.com/ML1000-GroupB/Project/blob/main/table1.rds?raw=true"))),
                                                                  actionLink(inputId = "cart1", label = "Add to cart", 
                                                                             icon = icon("shopping-cart")),
                                                                  
                                                                   DTOutput('table1'),style = "info"),
                                                  
                                                  bsCollapsePanel("Dairy Eggs", 
                                                                  selectInput(inputId = "select4",
                                                                                            label = "your dairy or egg product",
                                                                                            choices = readRDS(url("https://github.com/ML1000-GroupB/Project/blob/main/table2.rds?raw=true"))),
                                                                  
                                                                  actionLink(inputId = "cart1", label = "Add to cart", 
                                                                             icon = icon("shopping-cart")),
                                                                  
                                                                  DTOutput('table2'), style = "success"),
                                                  
                                                  bsCollapsePanel("Beverages", selectInput(inputId = "select5",
                                                                                           label = "your beverage product",
                                                                                           choices = "Sparkling Water Grapefruit"),
                                                                  
                                                                  actionLink(inputId = "cart1", label = "Add to cart", 
                                                                             icon = icon("shopping-cart")),
                                                                  
                                                                  DTOutput('table3'), style = "info"),
                                                  
                                                  bsCollapsePanel("Deli", selectInput(inputId = "select6",
                                                                                      label = "your deli product",
                                                                                      choices = "Original Hummus"),
                                                                  
                                                                  actionLink(inputId = "cart1", label = "Add to cart", 
                                                                             icon = icon("shopping-cart")),
                                                                  
                                                                  DTOutput('table4'), style = "success")
                                       ))),
                            tabPanel("Orders"),
                            tabPanel("Confirmation")
                            
                          )
                 ),
                 tabPanel("About Us",
                          h3("Why to associate with Smart cart?"),
                          br(),
                          h5("Smart Cart is a tool that will help with your online grocery orders.","Smart cart recommends you with products based on purchase history.","Never come home from shopping just to realize that you forgot to buy pancake mix, olive oil and first aid. ")),
                 tabPanel("Contact Us",
                          a(actionButton(inputId = "email1", label = "Send Email", 
                                         icon = icon("envelope", lib = "font-awesome")),
                            href="mailto:admin@smartcart.com"))
)


server <- function(input, output) {
  
  options(shiny.maxRequestSize = 30*1024^2)
  
  X_apri_rule=readRDS(url("https://github.com/ML1000-GroupB/Project/blob/main/trained_rules.rds?raw=true"))
  
  
  rhs1 = reactive ({
    
    dynamicrule=subset(X_apri_rule, lhs %in% "Organic Hass Avocado")
    rhs1=unique(dynamicrule@rhs@itemInfo$labels[dynamicrule@rhs@data@i+1])

  })
  
  
  output$text1<-renderPrint(print(rhs1()))
  output$table1 <- renderDT(readRDS(url("https://github.com/ML1000-GroupB/Project/blob/main/table1.rds?raw=true")))
  output$table2 <- renderDT(readRDS(url("https://github.com/ML1000-GroupB/Project/blob/main/table2.rds?raw=true")))
  output$table3 <- renderDT(readRDS(url("https://github.com/ML1000-GroupB/Project/blob/main/table3.rds?raw=true")))
  output$table4 <- renderDT(readRDS(url("https://github.com/ML1000-GroupB/Project/blob/main/table4.rds?raw=true")))
}



# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server )


