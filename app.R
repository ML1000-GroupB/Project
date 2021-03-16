#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyApp(ui <- navbarPage("Smart Cart",
                 tabPanel("Home",
                          
                          fluidRow(
                              column(12,
                                     img(src='./g2.png')
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
                                           bsCollapsePanel("Produce", "This is a panel with just text ",
                                                           "and has the default style. You can change the style in ",
                                                           "the sidebar.", DTOutput('table1'),style = "info"),
                                           bsCollapsePanel("Frozen", "This panel has a generic plot. ",
                                                           "and a 'success' style.", DTOutput('table2'), style = "success"),
                                           bsCollapsePanel("Personal Care", "This panel has a generic plot. ",
                                                           "and a 'success' style.", DTOutput('table3'), style = "info")
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
),
                 


# Define server logic required to draw a histogram
server <- function(input, output,session) {

    products<-read.csv("products.csv")
    head(products)
    top_prod<-products[1:5,1:2]
    
   # output$tbl <- renderDataTable(top_prod, options = list(
    #     pageLength = 5,
    #     initComplete = I('function(setting, json) { alert("done"); }')
    # )
    #)
    
    
    output$table1 <- renderDT({
        data <- data.frame(ROW = top_prod$product_name,
                           TEXT = '<input id="text" type="text" class="form-control" value=""/>',
                           SINGLE_SELECT = '<select id="single_select" style="width: 100%;">
                                          <option value="" selected></option>
                                          <option value="A">A</option>
                                          <option value="B">B</option>
                                          <option value="C">C</option>
                                        </select>',stringsAsFactors = FALSE)
        
        datatable(data = data,
                  selection = "none",
                  escape = FALSE,
                  rownames = FALSE)
    })
    
    
    output$table2 <- renderDT({
        data <- data.frame(ROW = top_prod$product_name,
                           TEXT = '<input id="text" type="text" class="form-control" value=""/>',
                           SINGLE_SELECT = '<select id="single_select" style="width: 100%;">
                                          <option value="" selected></option>
                                          <option value="A">A</option>
                                          <option value="B">B</option>
                                          <option value="C">C</option>
                                        </select>',stringsAsFactors = FALSE)
        
        datatable(data = data,
                  selection = "none",
                  escape = FALSE,
                  rownames = FALSE)
    })
    output$table3 <- renderDT({
        data <- data.frame(ROW = top_prod$product_name,
                           TEXT = '<input id="text" type="text" class="form-control" value=""/>',
                           SINGLE_SELECT = '<select id="single_select" style="width: 100%;">
                                          <option value="" selected></option>
                                          <option value="A">A</option>
                                          <option value="B">B</option>
                                          <option value="C">C</option>
                                        </select>',stringsAsFactors = FALSE)
        
        datatable(data = data,
                  selection = "none",
                  escape = FALSE,
                  rownames = FALSE)
    })
    
    output$genericPlot <- renderPlot(plot(rnorm(100)))
    observeEvent(input$p1Button, ({
        updateCollapse(session, "collapseExample", open = "Produce")
    }))
    
    observeEvent(input$p2Button, ({
        updateCollapse(session, "collapseExample", open = "Frozen")
    }))
    
    observeEvent(input$p3Button, ({
        updateCollapse(session, "collapseExample", open = "Personal Care")
    }))
    
    
    
    
}
)
# Run the application 
#shinyApp(ui = ui, server = server)
