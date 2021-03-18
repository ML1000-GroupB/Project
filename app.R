
library(shiny)
library(DT)
library(collapse)
library(shinyBS)
library(stringi)
library(rlang)
library(shinythemes)
library(Matrix)
library(dplyr)
library(rsparse)
library(arules)

table1<-readRDS("table1.rds")
table2<-readRDS("table2.rds")
products<-readRDS("products.rds")
mcname<-readRDS("mcname.rds")
model<-readRDS("model.rds")


shinyApp(ui <- navbarPage(
                 "Smart Cart",
                 tabPanel("HOME",icon=icon("home"),
                          
                          fluidRow(
                            column(12,
                                   img(src='./g2.png')
                            )),
                          tabsetPanel(
                            id = "tabs",
                            type="tabs",
                            tabPanel("TOP SELLING",
                                     mainPanel(
                                       bsCollapse(id = "collapseExample", open = "Panel 2",
                                                  bsCollapsePanel("Produce", selectInput(inputId = "select1",
                                                                                         label = "Item1:",
                                                                                         choices = table1[,1]),
                                                                  actionLink(inputId = "cart1", label = "Add to cart", 
                                                                             icon = icon("shopping-cart")),
                                                                  
                                                                  selectInput(inputId = "select2",
                                                                              label = "Item2:",
                                                                              choices =table1[,1],selected=table1[2,1] ),
                                                                  actionLink(inputId = "cart2", label = "Add to cart", 
                                                                             icon = icon("shopping-cart")),
                                                                  
                                                                  selectInput(inputId = "select3",
                                                                              label = "Item3:",
                                                                              choices = table1[,1],selected=table1[3,1]),
                                                                  actionLink(inputId = "cart3", label = "Add to cart", 
                                                                             icon = icon("shopping-cart")),
                                                                  style = "info"),
                                                  
                                                  bsCollapsePanel("Dairy/Eggs", 
                                                                  selectInput(inputId = "select4",
                                                                                            label = "Item1:",
                                                                                            choices = table2[,1]),
                                                                  
                                                                  actionLink(inputId = "cart4", label = "Add to cart", 
                                                                             icon = icon("shopping-cart")),
                                                                  style = "success"),
                                                  
                                                  bsCollapsePanel("Beverages", selectInput(inputId = "select5",
                                                                                           label = "Item1:",
                                                                                           choices = "Sparkling Water Grapefruit"),
                                                                  
                                                                  actionLink(inputId = "cart5", label = "Add to cart", 
                                                                             icon = icon("shopping-cart")),
                                                                  style = "info"),
                                                  
                                                  bsCollapsePanel("Deli", selectInput(inputId = "select6",
                                                                                      label = "Item1:",
                                                                                      choices = "Original Hummus"),
                                                                  
                                                                  actionLink(inputId = "cart6", label = "Add to cart", 
                                                                             icon = icon("shopping-cart")),
                                                                  
                                                                  style = "success")
                                       ),
                                       actionButton("buynow", "ADD TO BAG",class = "btn-primary")
                                       )),
                            tabPanel("SHOPPING BAG",
                                     h3("You have added below items "),
                                     br(),
                                     br(),
                                     dataTableOutput("tableorder"),
                                     br(),
                                     h3("You May Also Like"),
                                     verbatimTextOutput("text1"),
                                     verbatimTextOutput("text2"),
                                     verbatimTextOutput("text3"),
                                     verbatimTextOutput("text4"),
                                     verbatimTextOutput("text5"),
                                     verbatimTextOutput("text6"),
                                     verbatimTextOutput("text7"),
                                     actionButton("paynow", "CHECK OUT",class = "btn-primary")
                                     ),
                            tabPanel("ORDERS",
                                     h3("You have checked out below items"),
                                     br(),
                                     br(),
                                     dataTableOutput("tableconfirm"),
                                     br(),
                                     h3("Shoppers Also Viewed "),
                                     br(),
                                     br(),
                                     dataTableOutput("tablepred"),
                                     actionButton("confirmnow", "PLACE ORDER",class = "btn-primary"),
                                     br(),
                                     br(),
                                     bsModal(id="pop",title="Thank You !!",trigger="confirmnow",size="medium",
                                             verbatimTextOutput("thank")))
                            
                          )
                 ),
                 tabPanel("ABOUT US",icon=icon("tags"),
                          h3("Why to associate with Smart cart?"),
                          br(),
                          h5("Smart Cart is a tool that will help with your online grocery orders.","Smart cart recommends you with products based on purchase history.","Never come home from shopping just to realize that you forgot to buy pancake mix, olive oil and first aid. ")),
                 tabPanel("CONTACT US",icon=icon("envelope"),
                          a(actionButton(inputId = "email1", label = "Send Email", 
                                         icon = icon("envelope", lib = "font-awesome")),
                            href="mailto:admin@smartcart.com"))
),

server <- function(input, output,session) {
  
 # options(shiny.maxRequestSize = 30*1024^2)
  
 
  
  X_apri_rule=readRDS(url("https://github.com/ML1000-GroupB/Project/blob/main/trained_rules.rds?raw=true"))
  

    rhs1 = reactive ({
      
        dynamicrule1=subset(X_apri_rule, lhs %in% input$select1)
        rhs1=unique(dynamicrule1@rhs@itemInfo$labels[dynamicrule1@rhs@data@i+1])

    })
  

  rhs2 = reactive ({
    
    dynamicrule2=subset(X_apri_rule, lhs %in% input$select2)
    rhs2=unique(dynamicrule2@rhs@itemInfo$labels[dynamicrule2@rhs@data@i+1])
    
  })
  
  
  rhs3 = reactive ({
    
    dynamicrule3=subset(X_apri_rule, lhs %in% input$select3)
    rhs3=unique(dynamicrule3@rhs@itemInfo$labels[dynamicrule3@rhs@data@i+1])
    
  })
  
  rhs4 = reactive ({
    
    dynamicrule4=subset(X_apri_rule, lhs %in% input$select4)
    rhs4=unique(dynamicrule4@rhs@itemInfo$labels[dynamicrule4@rhs@data@i+1])
    
  })
  
  
  rhs5 = reactive ({
    
    dynamicrule5=subset(X_apri_rule, lhs %in% input$select5)
    rhs5=unique(dynamicrule5@rhs@itemInfo$labels[dynamicrule5@rhs@data@i+1])
    
  })  
  
  rhs6 = reactive ({
    
    dynamicrule6=subset(X_apri_rule, lhs %in% input$select6)
    rhs6=unique(dynamicrule6@rhs@itemInfo$labels[dynamicrule6@rhs@data@i+1])
    
  })   
  
  
  text1 <- eventReactive(input$cart1, {
    if (is_empty(rhs1())==FALSE)
    print(rhs1()) 
  })
  

 text2 <-eventReactive(input$cart2, {
   if (is_empty(rhs2())==FALSE)
   print(rhs2())
 })
 
 
 text3 <-eventReactive(input$cart3, {
   if (is_empty(rhs3())==FALSE)
   print(rhs3())
 })
 
 text4 <-eventReactive(input$cart4, {
   if (is_empty(rhs4())==FALSE)
   print(rhs4())
 })
 
 text5 <-eventReactive(input$cart5, {
   if (is_empty(rhs5())==FALSE)
   print(rhs5())
 })
 
 text6 <-eventReactive(input$cart6, {
  if (is_empty(rhs6())==FALSE) {
    print(rhs6())
  }
 })
 
 
  output$text1 <-renderPrint(text1())
  output$text2 <-renderPrint(text2())
  output$text3 <-renderPrint(text3())
  output$text4 <-renderPrint(text4())
  output$text5 <-renderPrint(text5())
  output$text6 <-renderPrint(text6())


text7 <-reactive({
    
  cat("These items are frequently bought with the products in your cart: ",
      paste0(stri_unique(c(text1(),text2(),text3(),text4(),text5(),text6())),sep=",")
      
  )
}
)
output$text7 <-renderPrint(text7())

output$table1 <- renderDT(readRDS(url("https://github.com/ML1000-GroupB/Project/blob/main/table1.rds?raw=true")))
output$table2 <- renderDT(readRDS(url("https://github.com/ML1000-GroupB/Project/blob/main/table2.rds?raw=true")))
output$table3 <- renderDT(readRDS(url("https://github.com/ML1000-GroupB/Project/blob/main/table3.rds?raw=true")))
output$table4 <- renderDT(readRDS(url("https://github.com/ML1000-GroupB/Project/blob/main/table4.rds?raw=true")))



observeEvent(input$paynow, {
  
  #dataframe based on user inputs
  products_select<-rbind(input$select1,input$select2,input$select3,input$select4,input$select5,input$select6)
  products_select<-as.data.frame(products_select)
  names(products_select)<-c("product_name")
  products_select$product_id<-products_select %>% 
  left_join(products,by="product_name") %>% 
  select(product_id) 
  

  users <- data.frame(user_id = c("13726"))
  prod<-data.frame(product_id =mcname)
  #ord<-data.frame(user_id = c("13726"),product_id=c("196","130","13176"))
  
  
  
  #dataframe with column numbers
  ord$RowIdx <- match(ord$user_id, users$user_id)
  ord$ColIdx <- match(ord$product_id, prod$product_id)
  
  
  
  
  
  #create sparse matrix for model input
  # matSparse3 <- sparseMatrix(
  #   i = ord$RowIdx,
  #   j = ord$ColIdx,
  #   x = 1L,
  #   dims = c(nrow(users), nrow(prod)),
  #   dimnames = list(users$user_id, prod$product_id)
  # )


  # new_user_predictions <- model$predict(matSparse3, k =3)
  # top_list <- as(new_user_predictions, "list")
  # 
  # table_recom<-products %>%
  #   filter(product_id %in% top_list) %>%
  #   select(product_id,product_name)
  # table_recom

  
 output$tablepred<-renderDataTable({
   datatable(
     table_recom %>% 
       select(product_name),
     colnames=c("Product Name"))
   
 })
 
})






  
observeEvent(input$buynow, {
  updateTabsetPanel(session = session, inputId = "tabs", selected = "SHOPPING BAG")

  
  products_select<-rbind(input$select1,input$select2,input$select3,input$select4,input$select5,input$select6)
  products_select<-as.data.frame(products_select)
  names(products_select)<-c("product_name")
  products_select$product_id<-products_select %>% 
                              left_join(products,by="product_name") %>% 
                              select(product_id)    
  
  
  output$tableorder <- renderDataTable({
    datatable(
    products_select %>% 
      select(product_name),
    colnames=c("Serial","Product Name"))
  
  })
  
  output$tableconfirm <- renderDataTable({
    datatable(
      products_select %>% 
        select(product_name),
      colnames=c("Serial","Product Name"))
    
  })
  
  
  
  print(products_select)
  
})
  
observeEvent(input$paynow, {
  updateTabsetPanel(session = session, inputId = "tabs", selected = "ORDERS")
})





thanks<-eventReactive(input$confirmnow,
                    {print("Your order is being processed. Your support is invaluable !!")}
                    )

output$thank<-renderText(thanks())



  
  
  
  





}
)


# Create the shiny app             #
####################################
#shinyApp(ui = ui, server = server )


