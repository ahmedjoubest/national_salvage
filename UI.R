ui <- fluidPage(
  tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: #F5F5F5;  color:black}
    .tabbable > .nav                 {background-color: #F5F5F5;  color:black}
    .tabbable > .nav > li[class=active]    > a {background-color: white; color:black}
  ")),
  
  titlePanel("Price comparison", windowTitle = "Price comparison"),
  sidebarPanel(
    fileInput("PDF_file1", "Import CMI PDF File", accept = ".pdf"),
    br(),
    fileInput("PDF_file2", "Import ABC PDF File", accept = ".pdf"),
    br(),
    # tableOutput("files"),
    br(),
    fileInput("EXEL_file", "Import Exel File", accept = ".xlsx"),
    br(),
    # tableOutput("files"),
    br(),
    fluidRow(
      column(
        12,
        # M 2-1 C
        actionButton(
          "Run", "Run Comparison", style = "color: #fff; background-color: #337ab7;
                 border-color: #2e6da4; margin: auto")
        , align = "center"
        , style = "margin-bottom: 10px;"
        , style = "margin-top: -10px;"
      )
    ),
    
    width = 3),
  
  mainPanel(
    
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Price comparison",
        conditionalPanel(
          condition = "input.Run>=1",
          
          h3("Comparison price between CMI and ABC:"),
          br(),
          tags$p("The following bar plot allows you to compare the bids per purchaser and per items:"),
          br(),
          
          highchartOutput("Hc_BarPlot",height="388px") %>% 
            withSpinner(color="#3C8DBC",type=4, size = 0.5),
          
          h3("Price table"),
          br(),
          tags$p("The following data table shows the purchaser prices used in the plot above:"),
          br(),
          DT::dataTableOutput('DF_Best_Price') %>% 
            withSpinner(color="#3C8DBC",type=4,size = 0.5),
          br(),
          h3("Best purchasers:"),
          br(),
          tags$p("Based on the previous data, this list shows what item should be sold to which purchaser"),
          br(),
          uiOutput("purchase_list") %>% 
            withSpinner(color="#3C8DBC",type=4,size = 0.5),  
          br()
          
        )
      ),
      tabPanel(
        "More details",
        conditionalPanel(
          condition = "input.Run>=1",
          h3("Corresponding items:"), br(),
          tags$p("This data table shows the items that matches between ABC and CMI"),
          width=12,DT::dataTableOutput('Correspondance_Df')%>% 
            withSpinner(color="#3C8DBC",type=4, size = 0.5),br(),
          h3("Non Corresponding Data Frame"), br(),
          tags$p("You can find below the items that don't match between the two uploaded PDF's"),
          width=12,DT::dataTableOutput('non_Correspondance_Df') %>% 
            withSpinner(color="#3C8DBC",type=4, size = 0.5),
          br(),br()
        )
      )
    ),
    
    width = 9)
  
)

