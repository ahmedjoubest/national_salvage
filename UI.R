ui <- fluidPage(
  tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: #F5F5F5;  color:black}
    .tabbable > .nav                 {background-color: #F5F5F5;  color:black}
    .tabbable > .nav                 {background-color: #F5F5F5;  color:black}
    .tabbable > .nav > li[class=active]    > a {background-color: white; color:black}
  ")),
  
  titlePanel("National Salvage Dashboard", windowTitle = "Price comparison"),
  sidebarPanel(
    fileInput("PDF_file1", "Import CMI PDF File", accept = ".pdf"),
    br(),
    fileInput("PDF_file2", "Import ABC PDF File", accept = ".pdf"),
    br(),
    checkboxInput("UpdateGS",
                  "Update 'CMI Platinum Pricing' Sheet", value = FALSE),
    br(),
    checkboxInput("Update_HD",
                  "Update Price Data of CMI & ABC", value = FALSE),
    br(),br(),
    fluidRow(
      column(
        12,
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
          getDependency('sparkline'),
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
        "CMI & ABC Price Variation",
        conditionalPanel(
          condition = "input.Run>=1",
        h3("CMI & ABC Price Variation:"),
        br(),
        tags$p("The following plot shows the price historical data variation over time for each purchaser:"),
        br(),
        fluidRow(
          column(width=6,
                 pickerInput(
                   inputId = "CMI_referance",label = "CMI Item name:",
                   choices = CMI_Refrence_name[,2],
                   selected = CMI_Refrence_name[,2][1:2],
                   multiple = T,
                   options = pickerOptions(actionsBox = T, liveSearch = T, size = 5,
                                           dropdownAlignRight = T)
                 ), align = "center"
                 , style = "margin-bottom: 10px;"
                 , style = "margin-top: -10px;"),
          column(width=6,
                 pickerInput(
                   inputId = "ABC_referance",label = "ABC Item name:",
                   choices = ABC_Refrence_name[,2],
                   selected = ABC_Refrence_name[,2][1:2],
                   multiple = T,
                   options = pickerOptions(actionsBox = T, liveSearch = T, size = 5,
                                           dropdownAlignRight = T)
                 ),align = "center"
                 , style = "margin-bottom: 10px;"
                 , style = "margin-top: -10px;")

        ),
        highchartOutput("Hc_Price_variation",height="388px") %>% 
          withSpinner(color="#3C8DBC",type=4, size = 0.5),
        br(),
        fluidRow(
          column(width=6,downloadButton('download_CMI_Data', 'Download CMI Historical Data', style = "color: #fff; background-color: #337ab7;
                 border-color: #2e6da4; margin: auto") 
                 , align = "center"
                 , style = "margin-bottom: 10px;"
                 , style = "margin-top: -10px;"),
          column(width=6,downloadButton('download_ABC_Data', 'Download ABC Historical Data', style = "color: #fff; background-color: #337ab7;
                 border-color: #2e6da4; margin: auto")
                 , align = "center"
                 , style = "margin-bottom: 10px;"
                 , style = "margin-top: -10px;")
          )
      )),
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

