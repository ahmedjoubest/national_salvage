ui <- fluidPage(
  useShinyalert(), 
  # Custom loading pop up with css classes
  tags$head(tags$style(HTML_css())),
  tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: #F5F5F5;  color:black}
    .tabbable > .nav                 {background-color: #F5F5F5;  color:black}
    .tabbable > .nav                 {background-color: #F5F5F5;  color:black}
    .tabbable > .nav > li[class=active]    > a {background-color: white; color:black}
  ")),
  
  titlePanel("Price comparison", windowTitle = "Price comparison"),
  sidebarPanel(
    fileInput("PDF_file1", "Import CMI PDF File", accept = ".pdf"),
    fileInput("PDF_file2", "Import ABC PDF File", accept = ".pdf"),
    br(),
    checkboxInput("UpdateGS",
                  "Update CMI Platinum Pricing Sheet", value = FALSE), br(),
    checkboxInput("Update_HD",
                  "Update Price Variation Sheet", value = FALSE),
    br(),br(),
    br(),
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
    tabsetPanel(id = "navbar_id",type = "tabs",
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
                               inputId = "CMI_referance",
                               label = "CMI Item name:",
                               choices = c(),
                               selected = c(),
                               multiple = T,
                               options = pickerOptions(actionsBox = T, liveSearch = T, size = 5,
                                                       dropdownAlignRight = T)
                             ), align = "center"
                             , style = "margin-bottom: 10px;"
                             , style = "margin-top: -10px;"),
                      column(width=6,
                             pickerInput(
                               inputId = "ABC_referance",
                               label = "ABC Item name:",
                               choices = c(),
                               selected = c(),
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
                  )
                )
    ), width = 9)
  
)