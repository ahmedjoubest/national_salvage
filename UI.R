ui <- fluidPage(
  tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: #F5F5F5;  color:black}
    .tabbable > .nav                 {background-color: #F5F5F5;  color:black}
    .tabbable > .nav > li[class=active]    > a {background-color: white; color:black}
  ")),
  
  
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
        actionButton("Run", "Run App", style = "color: #fff; background-color: #337ab7;
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
      tabPanel(" titre",
               conditionalPanel(condition = "input.Run==true",
                                
                                h4("Results"), br(),
                                fluidRow(width=12,DT::dataTableOutput('DF_Best_Price') %>% 
                                           withSpinner(color="#3C8DBC",type=4, proxy.height = "127px",size = 0.5)),  
                                br(),
                                br(),
                                
                                fluidRow(
                                  pickerInput(
                                    inputId = "Filter",label = "Choose a Purchase Name",
                                    choices = c("CMI", "ABC"),
                                    selected = c("CMI"),
                                    options = pickerOptions(actionsBox = T, liveSearch = T, size = 10,
                                                            dropdownAlignRight = T)
                                  )), 
                                br(),
                                br(),
                                fluidRow(width=12,DT::dataTableOutput('Best_Price_filter')%>% 
                                           withSpinner(color="#3C8DBC",type=4, proxy.height = "127px",size = 0.5)),
                                
                                highchartOutput("Hc_BarPlot") %>% 
                                  withSpinner(color="#3C8DBC",type=4, proxy.height = "127px",size = 0.5)
               )
      ),
      tabPanel(" titre",
               conditionalPanel(condition = "input.Run==true",
                                h4("Correspondance Data Frame"), br(),
                                fluidRow(width=12,DT::dataTableOutput('Correspondance_Df')%>% 
                                           withSpinner(color="#3C8DBC",type=4, proxy.height = "127px",size = 0.5))
               ), 
               conditionalPanel((condition = "input.Run==true" ),
                                
                                h4("Non Correspondance Data Frame"), br(),
                                fluidRow(width=12,DT::dataTableOutput('non_Correspondance_Df') %>% 
                                           withSpinner(color="#3C8DBC",type=4, proxy.height = "127px",size = 0.5)),  

               )
               )
    ),
    
    width = 9)
  
)

