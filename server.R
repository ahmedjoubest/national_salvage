server <- function(input, output, session) {
  
  values <- reactiveValues(
    PDF_CMI = NULL,
    PDF_ABC = NULL,
    CMI_ABC_Df = NULL,
    Correspondance_Df = NULL,
    non_Correspondance_Df = NULL,
    DF_Best_Price = NULL
  )
  
  observeEvent(input$Run,{
    values$CMI_ABC_Df <-read_sheet("https://docs.google.com/spreadsheets/d/1sqZxzGNsn7Zoj0a_1Kam9fZy0aquleIqEjD067qZG1M/edit#gid=527635793", sheet = "Sheet1")
    
    values$PDF_CMI <-pdf_text(input$PDF_file1$datapath)%>% 
      str_split("\n")
    values$PDF_ABC <-pdf_text(input$PDF_file2$datapath)%>% 
      str_split("\n")
    #  t <- Sys.time()
    CMI_Df <- CMI_fun(values$PDF_CMI)
    ABC_Df <- ABC_fun(values$PDF_ABC)
    #  print(Sys.time()-t)
    list <- Compare_fun(values$CMI_ABC_Df,CMI_Df,ABC_Df)
    values$Correspondance_Df <- list[[1]]
    values$non_Correspondance_Df <- list[[2]]
    values$DF_Best_Price <- list[[3]]
    #  print(Sys.time()-t)
  })
  
  #--------------------
  
  output$purchase_list <- renderUI({
    req(input$PDF_file1)
    req(input$PDF_file2)
    
    CMI <- values$DF_Best_Price[values$DF_Best_Price$`Best Purchaser`=="CMI",]
    
    CMI <- sapply(
      1:nrow(CMI),
      function(i){
        paste0("<li>", CMI$`Item Name`[i],": $",CMI$`CMI Price`[i], "</li>")
      }) %>% paste0(collapse = "")
    ABC <- values$DF_Best_Price[values$DF_Best_Price$`Best Purchaser`=="ABC",]
    ABC <- sapply(
      1:nrow(ABC),
      function(i){
        paste0("<li>", ABC$`Item Name`[i],": $",ABC$`ABC Price`[i], "</li>")
      }) %>% paste0(collapse = "")
    tagList(
      fluidRow(
        column(6,
               HTML(
                 paste0(
                   "
    <p> <b> CMI best price items: </b></p>
    <ul>
        ",CMI,"</ul>"
                 )
               )),
        column(6,
               HTML(
                 paste0(
                   "
    <p> <b> ABC best price items: </b> </p>
    <ul>
        ",ABC,"</ul>"
                 )
               ))
      )
    )
  })
  
  output$DF_Best_Price <- DT::renderDataTable({
    req(input$PDF_file1)
    req(input$PDF_file2)
    values$DF_Best_Price %>% 
      DT::datatable( escape=F, rownames = F, 
                     callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                     options = list(lengthChange = F, paging = T))
  })
  
  
  output$Hc_BarPlot <- renderHighchart({
    req(input$PDF_file1)
    req(input$PDF_file2)
    highchart() %>% 
      hc_chart(type = "column") %>%
      hc_xAxis(categories =values$DF_Best_Price[,1],title = list(text= '<b> Item Name <b>'), crosshair= T) %>%
      hc_yAxis(min= 0, title=list(text= "<b> Price ($) <b>")) %>%
      hc_add_series(name="CMI Price",
                    data = values$DF_Best_Price[,2]) %>%
      hc_add_series(name="ABC Price",
                    data = values$DF_Best_Price[,3])%>%
      hc_colors(c("#e9724d", "#0000FF"))  %>% 
      hc_tooltip(shared = TRUE,
                 crosshairs = TRUE,
                 followPointer = T,
                 borderColor = "grey")
  })
  
  output$Correspondance_Df <- DT::renderDataTable({
    values$Correspondance_Df %>% 
      DT::datatable( escape=F, rownames = F, 
                     callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                     options = list(lengthChange = F, paging = T))
  })
  
  output$non_Correspondance_Df <- DT::renderDataTable({
    values$non_Correspondance_Df %>% 
      DT::datatable( escape=F, rownames = F, 
                     callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                     options = list(lengthChange = F, paging = T))
  })
}

