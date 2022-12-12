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
  #  t <- Sys.time()
    CMI_Df <- CMI_fun(values$PDF_CMI)
    ABC_Df <- ABC_fun(values$PDF_ABC)
    #  print(Sys.time()-t)
    values$Correspondance_Df <- Compare_fun(values$CMI_ABC_Df,CMI_Df,ABC_Df)[1] %>% as.data.frame()
    values$non_Correspondance_Df <- Compare_fun(values$CMI_ABC_Df,CMI_Df,ABC_Df)[2] %>% as.data.frame()
    values$DF_Best_Price <- Compare_fun(values$CMI_ABC_Df,CMI_Df,ABC_Df)[3] %>% as.data.frame()
    #  print(Sys.time()-t)
  })
  
  observeEvent(input$PDF_file1, {
    values$PDF_CMI <-pdf_text(input$PDF_file1$datapath)%>% 
      str_split("\n")
    
  })
  
  observeEvent(input$PDF_file2, {
    values$PDF_ABC <-pdf_text(input$PDF_file2$datapath)%>% 
      str_split("\n")
    
  })
  
  
  observeEvent(input$EXEL_file, {
    values$CMI_ABC_Df <-readxl::read_excel(input$EXEL_file$datapath, sheet = "Sheet1", na = "n/a")
  })
  
  
  #--------------------
  
  
  
  output$Correspondance_Df <- DT::renderDataTable({

    values$Correspondance_Df %>% 
      DT::datatable( escape=F, rownames = F, 
                     callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                     options = list(lengthChange = F,searching = F, info = FALSE,
                                    paging = T, ordering = F,digits = NULL
                                    ))
  })
  
  output$non_Correspondance_Df <- DT::renderDataTable({

    values$non_Correspondance_Df %>% 
      DT::datatable( escape=F, rownames = F, 
                     callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                     options = list(lengthChange = F,searching = F, info = FALSE,
                                    paging = T, ordering = F,digits = NULL
                     ))
  })
  
  output$DF_Best_Price <- DT::renderDataTable({
    req(input$EXEL_file)
    req(input$PDF_file1)
    req(input$PDF_file2)
    values$DF_Best_Price %>% 
      DT::datatable( escape=F, rownames = F, 
                     callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                     options = list(lengthChange = F,searching = F, info = FALSE,
                                    paging = T, ordering = F,digits = NULL
                     ))
  })
  
  
  output$Best_Price_filter <- DT::renderDataTable({
    req(input$Filter)
    req(input$EXEL_file)
    req(input$PDF_file1)
    req(input$PDF_file2)
    df <- values$DF_Best_Price
    
    df<-df %>% filter(df[,4] == input$Filter) %>% 
      DT::datatable( escape=F, rownames = F, 
                     callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                     options = list(lengthChange = F,searching = F, info = FALSE,
                                    paging = T, ordering = F,digits = NULL
                     ))
  })
  
  
  # output$download_Data_Comparable_DF <- downloadHandler(
  #   filename = function() {"Comparable_DF.xlsx"},
  #   content = function(file) {
  #     write.xlsx(Comparable_DF, file)
  #   }
  # )
  
  output$Hc_BarPlot <- renderHighchart({
    req(input$EXEL_file)
    req(input$PDF_file1)
    req(input$PDF_file2)
    highchart() %>% 
      hc_chart(type = "column") %>%
      hc_xAxis(categories =values$DF_Best_Price[,1],title = list(text= '<b> Items Name <b>'), crosshair= T) %>%
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
}

