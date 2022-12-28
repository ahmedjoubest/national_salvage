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
    ## -----------------------------
    
    values$CMI_ABC_Df <-read_sheet("https://docs.google.com/spreadsheets/d/1sqZxzGNsn7Zoj0a_1Kam9fZy0aquleIqEjD067qZG1M/edit#gid=527635793",
                                   sheet = "Sheet1")
    values$PDF_CMI <-pdf_text(input$PDF_file1$datapath)%>% 
      str_split("\n")
    values$PDF_ABC <-pdf_text(input$PDF_file2$datapath)%>% 
      str_split("\n")
    
    CMI_Df <- CMI_fun(values$PDF_CMI)
    ABC_Df <- ABC_fun(values$PDF_ABC)
    
    ## -----------------------------
    
    for( i in 1:nrow(CMI_Price_variation)){
      rownames(CMI_Price_variation)[i]=i
      rownames(ABC_Price_variation)[i]=i
      
    }
    
    for( i in 1:nrow(ABC_Price_variation)){
      rownames(CMI_Price_variation)[i]=as.character(Sys.Date()-128+i)
      rownames(ABC_Price_variation)[i]=as.character(Sys.Date()-128+i)
    }
    
    ## -----------------------------
    if (input$UpdateGS){
      CMI_Pricing <-read_sheet("https://docs.google.com/spreadsheets/d/1sqZxzGNsn7Zoj0a_1Kam9fZy0aquleIqEjD067qZG1M/edit#gid=527635793",
                               sheet = "CMI Platinum Pricing")
      
      CMI_Pricing <- CMI_Pricing %>% select("Product")
      CMI_Pricing<- CMI_Pricing[!is.na(CMI_Pricing$Product),] 
      DF_to_overwrite  <- data.frame(matrix(nrow = nrow(CMI_Pricing), ncol = 2))
      colnames(DF_to_overwrite)<- c("Product","CMI")
      for (i in 2:nrow(CMI_Pricing)) {
        for (j in 1:nrow(CMI_Df)) {
          if (CMI_Pricing[i,]==CMI_Df[j,1]){
            DF_to_overwrite[i,1]=CMI_Df[j,1]
            DF_to_overwrite[i,2]=CMI_Df[j,2] %>% as.numeric()
          }
        }
      }
      DF_to_overwrite<-DF_to_overwrite %>% drop_na()
      sheet_write("https://docs.google.com/spreadsheets/d/1sqZxzGNsn7Zoj0a_1Kam9fZy0aquleIqEjD067qZG1M/edit#gid=527635793",
                  data = DF_to_overwrite, sheet = "Scrapper for R")
      
      
    }
    
    ## -----------------------------
    if(input$Update_HD){

      # ----
      if(sum(rownames(CMI_Price_variation)[1:nrow(CMI_Price_variation)]==as.character(Sys.Date()))!=1){
        DF_CMI_stock <- data.frame(matrix(nrow = nrow(CMI_Df), ncol = 2))
        colnames(DF_CMI_stock)<- c( "Item Refrence name", as.character(Sys.Date()) )
        for (i in 1: nrow(DF_CMI_stock)){
          for (j in 1: nrow(CMI_Df)){
            if (CMI_Df$`Item name`[j]==CMI_Refrence_name$`Item name (in the PDF's)`[i]){
              DF_CMI_stock[i,1]= CMI_Refrence_name$`Refrence name`[i]
              DF_CMI_stock[i,2]=CMI_Df$`Daily price $`[j]
            }
          }
        }
        DF_CMI_stock<-DF_CMI_stock %>% t() %>% as.data.frame()
        colnames(DF_CMI_stock) <- DF_CMI_stock[1, ]
        DF_CMI_stock<-DF_CMI_stock[-1,]
        CMI_Price_variation<- rbind(CMI_Price_variation,DF_CMI_stock)
      }
      # ------- 
      if(sum(rownames(ABC_Price_variation)[1:nrow(ABC_Price_variation)]==as.character(Sys.Date()))!=1){
        DF_ABC_stock <- data.frame(matrix(nrow = nrow(ABC_Df), ncol = 2))
        colnames(DF_ABC_stock)<- c( "Item Refrence name", as.character(Sys.Date()) )
        for (i in 1: nrow(DF_ABC_stock)){
          for (j in 1: nrow(ABC_Df)){
            if (ABC_Df$`Item name`[j]==ABC_Refrence_name$`Item name (in the PDF's)`[i]){
              DF_ABC_stock[i,1]= ABC_Refrence_name$`Refrence name`[i]
              DF_ABC_stock[i,2]=ABC_Df$`Daily price $`[j]
            }
          }
        }
        DF_ABC_stock<-DF_ABC_stock %>% t() %>% as.data.frame()
        colnames(DF_ABC_stock) <- DF_ABC_stock[1, ]
        DF_ABC_stock<-DF_ABC_stock[-1,]
        ABC_Price_variation<- rbind(ABC_Price_variation,DF_ABC_stock)
      }
      # ----
      
      saveRDS(CMI_Price_variation, file = "CMI_Histo_example.rds")
      saveRDS(ABC_Price_variation, file = "ABC_Histo_example.rds")
      
      # drop_upload("CMI_Histo_example.rds",path = "National_Salvage_V_Test", dtoken = token)
      # drop_upload("ABC_Histo_example.rds",path = "National_Salvage_V_Test", dtoken = token)
    } 
    
    ## -----------------------------
    
    values$CMI_Price_variation<-CMI_Price_variation
    values$ABC_Price_variation<-ABC_Price_variation
    
    # ------
    CMI_Date<-rownames(values$CMI_Price_variation)
    ABC_Date<-rownames(values$ABC_Price_variation)
    sheet_write("https://docs.google.com/spreadsheets/d/1sqZxzGNsn7Zoj0a_1Kam9fZy0aquleIqEjD067qZG1M/edit#gid=2013206145",
                data = cbind(CMI_Date,values$CMI_Price_variation), sheet = "CMI Historical Data")
    sheet_write("https://docs.google.com/spreadsheets/d/1sqZxzGNsn7Zoj0a_1Kam9fZy0aquleIqEjD067qZG1M/edit#gid=527635793",
                data =  cbind(ABC_Date,values$ABC_Price_variation), sheet = "ABC Historical Data")
    
    ## -----------------------------
    
    list <- Compare_fun(values$CMI_ABC_Df,CMI_Df,ABC_Df,CMI_Price_variation,ABC_Price_variation)
    values$Correspondance_Df <- list[[1]]
    values$non_Correspondance_Df <- list[[2]]
    values$DF_Best_Price <- list[[3]] 
    
  })
  
  #--------------------
  
  output$purchase_list <- renderUI({
    req(input$PDF_file1)
    req(input$PDF_file2)
    CMI <- values$DF_Best_Price[values$DF_Best_Price$`Best Purchaser`=="CMI",]
    if(nrow(CMI)>0){
      CMI <- sapply(
        1:nrow(CMI),
        function(i){
          paste0("<li>", CMI$`Item Name`[i],": $",CMI$`CMI Price`[i], "</li>")
        }) %>% paste0(collapse = "")
    } else{
      CMI<- paste("<li> No Items for CMI </li>")
    }
    
    ABC <- values$DF_Best_Price[values$DF_Best_Price$`Best Purchaser`=="ABC",]
    if(nrow(ABC)>0){
      ABC <- sapply(
        1:nrow(ABC),
        function(i){
          paste0("<li>", ABC$`Item Name`[i],": $",ABC$`ABC Price`[i], "</li>")
        }) %>% paste0(collapse = "")
    } else{
      ABC<- paste("<li> No Items for ABC </li>")
    }
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
               )
        )
      )
    )
  })
  
  output$DF_Best_Price <- DT::renderDataTable({
    req(input$PDF_file1)
    
    values$DF_Best_Price %>% 
      DT::datatable( escape=F, rownames = F,
                     callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                     options = list(drawCallback =  JS('function(){debugger;HTMLWidgets.staticRender();}') 
                                    ,lengthChange = F, paging = F,selection="single"))
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
  
  output$Hc_Price_variation <- renderHighchart({
    req(input$PDF_file1)
    req(input$PDF_file2)
    highchart() %>% 
      hc_chart(type = "line") %>%
      hc_xAxis(categories =rownames(values$CMI_Price_variation),title = list(text= '<b> Date <b>')) %>%
      hc_yAxis(min= 0, title=list(text= "<b> Price ($) <b>")) %>%
      hc_add_series(name="CMI Price variation over Time",
                    data = values$CMI_Price_variation[,input$CMI_referance] %>% as.numeric())%>%
      hc_add_series(name="ABC Price variation over Time",
                    data = values$ABC_Price_variation[,input$ABC_referance] %>% as.numeric())
  })
  
  
  output$Correspondance_Df <- DT::renderDataTable({
    values$Correspondance_Df %>% 
      DT::datatable( escape=F, rownames = F, 
                     callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                     options = list(lengthChange = F, paging = F,selection="single"))
  })
  
  output$non_Correspondance_Df <- DT::renderDataTable({
    values$non_Correspondance_Df %>% 
      DT::datatable( escape=F, rownames = F, 
                     callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                     options = list(lengthChange = F, paging = F,selection="single"))
  })
  
  output$download_CMI_Data <- downloadHandler(
    filename = function() { 
      paste("CMI_Historical_Data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$CMI_Price_variation, file)
    })
  
  output$download_ABC_Data <- downloadHandler(
    filename = function() { 
      paste("ABC_Historical_Data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$ABC_Price_variation, file)
    })
}

