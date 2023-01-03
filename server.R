server <- function(input, output, session) {


  values <- reactiveValues(
    PDF_CMI = NULL,
    PDF_ABC = NULL,
    CMI_ABC_Df = NULL,
    CMI_Reference_name= NULL,
    Correspondance_Df = NULL,
    non_Correspondance_Df = NULL,
    DF_Best_Price = NULL
  )
  
  observeEvent(input$Run,{
    
    CMI_ABC_Reference_name <<- read_sheet("https://docs.google.com/spreadsheets/d/1ABYeL_aWjM8RZcevTXWnTAyl_meV2RVTdQFKvfIeOXI/edit#gid=0",
                                        sheet = "Reference name all items")
    
    CMI_Price_variation <<- read_sheet("https://docs.google.com/spreadsheets/d/1ABYeL_aWjM8RZcevTXWnTAyl_meV2RVTdQFKvfIeOXI/edit#gid=0",
                                      sheet = "CMI Historical Data") %>% as.data.frame()
    ABC_Price_variation  <<-  read_sheet("https://docs.google.com/spreadsheets/d/1ABYeL_aWjM8RZcevTXWnTAyl_meV2RVTdQFKvfIeOXI/edit#gid=0",
                                      sheet = "ABC Historical Data") %>% as.data.frame()
    
    rownames(CMI_Price_variation)  <<- CMI_Price_variation[,1]
    CMI_Price_variation  <<- CMI_Price_variation[,-1]
    
    rownames(ABC_Price_variation) <<- ABC_Price_variation[,1]
    ABC_Price_variation  <<- ABC_Price_variation[,-1]
    
    Reference_Name  <<- Reference_Name_All_Items(CMI_ABC_Reference_name)
    CMI_Reference_name  <<-  Reference_Name[[1]]
    ABC_Reference_name  <<-  Reference_Name[[2]]
    CMI_ABC_Df  <<-  Reference_Name[[3]]

    ## -----------------------------
    values$PDF_CMI <-pdf_text(input$PDF_file1$datapath)%>% 
      str_split("\n")
    values$PDF_ABC <-pdf_text(input$PDF_file2$datapath)%>% 
      str_split("\n")
    
    CMI_Df <- CMI_fun(values$PDF_CMI)
    ABC_Df <- ABC_fun(values$PDF_ABC)

    ## -----------------------------
    if (input$UpdateGS){
      CMI_Pricing <-read_sheet("https://docs.google.com/spreadsheets/d/1ABYeL_aWjM8RZcevTXWnTAyl_meV2RVTdQFKvfIeOXI/edit#gid=0",
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
      sheet_write("https://docs.google.com/spreadsheets/d/1ABYeL_aWjM8RZcevTXWnTAyl_meV2RVTdQFKvfIeOXI/edit#gid=0",
                  data = DF_to_overwrite, sheet = "Scrapper for R")
      
      
    }
    ## -----------------------------
    
    values$CMI_Price_variation<-CMI_Price_variation
    values$ABC_Price_variation<-ABC_Price_variation

    ## Update CMI Platinum Pricing Sheet -----------------------------
    if(input$Update_HD){
      if(nrow(values$CMI_Price_variation) > 0){
        if( sum(rownames(values$CMI_Price_variation)[1:nrow(values$CMI_Price_variation)]==as.character(Sys.Date()))==1){
          values$CMI_Price_variation <- values$CMI_Price_variation[-nrow(values$CMI_Price_variation),]
        }
      }
      ## Adding new reference names to historical data ------------------
      
      for(i in 1:nrow(CMI_Reference_name)){
        if (sum(colnames(values$CMI_Price_variation)[1:ncol(values$CMI_Price_variation)]==CMI_Reference_name$`Reference name`[i])==0){
          values$CMI_Price_variation<- values$CMI_Price_variation %>% 
            mutate(newcol=NA)
          colnames(values$CMI_Price_variation)[ncol(values$CMI_Price_variation)]=paste0(CMI_Reference_name$`Reference name`[i])
        }
        
      } 
      
      ##  if an item name from the pdf doesn't match with reference name sheet item's name------------------
      
      DF_CMI_stock <- data.frame(matrix(nrow = ncol(values$CMI_Price_variation), ncol = 2))
      colnames(DF_CMI_stock)<- c( "Reference name", as.character(Sys.Date()) )
      
      
      for (i in 1: nrow(CMI_Reference_name)){
        for (j in 1: nrow(CMI_Df)){
          if (CMI_Df$`Item name`[j]==CMI_Reference_name$`Item name (in the PDF's)`[i]){
            DF_CMI_stock[i,1]= CMI_Reference_name$`Reference name`[i]
            DF_CMI_stock[i,2]=CMI_Df$`Daily price $`[j]
          }
        } 
      }
      
      DF_CMI_stock=DF_CMI_stock[!is.na(DF_CMI_stock$`Reference name`),] 
      
      CMI_Missing_from_PDf <- data.frame(matrix(nrow = ncol(values$CMI_Price_variation), ncol = 3))
      colnames(CMI_Missing_from_PDf) = c("`Item name ","Reference name", as.character(Sys.Date()))
      
      
      for (i in 1: nrow(CMI_Missing_from_PDf)){
        for (j in 1: nrow(CMI_Reference_name)){
          if (sum(CMI_Df$`Item name`[1:nrow(CMI_Df)]==CMI_Reference_name$`Item name (in the PDF's)`[j])==0){
            CMI_Missing_from_PDf[j,1]= CMI_Reference_name$`Item name (in the PDF's)`[j]
            CMI_Missing_from_PDf[j,2]= CMI_Reference_name$`Reference name`[j]
          }
        } 
      } 
      CMI_Missing_from_PDf=CMI_Missing_from_PDf[!is.na(CMI_Missing_from_PDf[,1]),] 
      
      DF_CMI_stock<-rbind(DF_CMI_stock,CMI_Missing_from_PDf[,-1])
      
      
      
      DF_CMI_stock<-DF_CMI_stock  %>%  t() %>% as.data.frame()
      colnames(DF_CMI_stock) <- DF_CMI_stock[1, ]
      DF_CMI_stock<-DF_CMI_stock[-1,]
      
      if(ncol(values$CMI_Price_variation)>ncol(DF_CMI_stock) ){
        for(i in 1:ncol(values$CMI_Price_variation)){
          if (sum(colnames(DF_CMI_stock)[1:ncol(DF_CMI_stock)]==colnames(values$CMI_Price_variation)[i])==0){
            DF_CMI_stock<- DF_CMI_stock %>% 
              mutate(newcol=NA)
            colnames(DF_CMI_stock)[ncol(DF_CMI_stock)]=paste0(colnames(values$CMI_Price_variation)[i])
          }
          
        } 
      }
      
      values$CMI_Price_variation<- rbind(values$CMI_Price_variation,DF_CMI_stock)
      
      CMI_NewItems_in_PDf <- data.frame(matrix(nrow = ncol(values$CMI_Price_variation), ncol = 1))
      colnames(CMI_NewItems_in_PDf) = "Item name"
      
      for (i in 1: nrow(CMI_NewItems_in_PDf)){
        for (j in 1: nrow(CMI_Df)){
          if (sum(CMI_Reference_name$`Item name (in the PDF's)`[1:nrow(CMI_Reference_name)]==CMI_Df$`Item name`[j])==0){
            CMI_NewItems_in_PDf[j,1]=CMI_Df$`Item name`[j]
          }
        } 
      } 
      CMI_NewItems_in_PDf=CMI_NewItems_in_PDf[!is.na(CMI_NewItems_in_PDf$`Item name`),] %>% as.data.frame()
      
      ##### ABC -----------
      
      if(nrow(values$ABC_Price_variation) > 0){
        if( sum(rownames(values$ABC_Price_variation)[1:nrow(values$ABC_Price_variation)]==as.character(Sys.Date()))==1){
          values$ABC_Price_variation <- values$ABC_Price_variation[-nrow(values$ABC_Price_variation),]
        }
      }
      ## Adding new reference names to historical data ------------------
      
      for(i in 1:nrow(ABC_Reference_name)){
        if (sum(colnames(values$ABC_Price_variation)[1:ncol(values$ABC_Price_variation)]==ABC_Reference_name$`Reference name`[i])==0){
          values$ABC_Price_variation<- values$ABC_Price_variation %>% 
            mutate(newcol=NA)
          colnames(values$ABC_Price_variation)[ncol(values$ABC_Price_variation)]=paste0(ABC_Reference_name$`Reference name`[i])
        }
        
      } 
      
      ##  if an item name from the pdf doesn't match with reference name sheet item's name------------------
      
      DF_ABC_stock <- data.frame(matrix(nrow = ncol(values$ABC_Price_variation), ncol = 2))
      colnames(DF_ABC_stock)<- c( "Reference name", as.character(Sys.Date()) )
      
      
      for (i in 1: nrow(ABC_Reference_name)){
        for (j in 1: nrow(ABC_Df)){
          if (ABC_Df$`Item name`[j]==ABC_Reference_name$`Item name (in the PDF's)`[i]){
            DF_ABC_stock[i,1]= ABC_Reference_name$`Reference name`[i]
            DF_ABC_stock[i,2]=ABC_Df$`Daily price $`[j]
          }
        } 
      }
      
      DF_ABC_stock=DF_ABC_stock[!is.na(DF_ABC_stock$`Reference name`),] 
      
      ABC_Missing_from_PDf <- data.frame(matrix(nrow = ncol(values$ABC_Price_variation), ncol = 3))
      colnames(ABC_Missing_from_PDf) = c("`Item name ","Reference name", as.character(Sys.Date()))
      
      
      for (i in 1: nrow(ABC_Missing_from_PDf)){
        for (j in 1: nrow(ABC_Reference_name)){
          if (sum(ABC_Df$`Item name`[1:nrow(ABC_Df)]==ABC_Reference_name$`Item name (in the PDF's)`[j])==0){
            ABC_Missing_from_PDf[j,1]= ABC_Reference_name$`Item name (in the PDF's)`[j]
            ABC_Missing_from_PDf[j,2]= ABC_Reference_name$`Reference name`[j]
          }
        } 
      } 
      ABC_Missing_from_PDf=ABC_Missing_from_PDf[!is.na(ABC_Missing_from_PDf[,1]),] 
      
      DF_ABC_stock<-rbind(DF_ABC_stock,ABC_Missing_from_PDf[,-1])
      
      
      
      DF_ABC_stock<-DF_ABC_stock  %>%  t() %>% as.data.frame()
      colnames(DF_ABC_stock) <- DF_ABC_stock[1, ]
      DF_ABC_stock<-DF_ABC_stock[-1,]
      
      if(ncol(values$ABC_Price_variation)>ncol(DF_ABC_stock) ){
        for(i in 1:ncol(values$ABC_Price_variation)){
          if (sum(colnames(DF_ABC_stock)[1:ncol(DF_ABC_stock)]==colnames(values$ABC_Price_variation)[i])==0){
            DF_ABC_stock<- DF_ABC_stock %>% 
              mutate(newcol=NA)
            colnames(DF_ABC_stock)[ncol(DF_ABC_stock)]=paste0(colnames(values$ABC_Price_variation)[i])
          }
          
        } 
      }
      
      values$ABC_Price_variation<- rbind(values$ABC_Price_variation,DF_ABC_stock)
      
      ABC_NewItems_in_PDf <- data.frame(matrix(nrow = ncol(values$CMI_Price_variation), ncol = 1))
      colnames(ABC_NewItems_in_PDf) = "Item name"
      for (i in 1: nrow(ABC_NewItems_in_PDf)){
        for (j in 1: nrow(ABC_Df)){
          if (sum(ABC_Reference_name$`Item name (in the PDF's)`[1:nrow(ABC_Reference_name)]==ABC_Df$`Item name`[j])==0){
            ABC_NewItems_in_PDf[j,1]=ABC_Df$`Item name`[j]
          }
        } 
      } 
      ABC_NewItems_in_PDf=ABC_NewItems_in_PDf[!is.na(ABC_NewItems_in_PDf$`Item name`),] %>% as.data.frame()
      
      
      ## Warnings -----------
      
      if(nrow(CMI_Missing_from_PDf)>0 ){
        CMI_Missing_htlm <- sapply(
          1:nrow(CMI_Missing_from_PDf),
          function(i){
            paste0("<li>", CMI_Missing_from_PDf[i,1],"</li>")
          }) %>% paste0(collapse = "")
      } else{
        CMI_Missing_htlm<- paste("<li> all item names in the Sheet correspond to those in the PDF </li>")
      }
      
      
      
      if(nrow(CMI_NewItems_in_PDf)>0 ){
        CMI_NewItems_htlm <- sapply(
          1:nrow(CMI_NewItems_in_PDf),
          function(i){
            #paste0("<div  style=text-align: left <li>", CMI_NewItems_in_PDf[i,1]," </li></div>")
            paste0("<li>", CMI_NewItems_in_PDf[i,1],"</li>")
          }) %>% paste0(collapse = "")
        
      } else{
        CMI_NewItems_htlm<- paste("<li> all item names in the PDF correspond to those in the Sheet </li>")
      }
      
      
      
      if(nrow(ABC_Missing_from_PDf)>0 ){
        ABC_Missing_htlm <- sapply(
          1:nrow(ABC_Missing_from_PDf),
          function(i){
            paste0("<li>", ABC_Missing_from_PDf[i,1],"</li>")
          }) %>% paste0(collapse = "")
      } else{
        ABC_Missing_htlm<- paste("<li> all item names in the Sheet correspond to those in the PDF </li>")
      }
      
      
      
      if(nrow(ABC_NewItems_in_PDf)>0 ){
        ABC_NewItems_htlm <- sapply(
          1:nrow(ABC_NewItems_in_PDf),
          function(i){
            #paste0("<div  style=text-align: left <li>", ABC_NewItems_in_PDf[i,1]," </li></div>")
            paste0("<li>", ABC_NewItems_in_PDf[i,1],"</li>")
          }) %>% paste0(collapse = "")
        
      } else{
        ABC_NewItems_htlm<- paste("<li> all item names in the PDF correspond to those in the Sheet </li>")
      }
      
      
      
      if((nrow(CMI_Missing_from_PDf)>0 & nrow(ABC_Missing_from_PDf)>0) | (nrow(CMI_NewItems_in_PDf)>0 & nrow(ABC_NewItems_in_PDf)>0)){
        shinyalert( "Warning messages!",
                    html = TRUE,
                    text =  tagList(
                      fluidRow(
                        HTML(
                          paste("<font color=\"#FF0000\"><p> <b> Please Update Reference name's Sheet  </b></p></font>")),
                        br(),
                        column(3,
                               HTML(
                                 paste("<p> <b>  Missing items from CMI pdf: </b></p>")),
                               br(),
                               HTML(paste0(CMI_Missing_htlm))
                        ),
                        column(3,
                               HTML(
                                 paste("<p> <b>  Missing items from CMI Reference name: </b></p>")),
                               br(),
                               HTML( paste0(CMI_NewItems_htlm))
                        ),
                        column(3,
                               HTML(
                                 paste("<p> <b>  Missing items from ABC pdf: </b></p>")),
                               br(),
                               HTML(paste0(ABC_Missing_htlm))
                        ),
                        column(3,
                               HTML(
                                 paste("<p> <b>  Missing items from ABC Reference name: </b></p>")),
                               br(),
                               HTML(paste0(ABC_NewItems_htlm))
                        )
                      )), type = "warning", size = "l", animation = TRUE)
        
      } else if(nrow(CMI_Missing_from_PDf)>0 |nrow(CMI_NewItems_in_PDf)>0){
        shinyalert( "Warning messages!",
                    html = TRUE,
                    text =  tagList(
                      fluidRow(
                        HTML(
                          paste("<p> <b> Please Update Reference name's Sheet  </b></p>")),
                        br(),
                        column(6,
                               HTML(
                                 paste("<p> <b>  Missing items from CMI pdf: </b></p>")),
                               br(),
                               HTML(paste0(CMI_Missing_htlm)
                               )
                        ),
                        column(6,
                               HTML(
                                 paste("<p> <b>  Missing items from CMI Reference name: </b></p>")),
                               br(),
                               HTML(
                                 paste0(CMI_NewItems_htlm)
                               )
                        ))
                    ), type = "warning", size = "m", animation = TRUE)
      } else if(nrow(CMI_Missing_from_PDf)>0 |nrow(CMI_NewItems_in_PDf)>0){
        shinyalert( "Warning messages!",
                    html = TRUE,
                    text =  tagList(
                      fluidRow(
                        HTML(
                          paste("<p> <b> Please Update Reference name's Sheet  </b></p>")),
                        br(),
                        column(6,
                               HTML(
                                 paste("<p> <b>  Missing items from CMI pdf: </b></p>")),
                               br(),
                               HTML(paste0(CMI_Missing_htlm)
                               )
                        ),
                        column(6,
                               HTML(
                                 paste("<p> <b>  Missing items from CMI Reference name: </b></p>")),
                               br(),
                               HTML(
                                 paste0(CMI_NewItems_htlm)
                               )
                        ))
                    ), type = "warning", size = "m", animation = TRUE)
      } 
      
      
      
      
      sheet_write("https://docs.google.com/spreadsheets/d/1ABYeL_aWjM8RZcevTXWnTAyl_meV2RVTdQFKvfIeOXI/edit#gid=0",
                  data = cbind(CMI_Date=rownames(values$CMI_Price_variation),values$CMI_Price_variation), sheet = "CMI Historical Data")
      sheet_write("https://docs.google.com/spreadsheets/d/1ABYeL_aWjM8RZcevTXWnTAyl_meV2RVTdQFKvfIeOXI/edit#gid=0",
                  data =  cbind(ABC_Date=rownames(values$ABC_Price_variation),values$ABC_Price_variation), sheet = "ABC Historical Data")
      
    }
    ## -----------------------------
    list <- Compare_fun(CMI_ABC_Df,CMI_Df,ABC_Df,values$CMI_Price_variation,values$ABC_Price_variation)
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
                    data = values$DF_Best_Price[,4])%>%
      hc_colors(c("#e9724d", "#0000FF"))  %>% 
      hc_tooltip(shared = TRUE,
                 crosshairs = TRUE,
                 followPointer = T,
                 borderColor = "grey")
  })
  
  output$Hc_Price_variation <- renderHighchart({
    req(input$PDF_file1)
    req(input$PDF_file2)
    hchat <- highchart() %>% 
      hc_chart(type = "line") %>%
      hc_xAxis(categories =rownames(values$CMI_Price_variation),title = list(text= '<b> Date <b>')) %>%
      hc_yAxis(min= 0, title=list(text= "<b> Price ($) <b>")) %>%
      hc_tooltip(shared = TRUE,
                 crosshairs = TRUE,
                 followPointer = T,
                 borderColor = "grey")
    for(i in 1:length(input$CMI_referance)){
      hchat <- hchat %>% 
        hc_add_series(name=paste0(input$CMI_referance[i]," - ","CMI"),
                      data = values$CMI_Price_variation[,input$CMI_referance[i]] %>% as.numeric())
    }
    for(i in 1:length(input$ABC_referance)){
      hchat <- hchat %>% 
        hc_add_series(name=paste0(input$ABC_referance[i]," - ","ABC"),
                      data = values$ABC_Price_variation[,input$ABC_referance[i]] %>% as.numeric())
    }
    hchat %>% hc_colors(
      c(
        rep("#50C878",length(input$CMI_referance)),
        rep("grey",length(input$ABC_referance))
      )
    )
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

