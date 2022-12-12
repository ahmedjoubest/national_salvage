CMI_fun<-function(PDF_CMI){
  CMI_char_length <- data.frame(matrix(nrow = length(PDF_CMI[[1]]), ncol = 4))
  colnames(CMI_char_length)=c("ID","num_char", "num_space","num_char_nospace")
  CMI_char_length<- CMI_char_length%>% 
    mutate(cond=" ")
  i=1
  while (i <= length(PDF_CMI[[1]])){
    if(str_count(PDF_CMI[[1]][i], '\\s\\$*\\d[:punct:]\\d+')>=1){
      CMI_char_length[i,1]=i
      CMI_char_length[i,2]=nchar(PDF_CMI[[1]][i])
      CMI_char_length[i,3]=str_count(PDF_CMI[[1]][i], ' ')
      CMI_char_length[i,4]=nchar(gsub(" ", "",PDF_CMI[[1]][i]))
      i=i+1
    } else{
      i=i+1
    }
  }
  CMI_char_length<-CMI_char_length %>% drop_na()
  for (i in 1:nrow(CMI_char_length)){
    CMI_char_length$cond[i]= (CMI_char_length$num_char_nospace[i] > CMI_char_length$num_space[i]) & 
      (CMI_char_length$num_char [i] == max(CMI_char_length$num_char)) & 
      (str_count(PDF_CMI[[1]][CMI_char_length$ID[i]], '\\s\\$\\d[:punct:]\\d+')==1)
    
  }
  CMI<- list()
  PDF_CMI_Left_Side <-list()
  PDF_CMI_Right_Side <-list()
  
  CMI <- sapply(1:nrow(CMI_char_length), function(i){
    PDF_CMI[[1]][CMI_char_length$ID[i]]
  }) 
  
  
  PDF_CMI_Left_Side <- sapply(1:nrow(CMI_char_length), function(i){
    if(nchar(CMI[i])==max(CMI_char_length$num_char)){
      str_sub(CMI[i], start =  1, end = nchar(CMI[i])/2) 
    } else{
      CMI[i]
    }
  }) 
  
  PDF_CMI_Right_Side <- sapply(1:nrow(CMI_char_length), function(i){
    if(nchar(CMI[i])==max(CMI_char_length$num_char)){
      str_sub(CMI[i], start =  nchar(CMI[i])/2, end = nchar(CMI[i])) 
    } else{
      paste(" ")
    }
  }) 
  New_PDF_CMI<- c(PDF_CMI_Left_Side,PDF_CMI_Right_Side)
  
  CMI_Df <- data.frame(matrix(nrow = length(PDF_CMI), ncol = 2)) 
  colnames(CMI_Df)<-c("Item name","Daily price $")
  
  for (i in 1:length(New_PDF_CMI)){
    CMI_Df[i,1]= str_match(New_PDF_CMI[i], "^((.*?))\\$+")[2]  %>% str_squish()
    CMI_Df[i,2]=sub(".*\\$" , "", New_PDF_CMI[i]) %>% as.numeric()
  }
  CMI_Df=CMI_Df[!is.na(CMI_Df[,2]),] 
  return(CMI_Df)
}
ABC_fun<-function(PDF_ABC){
  
  ABC_char_length <- data.frame(matrix(nrow = length(PDF_ABC[[1]]), ncol = 4))
  colnames(ABC_char_length)=c("ID","num_char", "num_space","num_char_nospace")
  ABC_char_length<- ABC_char_length%>% 
    mutate(cond=" ")
  i=1
  length(PDF_ABC[[1]])
  while (i <= length(PDF_ABC[[1]])){
    if(str_count(PDF_ABC[[1]][i], '\\s\\$\\d[:punct:]\\d+')>=1){
      ABC_char_length[i,1]=i
      ABC_char_length[i,2]=nchar(PDF_ABC[[1]][i])
      ABC_char_length[i,3]=str_count(PDF_ABC[[1]][i], ' ')
      ABC_char_length[i,4]=nchar(gsub(" ", "",PDF_ABC[[1]][i]))
      i=i+1
    } else{
      i=i+1
    }
  }
  ABC_char_length<-ABC_char_length %>% drop_na()
  for (i in 1:nrow(ABC_char_length)){
    ABC_char_length$cond[i]= (ABC_char_length$num_char_nospace[i] > ABC_char_length$num_space[i]) & 
      (ABC_char_length$num_char [i] == max(ABC_char_length$num_char)) & 
      (str_count(PDF_ABC[[1]][ABC_char_length$ID[i]], '\\s\\$\\d[:punct:]\\d+')==1)
    
  }
  
  ABC<- list()
  PDF_ABC_Left_Side <-list()
  PDF_ABC_Right_Side <-list()
  
  ABC <- sapply(1:nrow(ABC_char_length), function(i){
    PDF_ABC[[1]][ABC_char_length$ID[i]]
  }) 
  i=1
  while(i<= length(ABC)){
    if(ABC_char_length$cond[i]){
      ABC_Left_To_Add <- str_match(ABC[i], "^((.*?))\\s\\s\\s")[2]
      ABC_Right_To_Add <- str_match(ABC[i], "\\s\\s\\s*((.*?))\\s*$")[2]
      i=i+1
      j=i
      while(j<= length(ABC)){
        if(ABC_char_length$num_char[j]<max(ABC_char_length$num_char)){
          ABC_Left_To_Add<-paste0(ABC_Left_To_Add %>% noquote(),
                                  ABC[j] %>% noquote()) 
          j=j+1
          i=j
        } else{
          PDF_ABC_Left_Side[[i-3]]  <- ABC_Left_To_Add
          PDF_ABC_Right_Side[[i-3]] <- ABC_Right_To_Add
          break
        }
      }
    } else {
      PDF_ABC_Left_Side[[i]] <- str_match(ABC[[i]], "^((.*?))\\d[:punct:]\\d+")[,1]
      PDF_ABC_Right_Side[[i]] <- str_match(ABC[[i]], "\\d+[:punct:]\\d+\\s*((.*?))\\s*$")[,2]
      i=i+1
    }
  }
  
  
  New_PDF_ABC<- c(PDF_ABC_Left_Side,PDF_ABC_Right_Side)
  New_PDF_ABC <- New_PDF_ABC %>% unlist() %>% list()
  test<-New_PDF_ABC
  
  ABC_Df <- data.frame(matrix(nrow = length(PDF_ABC), ncol = 2)) 
  colnames(ABC_Df)<-c("Item name","Daily price $")
  for (i in 1:length(New_PDF_ABC[[1]])){
    if(nchar(New_PDF_ABC[[1]][i])==max(ABC_char_length$num_char)){
      New_PDF_ABC[[1]][i]=str_match(New_PDF_ABC[[1]][i], "\\s\\s\\s*((.*?))$")[2]
      ABC_Df[i,1]= str_match(New_PDF_ABC[[1]][i], "^((.*?))\\s\\s\\s")[2]
      ABC_Df[i,2]=sub(".*\\$" , "", New_PDF_ABC[[1]][i]) %>% as.numeric()
    } else {
      ABC_Df[i,1]= str_match(New_PDF_ABC[[1]][i], "^((.*?))\\s\\s\\s")[2]
      ABC_Df[i,2]=sub(".*\\$" , "", New_PDF_ABC[[1]][i]) %>% as.numeric()
    }
  }
  ABC_Df=ABC_Df[!is.na(ABC_Df[,2]),] 
  #ABC_Df<-ABC_Df[!duplicated(ABC_Df[,1]),]
  
  return(ABC_Df)
}
Compare_fun<-function(CMI_ABC_Df,CMI_Df,ABC_Df){
  
  CMI_ABC_Df <- CMI_ABC_Df[!is.na(CMI_ABC_Df$...4),] 
  CMI_ABC_Df <- CMI_ABC_Df%>% 
    select(CMI,...7,...4)
  
  colnames(CMI_ABC_Df)<- c("CMI Items","ABC Items", "Common Item Names")
  
  for (i in 1:nrow(CMI_ABC_Df)){
    for (j in 1:length(CMI_ABC_Df)){
      CMI_ABC_Df[i,j] <- gsub("\\\r\n", "",CMI_ABC_Df[i,j]) 
    }
  }
  
  Correspondance_Df=data.frame(matrix(nrow=nrow(CMI_ABC_Df), ncol = 3)) 
  colnames(Correspondance_Df) = c("CMI Items","ABC Items", "Common Item Names")
  
  non_Correspondance_Df=data.frame(matrix(nrow=nrow(CMI_ABC_Df), ncol = 3)) 
  colnames(non_Correspondance_Df) = c("CMI Items","ABC Items", "Common Item Names")
  
  i=1
  while(i<=nrow(CMI_ABC_Df)){
    if((CMI_Df$`Item name`== CMI_ABC_Df$`CMI Items`[i]) %>% sum()==1){
      Correspondance_Df[i,1]= CMI_Df$`Item name`[CMI_Df$`Item name`== CMI_ABC_Df$`CMI Items`[i]]
    } else {
      Correspondance_Df[i,1]= NA
    }
    if((ABC_Df$`Item name`== CMI_ABC_Df$`ABC Items`[i]) %>% sum()==1){
      Correspondance_Df[i,2]= ABC_Df$`Item name`[ABC_Df$`Item name`== CMI_ABC_Df$`ABC Items`[i]]
    } else {
      Correspondance_Df[i,2]=  NA
    }
    Correspondance_Df[i,3]= CMI_ABC_Df$`Common Item Names` [i]
    i=i+1
  }
  
  for(i in 1:nrow(Correspondance_Df)){
    if(is.na(Correspondance_Df[i,]) %>% sum()>=1){
      non_Correspondance_Df[i,]<-CMI_ABC_Df[i,]
    }
  }
  
  Correspondance_Df<-na.omit(Correspondance_Df)
  non_Correspondance_Df<-na.omit(non_Correspondance_Df)
  
  Df_Comparable = data.frame(matrix(nrow=nrow(Correspondance_Df), ncol = 4)) 
  colnames(Df_Comparable) = c("CMI Items","CMI Price","ABC Items","ABC Price") 
  for(i in 1:nrow(Correspondance_Df)){
    Df_Comparable[i,1]=(CMI_Df %>% filter(CMI_Df$`Item name`== Correspondance_Df$`CMI Items`[i]))[1]
    Df_Comparable[i,2]=(CMI_Df %>% filter(CMI_Df$`Item name`== Correspondance_Df$`CMI Items`[i]))[2] %>% as.numeric()
    Df_Comparable[i,3]=(ABC_Df %>% filter(ABC_Df$`Item name`== Correspondance_Df$`ABC Items`[i]))[1]
    Df_Comparable[i,4]=(ABC_Df %>% filter(ABC_Df$`Item name`== Correspondance_Df$`ABC Items`[i]))[2] %>% as.numeric()
  }
  
  
  
  
  DF_Best_Price<-cbind ("Item Name"=Correspondance_Df$`Common Item Names` ,Df_Comparable) %>% 
    select("Item Name","CMI Price","ABC Price")
  
  
  DF_Best_Price <- DF_Best_Price %>% 
    mutate(`Best Purchaser`="")
  
  for (i in 1:nrow(DF_Best_Price)){
    if(DF_Best_Price$`CMI Price`[i]>DF_Best_Price$`ABC Price`[i]){
      DF_Best_Price$`Best Purchaser`[i]="CMI"
    } else {
      DF_Best_Price$`Best Purchaser`[i]="ABC"
    }
  }
  
  return(list(Correspondance_Df,non_Correspondance_Df,DF_Best_Price))
}

#PDF_CMI <- pdf_text("CMI Px 11 28 22.pdf")%>% 
#  str_split("\n")
#
#
#PDF_ABC <- pdf_text("National Salvage 11-28-22 (ABC PL).pdf")%>% 
#  strsplit("\n") 
#
#CMI_ABC_Df <- readxl::read_excel("PL1-PL2 comparison.xlsx", sheet = "Sheet1", na = "n/a")
#
#CMI_Df <- CMI_fun(PDF_CMI)
#
#ABC_Df <- ABC_fun(PDF_ABC)
#
#
#Correspondance_Df <- Compare_fun(CMI_ABC_Df,CMI_Df,ABC_Df)[1] %>% as.data.frame()
#
#non_Correspondance_Df <- Compare_fun(CMI_ABC_Df,CMI_Df,ABC_Df)[2] %>% as.data.frame()
#
#DF_Best_Price <- Compare_fun(CMI_ABC_Df,CMI_Df,ABC_Df)[3] %>% as.data.frame()
#
#highchart() %>% 
#  hc_chart(type = "column") %>%
#  hc_xAxis(categories =DF_Best_Price[,1],title = list(text= '<b> Item Name <b>')) %>%
#  hc_yAxis(min= 0, title=list(text= "<b> Price ($) <b>")) %>%
#  hc_add_series(name="CMI Price",
#                data = DF_Best_Price[,2]) %>%
#  hc_add_series(name="ABC Price",
#                data = DF_Best_Price[,3])%>%
#  hc_colors(c("#e9724d", "#0000FF")) %>% 
#  hc_tooltip(shared = TRUE,
#             crosshairs = TRUE,
#             followPointer = T,
#             borderColor = "grey")
  
  
  
  
  