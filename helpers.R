#Reading and cleaning Data from Google Sheet ---------
CMI_fun<-function(PDF_CMI){
  CMI_char_length <- data.frame(matrix(nrow = length(PDF_CMI[[1]]), ncol = 4))
  colnames(CMI_char_length)=c("ID","num_char", "num_space","num_char_nospace")
  CMI_char_length<- CMI_char_length%>% 
    mutate(cond=" ")
  
  #Detect Date ---------
  
  for(i in 1:length(PDF_CMI[[1]]) ){
    if(str_detect(PDF_CMI[[1]][i],"\\d{4}-\\d{2}-\\d{2}")){
      CMI_Date <- str_extract(PDF_CMI[[1]][i], "\\d{4}-\\d{2}-\\d{2}") 
      
    } else if(str_detect(PDF_CMI[[1]][i],"\\d{2}-\\d{2}-\\d{4}")){
      CMI_Date <- str_extract(PDF_CMI[[1]][i], "\\d{2}-\\d{2}-\\d{4}")
      CMI_Date <- as.Date(strptime(CMI_Date, format = "%d-%m-%Y"))
      
    } else if(str_detect(PDF_CMI[[1]][i],"\\d{1,2}-\\w{3}-\\d{2}")){
      CMI_Date <- str_extract(PDF_CMI[[1]][i], "\\d{1,2}-\\w{3}-\\d{2}")
      CMI_Date <- format(dmy(CMI_Date), "%d-%m-%Y")
      CMI_Date <- as.Date(strptime(CMI_Date, format = "%d-%m-%Y"))
    }
  }
  #---------
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
  return(list(CMI_Df=CMI_Df,CMI_Date=CMI_Date))
  
}

ABC_fun<-function(PDF_ABC){
  ABC_char_length <- data.frame(matrix(nrow = length(PDF_ABC[[1]]), ncol = 4))
  colnames(ABC_char_length)=c("ID","num_char", "num_space","num_char_nospace")
  ABC_char_length<- ABC_char_length%>% 
    mutate(cond=" ")
  
  ###-----------------
  for(i in 1:length(PDF_ABC[[1]]) ){
    if(str_detect(PDF_ABC[[1]][i],"\\d{4}-\\d{2}-\\d{2}")){
      ABC_Date <- str_extract(PDF_ABC[[1]][i], "\\d{4}-\\d{2}-\\d{2}")
      
    } else if(str_detect(PDF_ABC[[1]][i],"\\d{2}-\\d{2}-\\d{4}")){
      ABC_Date <- str_extract(PDF_ABC[[1]][i], "\\d{2}-\\d{2}-\\d{4}")
      ABC_Date <- as.Date(strptime(ABC_Date, format = "%d-%m-%Y"))
    } else if(str_detect(PDF_ABC[[1]][i],"\\d{1,2}-\\w{3}-\\d{2}")){
      ABC_Date <- str_extract(PDF_ABC[[1]][i], "\\d{1,2}-\\w{3}-\\d{2}")
      ABC_Date <- format(dmy(ABC_Date), "%d-%m-%Y")
      ABC_Date <- as.Date(strptime(ABC_Date, format = "%d-%m-%Y"))

    }
  }
  
  ###-----------------
  i=1
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
      PDF_ABC_Left_Side[[i]] <- str_match(ABC[[i]], "^((.*?))\\$\\d[:punct:]\\d+")[,1]
      PDF_ABC_Right_Side[[i]] <- str_match(ABC[[i]], "\\$\\d+[:punct:]\\d+\\s*((.*?))\\s*$")[,2]
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
  
  return(list(ABC_Df=ABC_Df,ABC_Date=ABC_Date))
}

Reference_Name_All_Items<- function(CMI_ABC_Reference_name){
  ##-----
  for (i in 1:nrow(CMI_ABC_Reference_name)){
    for (j in 1:ncol(CMI_ABC_Reference_name)){
      CMI_ABC_Reference_name[i,j] <- gsub("\n", "",CMI_ABC_Reference_name[i,j]) 
    }
  }
  
  CMI_ABC_Reference_name<-CMI_ABC_Reference_name[,-3]
  CMI_Reference_name<-CMI_ABC_Reference_name[,c(1,2)] %>% drop_na()
  colnames(CMI_Reference_name) <- CMI_Reference_name[1, ]
  CMI_Reference_name<-CMI_Reference_name[-1,]
  ABC_Reference_name<-CMI_ABC_Reference_name[,c(3,4)] %>% drop_na()
  colnames(ABC_Reference_name) <- ABC_Reference_name[1, ]
  ABC_Reference_name<-ABC_Reference_name[-1,]
  
  CMI_ABC_Df = data.frame(matrix(nrow=nrow(CMI_ABC_Reference_name), ncol = 3)) 
  colnames(CMI_ABC_Df)<- c("CMI Items","Commun Item Names","ABC Items")
  
  for(i in 1:nrow(CMI_Reference_name)){
    for(j in 1:nrow(ABC_Reference_name)){
      if(CMI_Reference_name$`Reference name`[i]==ABC_Reference_name$`Reference name`[j]){
        CMI_ABC_Df$`CMI Items`[i]=CMI_Reference_name$`Item name (in the PDF's)`[i]
        CMI_ABC_Df$`ABC Items`[i]=ABC_Reference_name$`Item name (in the PDF's)`[j]
        CMI_ABC_Df$`Commun Item Names`[i]=CMI_Reference_name$`Reference name`[i]
      }
    }
  }
  
  CMI_ABC_Df <- CMI_ABC_Df %>% drop_na()
  
  return(list(CMI_Reference_name,ABC_Reference_name,CMI_ABC_Df))
  
}

Compare_fun<-function(CMI_ABC_Df,CMI_Df,ABC_Df,CMI_Price_variation,ABC_Price_variation){
  
  ##----
  Correspondance_Df=data.frame(matrix(nrow=nrow(CMI_ABC_Df), ncol = 3)) 
  colnames(Correspondance_Df) = c("CMI Items","ABC Items", "Commun Item Names")
  
  non_Correspondance_Df=data.frame(matrix(nrow=nrow(CMI_ABC_Df), ncol = 3)) 
  colnames(non_Correspondance_Df) = c("CMI Items", "Commun Item Names","ABC Items")
  
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
    Correspondance_Df[i,3]= CMI_ABC_Df$`Commun Item Names` [i]
    i=i+1
  }
  
  for(i in 1:nrow(Correspondance_Df)){
    if(is.na(Correspondance_Df[i,]) %>% sum()>=1){
      non_Correspondance_Df[i,]<-CMI_ABC_Df[i,]
    }
  }
  
  Correspondance_Df<-na.omit(Correspondance_Df)
  non_Correspondance_Df<-na.omit(non_Correspondance_Df) %>% select("CMI Items","ABC Items", "Commun Item Names")
  
  Df_Comparable = data.frame(matrix(nrow=nrow(Correspondance_Df), ncol = 4)) 
  
  colnames(Df_Comparable) = c("CMI Items","CMI Price","ABC Items","ABC Price") 
  for(i in 1:nrow(Correspondance_Df)){
    Df_Comparable[i,1]=(CMI_Df %>% filter(CMI_Df$`Item name`== Correspondance_Df$`CMI Items`[i]))[1]
    Df_Comparable[i,2]=(CMI_Df %>% filter(CMI_Df$`Item name`== Correspondance_Df$`CMI Items`[i]))[2] %>% as.numeric()
    Df_Comparable[i,3]=(ABC_Df %>% filter(ABC_Df$`Item name`== Correspondance_Df$`ABC Items`[i]))[1]
    Df_Comparable[i,4]=(ABC_Df %>% filter(ABC_Df$`Item name`== Correspondance_Df$`ABC Items`[i]))[2] %>% as.numeric()
  }
  
  
  
  
  DF_Best_Price<-cbind ("Item Name"=Correspondance_Df$`Commun Item Names` ,Df_Comparable) %>% 
    mutate(`CMI Variation` = NA) %>% 
    mutate(`ABC Variation` = NA) %>%
    select("Item Name","CMI Price","CMI Variation","ABC Price","ABC Variation") %>% 
    mutate(`Best Purchaser`="")
  
  for(i in 1:nrow(DF_Best_Price)){
    if(DF_Best_Price$`CMI Price`[i]>DF_Best_Price$`ABC Price`[i]){
      DF_Best_Price$`Best Purchaser`[i]="CMI"
    } else {
      DF_Best_Price$`Best Purchaser`[i]="ABC"
    }
    
    for(j in 1:ncol(CMI_Price_variation)){
      if(!is.na(colnames(CMI_Price_variation)[j])){
        if(DF_Best_Price$`Item Name`[i]==colnames(CMI_Price_variation)[j]){
          DF_Best_Price$`CMI Variation`[i]=spk_chr(CMI_Price_variation[,colnames(CMI_Price_variation)[j]], type = 'line')
        }
      }
    }
    for(j in 1:ncol(ABC_Price_variation)){
      if(!is.na(colnames(ABC_Price_variation)[j])){
        if(DF_Best_Price$`Item Name`[i]==colnames(ABC_Price_variation)[j]){
          DF_Best_Price$`ABC Variation`[i]= spk_chr(ABC_Price_variation[,colnames(ABC_Price_variation)[j]], type = 'line')
        }
      }
    }
  }
  
  return(list(Correspondance_Df=Correspondance_Df,
              non_Correspondance_Df =non_Correspondance_Df,
              DF_Best_Price = DF_Best_Price
  )
  )
}


########--------------
HTML_css <- function() {
  library(shiny)
  return(HTML("
       .cssload-loader {
       width: 244px;
       height: 49px;
       line-height: 49px;
       text-align: center;
       position: absolute;
       left: 50%;
       transform: translate(-50%, -50%);
       -o-transform: translate(-50%, -50%);
       -ms-transform: translate(-50%, -50%);
       -webkit-transform: translate(-50%, -50%);
       -moz-transform: translate(-50%, -50%);
       font-family: helvetica, arial, sans-serif;
       text-transform: uppercase;
       font-weight: 900;
       font-size:18px;
       color: #0275D8;
       letter-spacing: 0.2em;
       }
       .cssload-loader::before, .cssload-loader::after {
       content: '';
       display: block;
       width: 15px;
       height: 15px;
       background: #0275D8;
       position: absolute;
       animation: cssload-load 0.81s infinite alternate ease-in-out;
       -o-animation: cssload-load 0.81s infinite alternate ease-in-out;
       -ms-animation: cssload-load 0.81s infinite alternate ease-in-out;
       -webkit-animation: cssload-load 0.81s infinite alternate ease-in-out;
       -moz-animation: cssload-load 0.81s infinite alternate ease-in-out;
       }
       .cssload-loader::before {
       top: 0;
       }
       .cssload-loader::after {
       bottom: 0;
       }
       
       
       
       @keyframes cssload-load {
       0% {
       left: 0;
       height: 9px;
       width: 350px;
       }
       50% {
       height: 9px;
       width: 350px;
       }
       100% {
       left: 229px;
       height: 9px;
       width: 350px;
       }
       }
       
       @-o-keyframes cssload-load {
       0% {
       left: 0;
       height: 29px;
       width: 15px;
       }
       50% {
       height: 8px;
       width: 39px;
       }
       100% {
       left: 229px;
       height: 29px;
       width: 15px;
       }
       }
       
       @-ms-keyframes cssload-load {
       0% {
       left: 0;
       height: 29px;
       width: 15px;
       }
       50% {
       height: 8px;
       width: 39px;
       }
       100% {
       left: 229px;
       height: 29px;
       width: 15px;
       }
       }
       
       @-webkit-keyframes cssload-load {
       0% {
       left: 0;
       height: 29px;
       width: 15px;
       }
       50% {
       height: 8px;
       width: 39px;
       }
       100% {
       left: 229px;
       height: 29px;
       width: 15px;
       }
       }
       
       @-moz-keyframes cssload-load {
       0% {
       left: 0;
       height: 29px;
       width: 15px;
       }
       50% {
       height: 8px;
       width: 39px;
       }
       100% {
       left: 229px;
       height: 29px;
       width: 15px;
       }
       }
       "
  ))
}








