func_4624_file<-function(dataframe){
  df_base<-dataframe[dataframe$deviceEventClassId %like% "%4624",]
}
func_4624_date<-function(dataframe){
  file_dates<-unique(as.Date(substr(dataframe[,"Event.Time"],1,10),"%Y/%m/%d")) 
}

func_4624_users<-function(data_frame,dates){
  my_list<-0
  file_users<-lapply(dates,function(x){unique(data_frame[(as.Date(substr(data_frame$Event.Time,1,10)) %like% x)&(nchar(data_frame$destinationUserName)<5) &!(data_frame$destinationUserName %like% "\\$%")&(data_frame$destinationUserName %like% c("t%","u%","h%","z%")),"destinationUserName"])})
  for (i in 1:length(dates)){
    if (!length(file_users[[i]])==0){
          r2<-cbind(rep(as.character.Date(dates[i]),as.integer(length(file_users[[i]]))),file_users[[i]])
          if (class(my_list)=="numeric"){my_list<-list(r2) }else{
              my_list<-c(my_list,list(r2)) }
    }
  }
  my_list
}


func_4624_src<-function(df_bs,usr_lst){
  src_lst<-0
  
  for (i in 1:length(usr_lst)){
    df_src<-0
    for (j in 1:nrow(usr_lst[[i]])){
      src<-unique(df_bs[ ((as.Date(substr(df_bs[,"Event.Time"],1,10),"%Y/%m/%d") %like% usr_lst[[i]][j,1])
                          &(df_bs$destinationUserName %like% usr_lst[[i]][j,2]))
                          &df_bs$deviceCustomString5 %like% "NTLM"
                         ,"sourceHostName" ])
      if (length(src)==0){
        src<-10
      }
      if(!(class(src)=="numeric")){
          for (k in 1:length(src)){
            r1<-0
            r1<-cbind(usr_lst[[i]][j,1],usr_lst[[i]][j,2],regmatches(src[k],regexpr("^[^\\.]+",src[k])))
            
            if (!(ncol(r1)<3)){
                       if (class(df_src)=="numeric"){
                        df_src<-r1 }else  {
                          df_src<-merge(df_src,r1,all=TRUE) }
                  
                }
            }
      }
    }

    if (!(df_src==0)){
      df_src<-data.frame(cbind((as.character(df_src[,1])),(as.character(df_src[,2])),(as.character(df_src[,3]))),stringsAsFactors = FALSE)
      colnames(df_src)<-c("date","user","src")
      if (class(src_lst)=="numeric"){
        src_lst<-list(df_src)}else {
          src_lst<-c(src_lst,list(df_src))}
    }
  }
  
  
  src_lst
  
}






func_4624_file_src<-function(file_nm){
    file1_src<-0
    file1<-read.csv(file_nm,stringsAsFactors = FALSE)
    file1_base<-func_4624_file(file1)
    if (nrow(file1_base)>0){
      file1_dates<-func_4624_date(file1_base)
      file1_users<-func_4624_users(file1_base,file1_dates)
      if(!class(file1_users)=="numeric"){
      file1_src<-func_4624_src(file1_base,file1_users)}
      
      }
    
  file1_src
}


func_4624_output<-function(src_lst,file_nm){
  trgt_files<-list.files(path="../output", pattern= "^4624_[0-9]{4}-[0-9]{2}-[0-9]{2}")
  trgt_dates<-substr(trgt_files,6,15)
  for(i in 1:length(src_lst)){
    if(!(src_lst[[i]][1,"date"] %in% trgt_dates)){
      write.csv(src_lst[[i]],paste("../output/4624_",src_lst[[i]][1,"date"],".csv",sep = ""),row.names = FALSE)
    } else {
      src11<-read.csv(paste("../output/",trgt_files[match(src_lst[[i]][1,"date"],trgt_dates)],sep=""),stringsAsFactors = FALSE)
      src11<-merge(src11,src_lst[[i]],all=TRUE) 
      write.csv(src11,paste("../output/",trgt_files[match(src_lst[[i]][1,"date"],trgt_dates)],sep=""),row.names = FALSE)
    }
  }
  file.rename(file_nm,paste("../processed/",file_nm,sep=""))
}

func_4624<-function(input_filter){
  library(DescTools)
  csv<-list.files(pattern= "*.csv")
  csv<-grep(input_filter,csv,value = TRUE)
  for (i in 1:length(csv)){
      src<-func_4624_file_src(csv[i])
      if (!(class(src)=="numeric")){
        func_4624_output(src,csv[i])}
  }
}