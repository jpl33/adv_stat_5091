func_4769_file<-function(dataframe){
  df_base<-dataframe[dataframe$deviceEventClassId %like% "%4769",]
}
func_4769_date<-function(dataframe){
  file_dates<-unique(as.Date(substr(dataframe[,"Event.Time"],1,10),"%Y/%m/%d")) 
}

func_4769_users<-function(data_frame,dates){
  my_list<-0
  user_function<-function(x){
    user_vector<-data_frame[(as.Date(substr(data_frame$Event.Time,1,10)) %like% x)
                            &(nchar(regmatches(data_frame$destinationUserName,regexpr("^[^@]+",data_frame$destinationUserName)))<5)
                            &(data_frame$destinationUserName %like% c("t%","u%","h%","z%")),"destinationUserName"]
    final_user_vector<-unique(regmatches(user_vector,regexpr("^[^@]+",user_vector)))
    fv<-final_user_vector[nchar(final_user_vector)<5]
  }
  
  file_users<-lapply(dates,user_function)
  for (i in 1:length(dates)){
    if (!length(file_users[[i]])==0){
          r2<-cbind(rep(as.character.Date(dates[i]),as.integer(length(file_users[[i]]))),file_users[[i]])
          if (class(my_list)=="numeric"){my_list<-list(r2) }else{
              my_list<-c(my_list,list(r2)) }
    }
  }
  my_list
}


func_4769_src<-function(df_bs,usr_lst){
  src_lst<-0
  r1<-1
  uv<-regmatches(df_bs$destinationUserName,regexpr("^[^@]+",df_bs$destinationUserName))
  for (i in 1:length(usr_lst)){
    df_src<-0
    for (j in 1:nrow(usr_lst[[i]])){
      src<-unique(df_bs[ ((as.Date(substr(df_bs[,"Event.Time"],1,10),"%Y/%m/%d") %like% usr_lst[[i]][j,1]) &
                            (df_bs$destinationUserName %like% paste0(usr_lst[[i]][j,2],"%"))&
                            df_bs$destinationServiceName %like% "%\\$"),c("destinationServiceName" ,"deviceCustomString3")])
      if (length(src)==0){
        src<-10
      }
      for (k in 1:nrow(src)){
        if(!(class(src)=="numeric")){
          r1<-cbind(usr_lst[[i]][j,1],usr_lst[[i]][j,2],src[k,1],substring(src[k,2],8))
        }
        if (class(df_src)=="numeric"){
          df_src<-r1 }else  {
            df_src<-merge(df_src,r1,all=TRUE) }
      }
    }
    
    df_src<-data.frame(cbind((as.character(df_src[,1])),(as.character(df_src[,2])),(as.character(df_src[,3])),(as.character(df_src[,4]))),stringsAsFactors = FALSE)
    colnames(df_src)<-c("date","user","dst","src")
    if (class(src_lst)=="numeric"){
      src_lst<-list(df_src)}else {
        src_lst<-c(src_lst,list(df_src))}
  }
  
  
  src_lst
  
}


func_4769_list_merge<-function(list1,list2){
  
  for (i in length(list2):1){
    for(j in 1:length(list1)){
      if (!(class(list2[[i]])=="numeric")){ 
        if (list2[[i]][1,"date"]==list1[[j]][1,"date"]) {
          list1[[j]]<-merge(list1[[j]],list2[[i]],all=TRUE)
          list2[[i]]<-0}
      }
    }
  }
  list1
}  




func_4769_file_list<-function(file_list){
    file1_src<-0
  for (i in 1:length(file_list)){
    file1<-read.csv(file_list[i],stringsAsFactors = FALSE)
    file1_base<-func_4769_file(file1)
    if (nrow(file1_base)>0){
      file1_dates<-func_4769_date(file1_base)
      file1_users<-func_4769_users(file1_base,file1_dates)
      file1_src_tmp<-func_4769_src(file1_base,file1_users)
      if (class(file1_src)=="numeric"){
        file1_src<-file1_src_tmp}else {
          file1_src<-func_4769_list_merge(file1_src,file1_src_tmp)}
      
    }
    file.rename(file_list[i],paste("../processed/",file_list[i],sep=""))
  }
  file1_src
}

func_4769<-function(input_filter){
  library(DescTools)
  csv<-list.files(pattern= "*.csv")
  csv<-grep(input_filter,csv,value = TRUE)
  src<-func_4769_file_list(csv)
  trgt_files<-list.files(path="../output", pattern= "^4769_[0-9]{4}-[0-9]{2}-[0-9]{2}")
  trgt_dates<-substr(trgt_files,6,15)
  for(i in 1:length(src)){
    if(!(src[[i]][1,"date"] %in% trgt_dates)){
      write.csv(src[[i]],paste("../output/4769_",src[[i]][1,"date"],".csv",sep = ""),row.names = FALSE)
    } else {
      src11<-read.csv(paste("../output/",trgt_files[match(src[[i]][1,"date"],trgt_dates)],sep=""),stringsAsFactors = FALSE)
      src11<-merge(src11,src[[i]],all=TRUE) 
      write.csv(src11,paste("../output/",trgt_files[match(src[[i]][1,"date"],trgt_dates)],sep=""),row.names = FALSE)
    }
  }
  
  src
}