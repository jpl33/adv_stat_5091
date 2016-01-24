#read.deptNm <- function(filename) {
#  dNm<-gsub('[(boi_unit)|(.csv)]','',filename)
#  myfile<-read.csv(filename,header=FALSE,skip=3,stringsAsFactors = FALSE)
#  myfile[,"divNm"]<-dNm
#  raw<-myfile[1:nrow(myfile)-1,]
#  write.table(raw, "div_accounts.csv", col.names=FALSE, sep=",", append=TRUE)
# }

#mylist<-list.files(pattern="^boi_unit")
#data <- lapply(mylist, read.deptNm)
###myfile<-read.csv("div_accounts_hash.csv",colClasses=c("Acct"="integer"),stringsAsFactors = FALSE) 
# divNms<-unique(myfile[,2])

# mkRelAcct<-function(i,filename){
#    for (i in 1:length(divNms))  
#    {
#    div<-subset(myfile,myfile[,3] %in% divNms[[i]])
#    maxAcct<-max(div[,4])+1
#    myfile[myfile[,2] %in% div[,2],"relAcct"]<-myfile[myfile[,2] %in% div[,2],4]/maxAcct
#    write.csv(myfile,"div_accounts.csv")  
#     }
# }
#lapply(seq_along(divNms), mkRelAcct,filename=myfile)

#>hash_user<-lapply(seq_along(myfile[,2]),function(x){digest(myfile[x,2],algo="sha256")})
#> trunc_hash_user<-lapply(hash_user,function(x){substr(x,1,5)})
#> uniq<-unique(trunc_hash_user)
#if (length(uniq)==length(hash_user))


#myfile3[,2]<-unlist(replace(myfile3[,2],1:7,fake_user_3))
#  fake_user2<-lapply(seq_along(fake_user2),function(x){fake_user2[[x]]<-trunc_hash_user[[x]]})
#myfile<-myfile[,c("user","div","Acct","relAcct")]
# write.csv(myfile,"div_accounts_hash.csv",row.names = FALSE)
#div_beta<-function(x)
  {
   
  #dd<-fitdistr(myfile[myfile[,2]%in%divNms[x],4],"beta",list(shape1=3,shape2=2))
  # pdf(paste("beta_plot_",div_names[x],".pdf",sep=""))
  #pd<-plotdist(myfile[myfile[,2]%in%divNms[x],4],"beta",list(shape1=dd$estimate[[1]],shape2=dd$estimate[[2]]))
  #dev.off() 
  #dd
}

# band<-function (a,x){a-qbeta(pbeta(a,betas[,x]$estimate[[1]],betas[,x]$estimate[[2]])-0.75,betas[,x]$estimate[[1]],betas[,x]$estimate[[2]])}
# result<-optimize(band,c(0.995,0.005))
#betas<-sapply(seq_along(divNms),function(x){result<-optimize(band,c(0.995,0.005),x)})


#   GGVIS
#> dd<-subset(myfile,myfile[,3] %in% "B")
#> p<-ggvis(dd,x=~relAcct)
#subset(myfile,myfile[,3] %in% "C")%>%ggvis(x=~relAcct)%>%layer_histograms()

#   GPPLOT2
#dd<-subset(myfile,myfile[,3] %in% "B")
#ggplot(data=dd,aes(x=dd$relAcct)) + geom_histogram()
  
#ggplot(data=myfile,aes(x=relAcct)) + geom_histogram() +facet_wrap(~div, scales="free_y")


#  myfile<-read.csv("14_11_07_47_to_14_11_17_47.csv",stringsAsFactors = FALSE)
#  my_users_raw<-unique(myfile[,"Final.User.Name"])
#  my_users<-unique(myfile[nchar(myfile$Final.User.Name)<5,"Final.User.Name"])
# user_count<-sapply(seq_along(my_users),function(x){count(myfile[myfile$Final.User.Name %like% my_users[x],] ) } )
# my_stats_14_11<-data_frame(my_users,user_count)
# my_stats_14_11 <- data.frame(lapply(my_stats_14_11, as.character), stringsAsFactors=FALSE)
#  write.csv(my_stats_14_11,"stats_14_11.csv",row.names = FALSE)

#4768
# my_4768<-my_4768[(nchar(my_4768$destinationUserName) <5) ,]
# my_4768_users<-unique(myfile2[(nchar(myfile2$destinationUserName)<5)&!(myfile2$destinationUserName %like% "\\$%")&(myfile2$deviceEventClassId %like% "%4768"),"destinationUserName"])
# my_4768_dst<-unique(substring(my_4768$deviceCustomString3,8))
#   df<-c(0,0)
#   func_4768_file<-function(dataframe){
#     df_base<-dataframe[dataframe$deviceEventClassId %like% "%4768",]
#   }
#   func_4768_date<-function(dataframe){
#     file_dates<-unique(as.Date(substr(dataframe[,"Event.Time"],1,10),"%Y/%m/%d")) 
#     }
#   
#  func_4768_users<-function(data_frame,dates){
#    my_list<-0
#    file_users<-sapply(dates,function(x){unique(data_frame[(as.Date(substr(data_frame$Event.Time,1,10)) %like% x)&(nchar(data_frame$destinationUserName)<5) &!(data_frame$destinationUserName %like% "\\$%")&(data_frame$destinationUserName %like% c("t%","u%","h%","z%")),"destinationUserName"])})
#    for (i in 1:length(dates)){
#        if (!length(file_users[[i]])==0){
#          r2<-cbind(rep(as.character.Date(dates[i]),as.integer(length(file_users[[i]]))),file_users[[i]])
#              if (class(my_list)=="numeric"){my_list<-list(r2) }else{
#                my_list<-c(my_list,list(r2)) }
#        }
#    }
#    my_list
#  }
#    
#     
# func_4768_src<-function(df_bs,usr_lst){
#   src_lst<-0
#   
#   r1<-1
#   for (i in 1:length(usr_lst)){
#         df_src<-0
#         for (j in 1:nrow(usr_lst[[i]])){
#         src<-unique(substring(df_bs[ ((as.Date(substr(df_bs[,"Event.Time"],1,10),"%Y/%m/%d") %like% usr_lst[[i]][j,1]) &(df_bs$destinationUserName %like% usr_lst[[i]][j,2])),"deviceCustomString3" ],8))
#         if (length(src)==0){
#           src<-10
#         }
#             for (k in 1:length(src)){
#               if(!(class(src)=="numeric")){
#                 r1<-cbind(usr_lst[[i]][j,1],usr_lst[[i]][j,2],src[k])
#                 }
#               if (class(df_src)=="numeric"){
#                 df_src<-r1 }else  {
#                     df_src<-merge(df_src,r1,all=TRUE) }
#             }
#         }
#   
#     df_src<-data.frame(cbind((as.character(df_src[,1])),(as.character(df_src[,2])),(as.character(df_src[,3]))),stringsAsFactors = FALSE)
#     colnames(df_src)<-c("date","user","src")
#     if (class(src_lst)=="numeric"){
#       src_lst<-list(df_src)}else {
#         src_lst<-c(src_lst,list(df_src))}
#   }
#   
#   
#  src_lst
#   
# }
# 
# 
# func_4768_list_merge<-function(list1,list2){
#     
#     for (i in length(list2):1){
#           for(j in 1:length(list1)){
#             if (!(class(list2[[i]])=="numeric")){ 
#                 if (list2[[i]][1,"date"]==list1[[j]][1,"date"]) {
#                    list1[[j]]<-merge(list1[[j]],list2[[i]],all=TRUE)
#                    list2[[i]]<-0}
#                   }
#                }
#           }
#   list1
#       }  
#     
#    
# 
# 
# func_4768_file_list<-function(file_list){
#    file1_src<-0
#   for (i in 1:length(file_list)){
#     file1<-read.csv(file_list[i],stringsAsFactors = FALSE)
#     file1_base<-func_4768_file(file1)
#     if (nrow(file1_base)>0){
#     file1_dates<-func_4768_date(file1_base)
#     file1_users<-func_4768_users(file1_base,file1_dates)
#     file1_src_tmp<-func_4768_src(file1_base,file1_users)
#     if (class(file1_src)=="numeric"){
#       file1_src<-file1_src_tmp}else {
#         file1_src<-func_4768_list_merge(file1_src,file1_src_tmp)}
#     
#        }
#  file.rename(file_list[i],paste("./processed/",file_list[i],sep=""))
#     }
#   file1_src
# }
# 
# func_4768<-function(input_filter){
#   library(DescTools)
#     csv<-list.files(pattern= "*.csv")
#     csv<-grep(input_filter,csv,value = TRUE)
#     src<-func_4768_file_list(csv)
#     trgt_files<-list.files(path="./output", pattern= "^4768_[0-9]{4}-[0-9]{2}-[0-9]{2}")
#     trgt_dates<-substr(trgt_files,6,15)
#     for(i in 1:length(src)){
#       if(!(src[[i]][1,"date"] %in% trgt_dates)){
#           write.csv(src[[i]],paste("./output/4768_",src[[i]][1,"date"],".csv",sep = ""),row.names = FALSE)
#       } else {
#           src11<-read.csv(paste("./output/",trgt_files[match(src[[i]][1,"date"],trgt_dates)],sep=""),stringsAsFactors = FALSE)
#           src11<-merge(src11,src[[i]],all=TRUE) 
#           write.csv(src11,paste("./output/",trgt_files[match(src[[i]][1,"date"],trgt_dates)],sep=""),row.names = FALSE)
#        }
#     }
#     
#     src
# }
 split_files<-function(pattern){
   library(NCmisc)
  #csv<-list.files(pattern= "*.csv")
 csv<-list.files(pattern= pattern)
 csv_srt<-sapply(csv,function(x){file.split(x,size=100000)})
 input_filter<-"part[0-9]{1,2}.csv$"
 file_list<-list.files(pattern= input_filter)
 first_file<-file_list[1]
 column_names<-read.csv(first_file,nrows = 1)
 header_line<-function(x){
   write.csv(column_names,"column_names.csv",row.names = FALSE)
   file.append("column_names.csv",x)
   file.rename("column_names.csv",x)}
 sapply(file_list,header_line)
 input_filter_old<-"^[0-9]\\w+[0-9]{2}.csv$"
 file_list_old<-list.files(pattern= input_filter_old)
 l1<-paste(file_list_old,collapse = " ")
 c1<-paste('"C:\\Program Files\\WinRAR\\rar.exe\"',"a","old_files.rar",l1)
 shell(c1)
 }
 
#  file 15_11_13_47_to_16_11_01_47.csv  size: 506 MB
#  split_files: 63.8 Second
#  func_4768: 55 Second
#  total : 119 Second
#   func_4768 whole file:  161 seconds
 
# func_4625_file<-function(dataframe){
#   df_base<-dataframe[dataframe$deviceEventClassId %like% "%4625",]
# }
# func_4625_date<-function(dataframe){
#   file_dates<-unique(as.Date(substr(dataframe[,"Event.Time"],1,10),"%Y/%m/%d")) 
# }
# 
# func_4625_users<-function(data_frame,dates){
#   my_list<-0
#   file_users<-sapply(dates,function(x){unique(data_frame[(as.Date(substr(data_frame$Event.Time,1,10)) %like% x)&(nchar(data_frame$destinationUserName)<5) &!(data_frame$destinationUserName %like% "\\$%")&(data_frame$destinationUserName %like% c("t%","u%","h%","z%")),"destinationUserName"])})
#   for (i in 1:length(dates)){
#     if (!length(file_users[[i]])==0){
#           r2<-cbind(rep(as.character.Date(dates[i]),as.integer(length(file_users[[i]]))),file_users[[i]])
#           if (class(my_list)=="numeric"){my_list<-list(r2) }else{
#               my_list<-c(my_list,list(r2)) }
#     }
#   }
#   my_list
# }
# 
# 
# func_4625_src<-function(df_bs,usr_lst){
#   src_lst<-0
#   
#   r1<-1
#   for (i in 1:length(usr_lst)){
#     df_src<-0
#     for (j in 1:nrow(usr_lst[[i]])){
#       src<-unique(df_bs[ ((as.Date(substr(df_bs[,"Event.Time"],1,10),"%Y/%m/%d") %like% usr_lst[[i]][j,1]) &(df_bs$destinationUserName %like% usr_lst[[i]][j,2])),"sourceAddress" ])
#       if (length(src)==0){
#         src<-10
#       }
#       for (k in 1:length(src)){
#         if(!(class(src)=="numeric")){
#           r1<-cbind(usr_lst[[i]][j,1],usr_lst[[i]][j,2],src[k])
#         }
#         if (class(df_src)=="numeric"){
#           df_src<-r1 }else  {
#             df_src<-merge(df_src,r1,all=TRUE) }
#       }
#     }
#     
#     df_src<-data.frame(cbind((as.character(df_src[,1])),(as.character(df_src[,2])),(as.character(df_src[,3]))),stringsAsFactors = FALSE)
#     colnames(df_src)<-c("date","user","src")
#     if (class(src_lst)=="numeric"){
#       src_lst<-list(df_src)}else {
#         src_lst<-c(src_lst,list(df_src))}
#   }
#   
#   
#   src_lst
#   
# }
# 
# 
# func_4625_list_merge<-function(list1,list2){
#   
#   for (i in length(list2):1){
#     for(j in 1:length(list1)){
#       if (!(class(list2[[i]])=="numeric")){ 
#         if (list2[[i]][1,"date"]==list1[[j]][1,"date"]) {
#           list1[[j]]<-merge(list1[[j]],list2[[i]],all=TRUE)
#           list2[[i]]<-0}
#       }
#     }
#   }
#   list1
# }  
# 
# 
# 
# 
# func_4625_file_list<-function(file_list){
#   file1_src<-0
#   for (i in 1:length(file_list)){
#     file1<-read.csv(file_list[i],stringsAsFactors = FALSE)
#     file1_base<-func_4625_file(file1)
#     if (nrow(file1_base)>0){
#       file1_dates<-func_4625_date(file1_base)
#       file1_users<-func_4625_users(file1_base,file1_dates)
#       file1_src_tmp<-func_4625_src(file1_base,file1_users)
#       if (class(file1_src)=="numeric"){
#         file1_src<-file1_src_tmp}else {
#           file1_src<-func_4625_list_merge(file1_src,file1_src_tmp)}
#       
#     }
#     file.rename(file_list[i],paste("./processed/",file_list[i],sep=""))
#   }
#   file1_src
# }
# 
# func_4625<-function(input_filter){
#   library(DescTools)
#   csv<-list.files(pattern= "*.csv")
#   csv<-grep(input_filter,csv,value = TRUE)
#   src<-func_4625_file_list(csv)
#   trgt_files<-list.files(path="./output", pattern= "^4625_[0-9]{4}-[0-9]{2}-[0-9]{2}")
#   trgt_dates<-substr(trgt_files,6,15)
#   for(i in 1:length(src)){
#     if(!(src[[i]][1,"date"] %in% trgt_dates)){
#       write.csv(src[[i]],paste("./output/4625_",src[[i]][1,"date"],".csv",sep = ""),row.names = FALSE)
#     } else {
#       src11<-read.csv(paste("./output/",trgt_files[match(src[[i]][1,"date"],trgt_dates)],sep=""),stringsAsFactors = FALSE)
#       src11<-merge(src11,src[[i]],all=TRUE) 
#       write.csv(src11,paste("./output/",trgt_files[match(src[[i]][1,"date"],trgt_dates)],sep=""),row.names = FALSE)
#     }
#   }
#   
#   src
# }

# un_processed<-function(){
#   flst<-list.files(pattern= "[0-9_]{12}.csv$")
#   lapply(flst,function(x){file.rename(x,substr(x,11,40))})
# }
   
# my_dst<-unique(myfile[!(myfile$Destination.Service.Name  %like% "%\\$"),"Destination.Service.Name"])
# my_dst_4625<-unique(myfile[!(myfile$Device.Custom.String5  %like% "NTLM"),"Final.Source.Workstation"])
# d2<-as.Date(substr(myfile2[1,"Event.Time"],1,10),"%Y/%m/%d")
# Final.User_name = target_user_name 
# Final.Source.Workstation = if SourceHostName=NULL then SourceAddress else attackerHostName
# IP=deviceCustomString3

