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
# > write.csv(my_stats_14_11,"stats_14_11.csv",row.names = FALSE)

#4768
# my_4768<-my_4768[(nchar(my_4768$destinationUserName) <5) ,]
# my_4768_users<-unique(myfile2[(nchar(myfile2$destinationUserName)<5)&!(myfile2$destinationUserName %like% "\\$%")&(myfile2$deviceEventClassId %like% "%4768"),"destinationUserName"])
# my_4768_dst<-unique(substring(my_4768$deviceCustomString3,8))
#   df<-c(0,0)
  func_4768_file<-function(dataframe){
    df_base<-dataframe[dataframe$deviceEventClassId %like% "%4768",]
  }
  func_4768_date<-function(dataframe){
    file_dates<-unique(as.Date(substr(dataframe[,"Event.Time"],1,10),"%Y/%m/%d")) 
    }
  
 func_4768_users<-function(data_frame,dates){
   my_df<-0
   file_users<-sapply(dates,function(x){unique(data_frame[(as.Date(substr(data_frame$Event.Time,1,10)) %like% x) &(nchar(data_frame$destinationUserName)<5)&!(data_frame$destinationUserName %like% "\\$%"),"destinationUserName"])})
   for (i in 1:length(dates)){
     r2<-cbind(rep(as.character.Date(dates[i]),as.integer(length(file_users[[i]]))),file_users[[i]])
     if (my_df==0){my_df<-r2 }else{
       my_df<-rbind(my_df,r2) }
      }
   my_df<-data.frame(my_df,stringsAsFactors = FALSE)
   colnames(df_src)<-c("date","user")
 }
   
    
func_4768_src<-function(df_bs,df_usr){
  df_src<-0
  r1<-1
  for (i in 1:nrow(df_usr)){
    src<-unique(substring(df_bs[ ((as.Date(substr(df_bs[,"Event.Time"],1,10),"%Y/%m/%d") %like% df_usr[i,"date"]) &(df_bs$destinationUserName %like% df_usr[i,"user"])),"deviceCustomString3" ],8))
    if (length(src)==0){
      src<-"10"
    }
    for (j in 1:length(src)){
      if(!(src=="10")){
        r1<-cbind(df_usr[i,"date"],df_usr[i,"user"],src[j])
        }
      if (df_src==0){
        df_src<-r1 }else if(!(r1==1)) {
            df_src<-merge(df_src,r1,all=TRUE) }
    }
  }
  
  df_src<-data.frame(df_src[,1:3],stringsAsFactors = FALSE)
  colnames(df_src)<-c("date","user","src")
  df_src
  
}
    
func_4768_file_list<-function(file_list){
   file1_src<-cbind(date="2015-11-12",user="z001",src="10.0.0.0")
  for (i in 1:length(file_list)){
    file1<-read.csv(file_list[i],stringsAsFactors = FALSE)
    file1_base<-func_4768_file(file1)
    file1_dates<-func_4768_date(file1_base)
    file1_users<-func_4768_users(file1_base,file1_dates)
    file1_src_tmp<-func_4768_src(file1_base,file1_users)
    file1_src<-merge(file1_src,file1_src_tmp,all = TRUE)
    
  }
  file1_src
}
func_4768<-function(){
  library(DescTools)
    csv<-list.files(pattern= "*.csv")
    csv<-grep("10_11*",csv,value = TRUE)
    src<-func_4768_file_list(csv)
      
    src
    }

#   func_4768_user<-function(dataframe,dt,user){
#       func_4768_user<-function(dt,user,df){
#       r1<-cbind(dt,user, )
#       df<-rbind(df,r1)
# }
# my_dst<-unique(myfile[!(myfile$Destination.Service.Name  %like% "%\\$"),"Destination.Service.Name"])
# my_dst_4625<-unique(myfile[!(myfile$Device.Custom.String5  %like% "NTLM"),"Final.Source.Workstation"])
# d2<-as.Date(substr(myfile2[1,"Event.Time"],1,10),"%Y/%m/%d")
# Final.User_name = target_user_name 
# Final.Source.Workstation = if SourceHostName=NULL then SourceAddress else attackerHostName
# IP=deviceCustomString3

