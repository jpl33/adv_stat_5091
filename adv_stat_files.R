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
file.remove(l1)
 }
 


   

# my_dst_4625<-unique(myfile[!(myfile$Device.Custom.String5  %like% "NTLM"),"Final.Source.Workstation"])
# d2<-as.Date(substr(myfile2[1,"Event.Time"],1,10),"%Y/%m/%d")
# Final.User_name = target_user_name 
# Final.Source.Workstation = if SourceHostName=NULL then SourceAddress else attackerHostName
# IP=deviceCustomString3

