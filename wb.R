#Rscript wb.R
#read data
t<-read.table("weibo_train_data.txt",header=T,sep="\t",quote="\n") 
p<-read.table("weibo_predict_data.txt",header=T,sep="\t",quote="\n")
#t<-read.table("t.txt",header=T,sep="\t",quote="\n") 
#p<-read.table("p.txt",header=T,sep="\t",quote="\n")
#calculate mean value
tm<-aggregate(t,by=list(t$uid),FUN=mean)[,c(1,5:7)]
#assign vaule to predict data
names(tm)<-c("uid","foreward_count","comment_count","like_count")
pu<-p[,1:2]
r<-merge(pu,tm,by=c("uid"),all.x=T)
#assign 0 to new uid
#r$foreward_count[is.na(r$foreward_count)]=0
r$comment_count[is.na(r$comment_count)]=0
#r$like_count[is.na(r$like_count)]=0
#save into .txt
write.csv(r,"r.txt")
#linux \t for tabs
#system("sed -e 's#^\"[0-9]*\",##g' -e 's#\",#\t#g' -e 's#\"##g' -e '/_/d' r.txt > weibo_result.txt")
#OS X ctrl+v+tab for tabs
system("sed -e 's#^\"[0-9]*\",##g' -e 's#\",#	#g' -e 's#\"##g' -e '/_/d' r.txt > weibo_result.txt")
