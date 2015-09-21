#Rscript wb.R
print(Sys.time())
#argv <- commandArgs(TRUE)
#isReal <- as.numeric(argv[1])
#clustNum <- as.numeric(argv[3])
isReal <- 1
cat("isReal",isReal,"\n")
cat("> read data \n")
print(Sys.time())
if(isReal)
{
    t<-read.csv(paste("~/file/weibo/weibo_train_data.txt",sep=""),sep="\t",quote="",comment="") 
    p<-read.csv(paste("~/file/weibo/weibo_predict_data.txt",sep=""),sep="\t",quote="",comment="")
    colnames(t)<-c("uid","mid","time","foreward_count","comment_count","like_count","content")
    colnames(p)<-c("uid","mid","time","content")
}
#else
#{
    #t<-read.csv(paste("~/file/weibo/sub/test/t",subNum,".txt",sep=""),sep="\t",quote="",comment="") 
    #p<-read.csv(paste("~/file/weibo/sub/test/p",subNum,".txt",sep=""),sep="\t",quote="",comment="")
    #colnames(t)<-c("uid","mid","time","foreward_count","comment_count","like_count","content")
    #colnames(p)<-c("uid","mid","time","foreward_count","comment_count","like_count","content")
#}
cat("> group by uid \n")
print(Sys.time())
tt=rbind(data.frame(uid=t$uid,mid=t$mid,time=t$time,content=t$content),data.frame(uid=p$uid,mid=p$mid,time=p$time,content=p$content))
thr=15000
tp<-split(tt,f=as.factor(tt$uid))
p1=NULL
c1=0
subNum=0
cat("> save into files \n")
print(Sys.time())
for(tpi in 1:NROW(tp))
{
    n=NROW(tp[tpi][[1]])
    u=tp[tpi][[1]]$uid[1]
    if(c1<thr)
    {
        p1=c(p1,as.character(u))
        c1=c1+n
    }else if(c1>=thr || tpi==NROW(tp))
    {
        tpart1=t[t$uid %in% p1,]
        ppart1=p[p$uid %in% p1,]
        subNum=subNum+1
        write.table(tpart1,paste("~/file/weibo/sub/real/td",subNum,".txt",sep=""),sep="\t",quote=F,row.names=F)
        write.table(ppart1,paste("~/file/weibo/sub/real/pd",subNum,".txt",sep=""),sep="\t",quote=F,row.names=F)
        p1=NULL
        c1=0
        cat("subNum : ",subNum," ",NROW(tpart1)," ",NROW(ppart1)," ")
        print(Sys.time())
    }
}

