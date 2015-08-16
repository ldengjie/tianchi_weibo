#t<-read.csv("weibo_train_data.txt",header=T,sep="\t",quote="\n") 
#p<-read.csv("weibo_predict_data.txt",header=T,sep="\t",quote="\n")
#load("RDataTotal")

#t<-read.csv("train.txt",header=T,sep="\t",quote="\n") 
#p<-read.csv("predict.txt",header=T,sep="\t",quote="\n")
load("RDataTest")
#names(t)=c("uid","mid","time","foreward_count","comment_count","like_count","content")
#names(p)=c("uid","mid","time","foreward_count","comment_count","like_count","content")

#1
izero=function(x)
{
    if(is.factor(x))
    {
        result=NA
    }else
    {
        result=0
    }
    return(result)
}
#2 3.1
rmOutlier=function(x)
{
    if(is.factor(x))
    {
        result=NA
    }else
    {
        xMorethanZero=x
        xOutlierx=boxplot.stats(xMorethanZero)$out
        result=as.integer((sum(x)-sum(xOutlierx))/(NROW(x)-NROW(xOutlierx)))
    }
    return(result)
}
#3 3.2
rmOutlier2=function(x)
{
    if(is.factor(x))
    {
        result=NA
    }else
    {
        xMorethanZero=x
        xOutlierx=boxplot.stats(xMorethanZero)$out
        result=as.integer((sum(x)-sum(xOutlierx)+boxplot.stats(xMorethanZero)$stats[5]*NROW(xOutlierx))/NROW(x))
    }
    return(result)
}
#3 3.3
rmOutlier3=function(x)
{
    if(is.factor(x))
    {
        result=NA
    }else
    {
        xMorethanZero=x
        xOutlierx=boxplot.stats(xMorethanZero)$out
        result=as.integer((sum(x)-sum(xOutlierx)+boxplot.stats(xMorethanZero)$stats[5]*NROW(xOutlierx))/NROW(x)+0.085)
    }
    return(result)
}
#6
imean=function(x)
{
    if(is.factor(x))
    {
        result=NA
    }else
    {
        result=as.integer(mean(x))
    }
    return(result)
}

funl=c(izero,rmOutlier,rmOutlier2,rmOutlier3,imean)

for(fi in 4:4)
{
    tm<-aggregate(t,by=list(t$uid),FUN=funl[[fi]])[,c(1,5:7)]

    names(tm)<-c("uid","foreward_count","comment_count","like_count")
    pu<-p[,1:2]
    r<-merge(pu,tm,by=c("uid"),all.x=T)

    #cat("> assign 0 to new uid \n")
    r$foreward_count[is.na(r$foreward_count)]=0
    r$comment_count[is.na(r$comment_count)]=0
    r$like_count[is.na(r$like_count)]=0

    #save into .txt
    #write.csv(r,"r.txt")
    write.table(r,"testr3.txt",sep="\t",row.names=F,quote=F)
    #linux \t for tabs
    #system("sed -e 's#^\"[0-9]*\",##g' -e 's#\",#\t#g' -e 's#\"##g' -e '/_/d' r.txt > weibo_result.txt")
    #OS X ctrl+v+tab for tabs
    #system("sed -e 's#^\"[0-9]*\",##g' -e 's#\",#	#g' -e 's#\"##g' -e '/_/d' r.txt > weibo_result.txt")

    if(1)
    {
        rp<-merge(p,r,by=c("uid","mid"))
        devf=abs(rp$foreward_count.y-rp$foreward_count.x)/(rp$foreward_count.x+5)
        devc=abs(rp$comment_count.y-rp$comment_count.x)/(rp$comment_count.x+3)
        devl=abs(rp$like_count.y-rp$like_count.x)/(rp$like_count.x+3)
        prei=1-0.5*devf-0.25*devc-0.25*devl
        pret=prei
        prei[(prei-0.8)>0]=1
        prei[(prei-0.8)<=0]=0
        count=(rp$foreward_count.x+rp$comment_count.x+rp$like_count.x+1)
        count[count>100]=100
        pre=sum(count*prei)/sum(count)
        cat(">>> precision = ",pre,"\n")
    }
}
