#Rscript wb.R
#print(Sys.time())
#cat("> read data \n")

#t<-read.csv("weibo_train_data.txt",header=T,sep="\t",quote="\n") 
#p<-read.csv("weibo_predict_data.txt",header=T,sep="\t",quote="\n")
#load("RDataTotal")

#t<-read.csv("train.txt",header=T,sep="\t",quote="\n") 
#p<-read.csv("predict.txt",header=T,sep="\t",quote="\n")
#load("RDataTest")

#t<-read.csv("t.txt",sep="\t",quote="") 
#p<-read.csv("p.txt",sep="\t",quote="")
#names(t)=c("uid","mid","time","foreward_count","comment_count","like_count","content")
#names(p)=c("uid","mid","time","foreward_count","comment_count","like_count","content")

#p=p[p$uid=="82990720c0936340c7a17e712f549b30",]
#t=t[t$uid=="82990720c0936340c7a17e712f549b30",]
#p=p[p$uid=="3df25e570db062bab9cbf0782cf09630",]
#t=t[t$uid=="3df25e570db062bab9cbf0782cf09630",]

load("cor.RData")

#print(Sys.time())
#cat("> calculate mean value \n")

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
#2 4.1
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
#3 4.2
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
#4 3.1
rmOutlierMorethanZero=function(x)
{
    if(is.factor(x))
    {
        result=NA
    }else
    {
        xMorethanZero=x[x>0]
        xOutlierx=boxplot.stats(xMorethanZero)$out
        result=as.integer((sum(x)-sum(xOutlierx))/(NROW(x)-NROW(xOutlierx)))
    }
    return(result)
}
#5 3.2
rmOutlierMorethanZero2=function(x)
{
    if(is.factor(x))
    {
        result=NA
    }else
    {
        xMorethanZero=x[x>0]
        xOutlierx=boxplot.stats(xMorethanZero)$out
        result=as.integer((sum(x)-sum(xOutlierx)+boxplot.stats(xMorethanZero)$stats[5]*NROW(xOutlierx))/NROW(x))
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
#7
auto=function(x)
{
    if(is.factor(x))
    {
        result=NA
    }else
    {
        if(mean(x)>5)
        {
            result=as.integer(mean(x))
        }else
        {
            xOutlierx=boxplot.stats(x)$out
            result=as.integer((sum(x)-sum(xOutlierx)+boxplot.stats(x)$stats[5]*NROW(xOutlierx))/NROW(x))
        }
    }
    return(result)
}
#8
test=function(x)
{
    if(is.factor(x))
    {
        result=NA
    }else
    {
        result=rep(0,7)
        wk=c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
        for(wi in 1:7)
        {
            #as.integer((sum(l1)-sum(l1out))/(NROW(l1)-NROW(l1out)))
            f1=tu$foreward_count[ weekdays(as.Date(tu$time))==wk[wi]]
            f1out=boxplot.stats(f1)$out
            if(NROW(f1)>0) foreward_mean[wi]=as.integer((sum(f1)-sum(f1out)+boxplot.stats(f1)$stats[5]*NROW(f1out))/NROW(f1))
        }
    }
    return(result)
}


if(1)
{
    print(Sys.time())
    library("stringr")
    library("cluster")
    library("mclust")
    if(1)
    {
        library(rJava)
        library(Rwordseg)
        cat(">> segment ")
        print(Sys.time())
        #doc=c(as.character(t$content),as.character(p$content)) 
        #doc=gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","",doc)                            
        #tag=str_extract(doc,"#.+?#") 
        #tag=na.omit(tag)  #去除NA
        #tag=unique(tag)    #去重
        #if(length(tag)>0) insertWords(tag)
        ##docSeg=NULL
        ##for(si in 1:(NROW(doc)%/%10000))
        ##{
            ##cat(">>> ",si,"/",(NROW(doc)%/%10000))
            ##print(Sys.time())
            ##docSeg[si]=segmentCN(doc[((si-1)*10000+1):(si*10000)])
        ##}
        #docSeg=segmentCN(doc)
        ##detach("package:Rwordseg", unload=TRUE)

    }
    library("tm")
    cat(">> corpus ")
    #print(Sys.time())
    #docCor=Corpus(VectorSource(docSeg))
    # remove numbers
    #docCor=tm_map(docCor, removeNumbers)
    cat(">> stopWord ")
    print(Sys.time())
    # remove stop words
    #stw=read.table(file="dict/stopWord.txt",quote="",colClasses="character")
    #docCor=tm_map(docCor,tm::removeWords,stw[,1])
    cat(">> tdm ")
    print(Sys.time())
    ctl=list(removePunctuation=T,minDocFreq=5,wordLengths = c(1, Inf),weighting = weightTfIdf)
    docTdm=TermDocumentMatrix(docCor,control=ctl)
    #length(docTdm$dimnames$Terms)
    #tdm_removed=removeSparseTerms(docTdm, 0.9998) # 1-去除了低于 99.98% 的稀疏条目项
    #length(tdm_removed$dimnames$Terms)
    #time week features
    dw=c(weekdays(as.Date(t$time)),weekdays(as.Date(p$time)))
    #dw=weekdays(as.Date(data$time))
    #Mon=rep(0,NROW(dw))
    #Tue=rep(0,NROW(dw))
    #Wed=rep(0,NROW(dw))
    #Thu=rep(0,NROW(dw))
    #Fri=rep(0,NROW(dw))
    #Sat=rep(0,NROW(dw))
    #Sun=rep(0,NROW(dw))
    #Mon[dw=="Monday"]=1
    #Tue[dw=="Tuesday"]=1
    #Wed[dw=="Wednesday"]=1
    #Thu[dw=="Thursday"]=1
    #Fri[dw=="Friday"]=1
    #Sat[dw=="Saturday"]=1
    #Sun[dw=="Sunday"]=1

    cat(">> clust ")
    print(Sys.time())
    #clust
    ctNum=10
    m=as.matrix(docTdm)
    #m=rbind(m, Monday=Mon, Tuesday=Tue, Wednesday=Wed, Thursday=Thu, Friday=Fri, Saturday=Sat, Sunday=Sun)
    dm=t(m)
    #HC
    dist_tdm <- proxy::dist(dm, method = 'cosine')
    hc <- hclust(dist_tdm, method = 'mcquitty')
    ct = cutree(hc,k=ctNum)
    ##"k-Means"
    #k<-kmeans(dm,ctNum)
    #ct=k$cluster
    ##k-Medoids
    #pp<-pam(dm,ctNum)
    #ct=pp$clustering
    #EM
    #fm<-Mclust(dm,ctNum)
    #ct=fm$class
    #doc=cbind(doc,clust=ct,fm=rep(NA,NROW(ct),cm=rep(NA,NROW(ct),lm=rep(NA,NROW(ct))
    tc=cbind(clust=ct[1:NROW(t)],t)
    pc=cbind(clust=ct[(1+NROW(t)):NROW(doc)],p)
    #8
    cat(">> split ")
    print(Sys.time())
    tp<-split(tc,f=as.factor(tc$uid))
    tcm=NULL
    for(tpi in 1:NROW(tp))
    {
        tu=tp[tpi][[1]]
        if(NROW(tu)==0) next
        tu2=tu
        cat(">>>>>> ",tpi,"\n")
        foreward_mean=matrix(rep(0,ctNum*7),ctNum,7)
        comment_mean=matrix(rep(0,ctNum*7),ctNum,7)
        like_mean=matrix(rep(0,ctNum*7),ctNum,7)
        for(cti in 1:ctNum)
        {
            #foreward_mean[cti]=as.integer(mean(na.omit(tu$foreward_count[tu$clust==cti])))
            #f1=tu$foreward_count[tu$clust==cti]
            #f1out=boxplot.stats(f1)$out
            #foreward_mean[cti]=as.integer((sum(f1)-sum(f1out))/(NROW(f1)-NROW(f1out)))
            #comment_mean[cti]=as.integer(mean(na.omit(tu$comment_count[tu$clust==cti])))
            #c1=tu$comment_count[tu$clust==cti]
            #cOutlier=boxplot.stats(c1)$out
            #comment_mean[cti]=as.integer((sum(c1)-sum(cOutlier))/(NROW(c1)-NROW(cOutlier)))
            #like_mean[cti]=as.integer(mean(na.omit(tu$like_count[tu$clust==cti])))
            #l1=tu$like_count[tu$clust==cti]
            #lOutlier=boxplot.stats(l1)$out
            #like_mean[cti]=as.integer((sum(l1)-sum(lOutlier))/(NROW(l1)-NROW(lOutlier)))
            #cat(foreward_mean[cti],comment_mean[cti],like_mean[cti],"\n")
            wk=c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
            for(wi in 1:7)
            {
                #as.integer((sum(l1)-sum(l1out))/(NROW(l1)-NROW(l1out)))
                f1=tu$foreward_count[((tu$clust==cti) + (weekdays(as.Date(tu$time))==wk[wi]))==2]
                f1out=boxplot.stats(f1)$out
                if(NROW(f1)>0) foreward_mean[cti,wi]=as.integer((sum(f1)-sum(f1out)+boxplot.stats(f1)$stats[5]*NROW(f1out))/NROW(f1))

                c1=tu$comment_count[((tu$clust==cti) + (weekdays(as.Date(tu$time))==wk[wi]))==2]
                c1out=boxplot.stats(c1)$out
                if(NROW(c1)>0) comment_mean[cti,wi]=as.integer((sum(c1)-sum(c1out)+boxplot.stats(c1)$stats[5]*NROW(c1out))/NROW(c1))

                l1=tu$like_count[((tu$clust==cti) + (weekdays(as.Date(tu$time))==wk[wi]))==2]
                l1out=boxplot.stats(l1)$out
                if(NROW(l1)>0) like_mean[cti,wi]=as.integer((sum(l1)-sum(l1out)+boxplot.stats(l1)$stats[5]*NROW(l1out))/NROW(l1))
            }
        }
        print(rep(tu$uid[1],ctNum))
        print(seq(1:ctNum))
        print(foreward_mean)
        print(comment_mean)
        print(like_mean)
        tcm=rbind(tcm,data.frame(rep(as.character(tu$uid[1]),ctNum*7),rep(seq(1:ctNum),7),rep(wk,10),c(foreward_mean),c(comment_mean),c(like_mean)))
    }
    print(NROW(tcm))
    colnames(tcm)<-c("uid","clust","week","foreward_count","comment_count","like_count")
    print(Sys.time())
}

funl=c(izero,rmOutlier,rmOutlier2,rmOutlierMorethanZero,rmOutlierMorethanZero2,imean,auto)
for(fi in 9:9)
{
    if(fi<8)
    {
        tm<-aggregate(t,by=list(t$uid),FUN=funl[[fi]])[,c(1,5:7)]
        names(tm)<-c("uid","foreward_count","comment_count","like_count")
        pu<-p[,1:2]
        r<-merge(pu,tm,by=c("uid"),all.x=T)
    }else if(fi==8)
    {
        #9
        cat(">> split ")
        print(Sys.time())
        tp<-split(t,f=as.factor(t$uid))
        tcm=NULL
        for(tpi in 1:NROW(tp))
        {
            tu=tp[tpi][[1]]
            if(NROW(tu)==0) next
            foreward_mean=rep(0,7)
            comment_mean=rep(0,7)
            like_mean=rep(0,7)
            wk=c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
            for(wi in 1:7)
            {
                #as.integer((sum(l1)-sum(l1out))/(NROW(l1)-NROW(l1out)))
                f1=tu$foreward_count[ weekdays(as.Date(tu$time))==wk[wi]]
                f1out=boxplot.stats(f1)$out
                if(NROW(f1)>0) foreward_mean[wi]=as.integer((sum(f1)-sum(f1out)+boxplot.stats(f1)$stats[5]*NROW(f1out))/NROW(f1))

                c1=tu$comment_count[ weekdays(as.Date(tu$time))==wk[wi]]
                c1out=boxplot.stats(c1)$out
                if(NROW(c1)>0) comment_mean[wi]=as.integer((sum(c1)-sum(c1out)+boxplot.stats(c1)$stats[5]*NROW(c1out))/NROW(c1))

                l1=tu$like_count[ weekdays(as.Date(tu$time))==wk[wi]]
                l1out=boxplot.stats(l1)$out
                if(NROW(l1)>0) like_mean[wi]=as.integer((sum(l1)-sum(l1out)+boxplot.stats(l1)$stats[5]*NROW(l1out))/NROW(l1))

            }
            tcm=rbind(tcm,data.frame(rep(as.character(tu$uid[1]),7),wk,foreward_mean,comment_mean,like_mean))
        }
        colnames(tcm)<-c("uid","week","foreward_count","comment_count","like_count")
        pcu=cbind(p[,1:2],week=dw[(1+NROW(t)):(NROW(t)+NROW(p))])
        r<-merge(pcu,tcm,by=c("uid","week"),all.x=T)
    }else
    {
        pcu=cbind(pc[,1:3],week=dw[(1+NROW(t)):NROW(doc)])
        r<-merge(pcu,tcm,by=c("uid","clust","week"),all.x=T)
    }

    cat(fi,"\n")
    #print(Sys.time())
    #cat("> assign vaule to predict data \n")
    #print(Sys.time())
    #cat("> assign 0 to new uid \n")
    r$foreward_count[is.na(r$foreward_count)]=0
    r$comment_count[is.na(r$comment_count)]=0
    r$like_count[is.na(r$like_count)]=0
    #save into .txt
    #cat("r.NROW= ", NROW(r)," p.NROW= ",NROW(p),"\n")
    #write.csv(r,"r.txt")
    #linux \t for tabs
    #system("sed -e 's#^\"[0-9]*\",##g' -e 's#\",#\t#g' -e 's#\"##g' -e '/_/d' r.txt > weibo_result.txt")
    #OS X ctrl+v+tab for tabs
    #if(fi==1) system("sed -e 's#^\"[0-9]*\",##g' -e 's#\",#	#g' -e 's#\"##g' -e '/_/d' r.txt > 1/weibo_result.txt")
    #if(fi==2) system("sed -e 's#^\"[0-9]*\",##g' -e 's#\",#	#g' -e 's#\"##g' -e '/_/d' r.txt > 2/weibo_result.txt")
    #if(fi==3) system("sed -e 's#^\"[0-9]*\",##g' -e 's#\",#	#g' -e 's#\"##g' -e '/_/d' r.txt > 3/weibo_result.txt")
    #if(fi==4) system("sed -e 's#^\"[0-9]*\",##g' -e 's#\",#	#g' -e 's#\"##g' -e '/_/d' r.txt > 4/weibo_result.txt")
    #if(fi==5) system("sed -e 's#^\"[0-9]*\",##g' -e 's#\",#	#g' -e 's#\"##g' -e '/_/d' r.txt > 5/weibo_result.txt")
    #if(fi==6) system("sed -e 's#^\"[0-9]*\",##g' -e 's#\",#	#g' -e 's#\"##g' -e '/_/d' r.txt > 6/weibo_result.txt")
    #if(fi==7) system("sed -e 's#^\"[0-9]*\",##g' -e 's#\",#	#g' -e 's#\"##g' -e '/_/d' r.txt > 7/weibo_result.txt")
    #if(fi==8) system("sed -e 's#^\"[0-9]*\",##g' -e 's#\",#	#g' -e 's#\"##g' -e '/_/d' r.txt > 8/weibo_result.txt")
    ##print(Sys.time())
    #cat("> Calculating pricision , only for test \n")

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
        pre=sum(count*prei)/sum(count)
        #print(Sys.time())
        cat(">>> precision = ",pre,"\n") #print(Sys.time()) } }
