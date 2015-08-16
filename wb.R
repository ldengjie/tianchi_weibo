#Rscript wb.R
print(Sys.time())
#cat("> read data \n")
argv <- commandArgs(TRUE)
istest <- as.numeric(argv[1])
clustType<- as.numeric(argv[2])
clustNum <- as.numeric(argv[3])

#t<-read.csv("weibo_train_data.txt",header=T,sep="\t",quote="") 
#p<-read.csv("weibo_predict_data.txt",header=T,sep="\t",quote="")

#t<-read.csv("train.txt",header=T,sep="\t",quote="") 
#p<-read.csv("predict.txt",header=T,sep="\t",quote="")

#t<-read.csv("t2.txt",sep="\t",quote="") 
#p<-read.csv("p2.txt",sep="\t",quote="")

#p=p[p$uid=="82990720c0936340c7a17e712f549b30",]
#t=t[t$uid=="82990720c0936340c7a17e712f549b30",]
#p=p[p$uid=="3df25e570db062bab9cbf0782cf09630",]
#t=t[t$uid=="3df25e570db062bab9cbf0782cf09630",]

#save.image("RDataTest")
#save.image("RDataTotal")

ifelse(istest,load("RDataTest"),load("RDataTotal"))
#if(istest)
#{
#t<-read.csv("t1.txt",sep="\t",quote="") 
#p<-read.csv("p1.txt",sep="\t",quote="")
#}else
#{
#t<-read.csv("t3.txt",sep="\t",quote="") 
#p<-read.csv("p3.txt",sep="\t",quote="")
#}
#t=cbind(t,week=as.numeric(format(as.Date(t$time),"%w")))
#p=cbind(p,week=as.numeric(format(as.Date(p$time),"%w")))                                                                                       
#load("seg.RData")
#load("sp.RData")

#tp=split(t,f=as.factor(t$uid))

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
#2
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
        #result=round((sum(x)-sum(xOutlierx))/(NROW(x)-NROW(xOutlierx)))
    }
    return(result)
}
#3 
rmOutlier2=function(x)
{
    if(is.factor(x))
    {
        result=NA
    }else
    {
        xMorethanZero=x
        xOutlierx=boxplot.stats(xMorethanZero)$out
        #result=as.integer((sum(x)-sum(xOutlierx)+boxplot.stats(xMorethanZero)$stats[5]*NROW(xOutlierx))/NROW(x))
        #if(result>0) result=result-1
        result=as.integer((sum(x)-sum(xOutlierx)+boxplot.stats(xMorethanZero)$stats[5]*NROW(xOutlierx))/NROW(x)+0.085)
        #result=(sum(x)-sum(xOutlierx)+boxplot.stats(xMorethanZero)$stats[5]*NROW(xOutlierx))/NROW(x)
        #result=round((sum(x)-sum(xOutlierx)+boxplot.stats(xMorethanZero)$stats[5]*NROW(xOutlierx))/NROW(x))
    }
    return(result)
}
#4
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
        #result=round((sum(x)-sum(xOutlierx))/(NROW(x)-NROW(xOutlierx)))
    }
    return(result)
}
#5
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
        #result=round((sum(x)-sum(xOutlierx)+boxplot.stats(xMorethanZero)$stats[5]*NROW(xOutlierx))/NROW(x))
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
        #result=round(mean(x))
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
            #result=round(mean(x))
        }else
        {
            xOutlierx=boxplot.stats(x)$out
            result=as.integer((sum(x)-sum(xOutlierx)+boxplot.stats(x)$stats[5]*NROW(xOutlierx))/NROW(x))
            #result=round((sum(x)-sum(xOutlierx)+boxplot.stats(x)$stats[5]*NROW(xOutlierx))/NROW(x))
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


funl=c(izero,rmOutlier,rmOutlier2,rmOutlierMorethanZero,rmOutlierMorethanZero2,imean,auto)
for(fi in 9:9)
{
    if(fi<8)
    {
        print(fi)
        tm<-aggregate(t,by=list(t$uid),FUN=funl[[fi]])[,c(1,5:7)]
        names(tm)<-c("uid","foreward_count","comment_count","like_count")
        pu<-p[,1:2]
        r<-merge(pu,tm,by=c("uid"),all.x=T)
    }else if(fi==8)
    {
        #8
        cat(">> split ")
        print(Sys.time())
        tp<-split(t,f=as.factor(t$uid))
        print(Sys.time())
        tcm=NULL
        print(NROW(tp))
        for(tpi in 1:NROW(tp))
        {
            if(tpi%%1000==1) 
            {
                cat("> ",tpi)
                print(Sys.time())
            }
            tu=tp[tpi][[1]]
            tup=split(tu,f=as.factor(tu$week))
            if(NROW(tu)==0) next
            foreward_mean=rep(0,7)
            comment_mean=rep(0,7)
            like_mean=rep(0,7)
            for(wi in 1:7)
            {
                #as.integer((sum(l1)-sum(l1out))/(NROW(l1)-NROW(l1out)))
                f1=tup[wi][[1]]$foreward_count
                f1out=boxplot.stats(f1)$out
                if(NROW(f1)>0)
                {
                    foreward_mean[wi]=as.integer((sum(f1)-sum(f1out)+boxplot.stats(f1)$stats[5]*NROW(f1out))/NROW(f1))
                }else
                {
                    foreward_mean[wi]=as.integer(mean(tu$foreward_count))
                }


                c1=tup[wi][[1]]$comment_count
                c1out=boxplot.stats(c1)$out
                if(NROW(c1)>0)
                {
                    comment_mean[wi]=as.integer((sum(c1)-sum(c1out)+boxplot.stats(c1)$stats[5]*NROW(c1out))/NROW(c1))
                }else
                {
                    comment_mean[wi]=as.integer(mean(tu$comment_count))
                }
                l1=tup[wi][[1]]$like_count
                l1out=boxplot.stats(l1)$out
                if(NROW(l1)>0)
                {
                    like_mean[wi]=as.integer((sum(l1)-sum(l1out)+boxplot.stats(l1)$stats[5]*NROW(l1out))/NROW(l1))
                }else
                {
                    like_mean[wi]=as.integer(mean(tu$like_count))
                }

            }
            tcm=rbind(tcm,data.frame(rep(as.character(tu$uid[1]),7),c(0:6),foreward_mean,comment_mean,like_mean))
            #foreward_mean=mean(tu$foreward_count)
            #comment_mean=mean(tu$comment_count)
            #like_mean=mean(tu$like_count)
            #tcm=rbind(tcm,tu$uid,foreward_mean,comment_mean,like_mean)
        }
        colnames(tcm)<-c("uid","week","foreward_count","comment_count","like_count")
        pcu=cbind(p[,c(1,2,NCOL(p))])
        r<-merge(pcu,tcm,by=c("uid","week"),all.x=T)
    }else if(fi==9)
    {
        #9
        print(Sys.time())
        library("stringr")
        library("cluster")
        library("mclust")
        library("tm")
        library(rJava)
        library(Rwordseg)

        cat(">> split ")
        print(Sys.time())
        #tt=rbind(t[c(1,2,3,7,8)],p)
        tt=rbind(data.frame(uid=t$uid,mid=t$mid,time=t$time,content=t$content,week=t$week),data.frame(uid=p$uid,mid=p$mid,time=p$time,content=p$content,week=p$week))
        tp<-split(tt,f=as.factor(tt$uid))
        pcm=NULL
        tcm=NULL
        for(tpi in 1:NROW(tp))
        {
            #if(tpi%%10==1) 
            #{
            #cat("> ",tpi)
            #print(Sys.time())
            #}
            cat(">>>>>> ",tpi)
            tu=tp[tpi][[1]]
            ctNum=min(clustNum,NROW(tu))
            if(NROW(tu)>3)
            {
                #cat(">> segment ")
                #print(Sys.time())
                doc=c(as.character(tu$content)) 
                doc=gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","",doc)                            
                tag=str_extract(doc,"#.+?#") 
                tag=na.omit(tag)  #去除NA
                tag=unique(tag)    #去重
                if(length(tag)>0) insertWords(tag)
                #docSeg=NULL
                #for(si in 1:(NROW(doc)%/%10000))
                #{
                #cat(">>> ",si,"/",(NROW(doc)%/%10000))
                #print(Sys.time())
                #docSeg[si]=segmentCN(doc[((si-1)*10000+1):(si*10000)])
                #}
                docSeg=segmentCN(doc)
                #detach("package:Rwordseg", unload=TRUE)

                #cat(">> corpus ")
                #print(Sys.time())
                docCor=Corpus(VectorSource(docSeg))
                # remove numbers
                #docCor=tm_map(docCor, removeNumbers)
                #cat(">> stopWord ")
                #print(Sys.time())
                # remove stop words
                stw=read.table(file="dict/stopWord.txt",quote="",colClasses="character")
                docCor=tm_map(docCor,tm::removeWords,stw[,1])
                #cat(">> tdm ")
                #print(Sys.time())
                ctl=list(removePunctuation=T,minDocFreq=2,wordLengths = c(1, Inf),weighting = weightTfIdf)
                docTdm=TermDocumentMatrix(docCor,control=ctl)
                #length(docTdm$dimnames$Terms)
                #tdm_removed=removeSparseTerms(docTdm, 0.9998) # 1-去除了低于 99.98% 的稀疏条目项
                #length(tdm_removed$dimnames$Terms)
                #time week features
                #dw=c(weekdays(as.Date(t$time)),weekdays(as.Date(p$time)))
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

                cat(" clust ",NROW(tu))
                print(Sys.time())
                #clust
                m=as.matrix(docTdm)
                #m=rbind(m, Monday=Mon, Tuesday=Tue, Wednesday=Wed, Thursday=Thu, Friday=Fri, Saturday=Sat, Sunday=Sun)
                dm=t(m)
                #cat("1\n")
                #HC
                if(clustType==1)
                {
                    dist_tdm <- proxy::dist(dm, method = 'cosine')
                    hc <- hclust(dist_tdm, method = 'mcquitty')
                    ct = cutree(hc,k=ctNum)
                }
                ##"k-Means"
                if(clustType==2)
                {
                    ctNum=min(ctNum,NROW(dm))
                    k<-kmeans(dm,ctNum)
                    ct=k$cluster
                }
                ##k-Medoids
                if(clustType==3)
                {
                    pp<-pam(dm,ctNum)
                    ct=pp$clustering
                }
                #EM
                if(clustType==4)
                {
                    fm<-Mclust(dm,ctNum)
                    ct=fm$class
                }
                #doc=cbind(doc,clust=ct,fm=rep(NA,NROW(ct),cm=rep(NA,NROW(ct),lm=rep(NA,NROW(ct))
                #cat("2\n")
            }else
            {
                ct=rep(1,NROW(tu))
            }

            #tc=cbind(clust=ct[1:NROW(t)],t)
            #pc=cbind(clust=ct[(1+NROW(t)):NROW(doc)],p)
            tuc=cbind(clust=ct[1:NROW(tu)],tu)
            tc=tuc[as.Date(tuc$time)<=max(as.Date(t$time)),]
            pc=tuc[as.Date(tuc$time)>=min(as.Date(p$time)),]

            if(NROW(tc)>0)
            {
                tc=merge(tc,data.frame(mid=t$mid,foreward_count=t$foreward_count,comment_count=t$comment_count,like_count=t$like_count),by=c("mid"))

                foreward_mean=rep(0,ctNum)
                comment_mean=rep(0,ctNum)
                like_mean=rep(0,ctNum)
                #foreward_mean=matrix(rep(0,ctNum*7),ctNum,7)
                #comment_mean=matrix(rep(0,ctNum*7),ctNum,7)
                #like_mean=matrix(rep(0,ctNum*7),ctNum,7)
                for(cti in 1:ctNum)
                {
                    #foreward_mean[cti]=as.integer(mean(na.omit(tc$foreward_count[tc$clust==cti])))
                    f1=tc$foreward_count[tc$clust==cti]
                    f1out=boxplot.stats(f1)$out
                    if(NROW(f1)>0) foreward_mean[cti]=as.integer((sum(f1)-sum(f1out)+boxplot.stats(f1)$stats[5]*NROW(f1out))/NROW(f1))
                    #comment_mean[cti]=as.integer(mean(na.omit(tc$comment_count[tc$clust==cti])))
                    c1=tc$comment_count[tc$clust==cti]
                    c1out=boxplot.stats(c1)$out
                    if(NROW(c1)>0) comment_mean[cti]=as.integer((sum(c1)-sum(c1out)+boxplot.stats(c1)$stats[5]*NROW(c1out))/NROW(c1))
                    #like_mean[cti]=as.integer(mean(na.omit(tc$like_count[tc$clust==cti])))
                    l1=tc$like_count[tc$clust==cti]
                    l1out=boxplot.stats(l1)$out
                    if(NROW(l1)>0) like_mean[cti]=as.integer((sum(l1)-sum(l1out)+boxplot.stats(l1)$stats[5]*NROW(l1out))/NROW(l1))
                    #wk=c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
                    #for(wi in 1:7)
                    #{
                    ##as.integer((sum(l1)-sum(l1out))/(NROW(l1)-NROW(l1out)))
                    #f1=tc$foreward_count[((tc$clust==cti) + (weekdays(as.Date(tc$time))==wk[wi]))==2]
                    #f1out=boxplot.stats(f1)$out
                    #if(NROW(f1)>0) foreward_mean[cti,wi]=as.integer((sum(f1)-sum(f1out)+boxplot.stats(f1)$stats[5]*NROW(f1out))/NROW(f1))
                    #
                    #c1=tc$comment_count[((tc$clust==cti) + (weekdays(as.Date(tc$time))==wk[wi]))==2]
                    #c1out=boxplot.stats(c1)$out
                    #if(NROW(c1)>0) comment_mean[cti,wi]=as.integer((sum(c1)-sum(c1out)+boxplot.stats(c1)$stats[5]*NROW(c1out))/NROW(c1))
                    #
                    #l1=tc$like_count[((tc$clust==cti) + (weekdays(as.Date(tc$time))==wk[wi]))==2]
                    #l1out=boxplot.stats(l1)$out
                    #if(NROW(l1)>0) like_mean[cti,wi]=as.integer((sum(l1)-sum(l1out)+boxplot.stats(l1)$stats[5]*NROW(l1out))/NROW(l1))
                    #}
                }
                #print(rep(tc$uid[1],ctNum))
                #print(seq(1:ctNum))
                #print(foreward_mean)
                #print(comment_mean)
                #print(like_mean)
                tcm=rbind(tcm,data.frame(rep(as.character(tc$uid[1]),ctNum),seq(1:ctNum),foreward_mean,comment_mean,like_mean))
            }
            #tcm=rbind(tcm,data.frame(rep(as.character(tc$uid[1]),ctNum*7),rep(seq(1:ctNum),7),rep(wk,10),c(foreward_mean),c(comment_mean),c(like_mean)))
            if(NROW(pc)>0) pcm=rbind(pcm,pc)
        }
        #print(NROW(tcm))
        colnames(tcm)<-c("uid","clust","foreward_count","comment_count","like_count")
        #pcu=cbind(pcm[,1:3])
        pcu=data.frame(uid=pcm$uid,mid=pcm$mid,clust=pcm$clust)
        if(NROW(pcu)>0)
        {
            r<-merge(pcu,tcm,by=c("uid","clust"),all.x=T)
            r=r[,-which(names(r)=="clust")]
        }
        #colnames(tcm)<-c("uid","clust","week","foreward_count","comment_count","like_count")
        #pcu=cbind(pc[,1:3],week=dw[(1+NROW(t)):NROW(doc)])
        #r<-merge(pcu,tcm,by=c("uid","clust","week"),all.x=T)
        print(Sys.time())
    }else
    {
    }

    cat(fi," done loop ...\n")
    #print(Sys.time())
    #cat("> assign vaule to predict data \n")
    #print(Sys.time())
    #cat("> assign 0 to new uid \n")
    r$foreward_count[is.na(r$foreward_count)]=0
    r$comment_count[is.na(r$comment_count)]=0
    r$like_count[is.na(r$like_count)]=0
    #save into .txt
    #cat("r.NROW= ", NROW(r)," p.NROW= ",NROW(p),"\n")
    if(!istest) write.csv(r,paste("r",clustType,"_",clustNum,".txt",sep=""))
    #linux \t for tabs
    #system("sed -e 's#^\"[0-9]*\",##g' -e 's#\",#\t#g' -e 's#\"##g' -e '/_/d' r.txt > weibo_result.txt")
    #OS X ctrl+v+tab for tabs
    #system(paste("sed -e 's#^\"[0-9]*\",##g' -e 's#\",# #g' -e 's#\"##g' -e '/_/d' r.txt > ",fi,"/weibo_result.txt",sep=""))
    if(!istest) system(paste("sed -e 's#^\"[0-9]*\",##g' -e 's#\",# #g' -e 's#\"##g' -e '/_/d' r",clustType,"_",clustNum,".txt > ",fi,"/",clustType,"_",clustNum,"weibo_result.txt",sep=""))
    ##print(Sys.time())
    #cat("> Calculating pricision , only for test \n")
    if(0)
    {
        r3=r[,3]
        r4=r[,4]
        r5=r[,5]
        #for(c3 in seq(-0.5,0.5,0.1))
        #for(c3 in 0.085)
        #{
        #for(c4 in 0.085)
        #{
        for(c6 in seq(0.080,0.09,0.001))                                                                               
        {
            #r[,3]=as.integer(r3+c3)
            #r[,4]=as.integer(r4+c4)
            #r[,5]=as.integer(r5+c5)
            r[,3]=as.integer(r3+c6)
            r[,4]=as.integer(r4+c6)
            r[,5]=as.integer(r5+c6)
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
            cat(">>> ",c3,c4,c5," precision = ",pre,"\n")
            #print(Sys.time())
        }
        #}
        #}
    }
    if(istest)
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
        #print(Sys.time())
        cat(">>> precision (",clustType,"_",clustNum," ) = ",pre,"\n")
        #print(Sys.time())
    }
}
