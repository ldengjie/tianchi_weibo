#Rscript wb.R
library("stringr")
library("cluster")
library("mclust")
library("tm")
library(rJava)
library(Rwordseg)

print(Sys.time())
#argv <- commandArgs(TRUE)
#isReal <- as.numeric(argv[1])
#clustType<- as.numeric(argv[2])
#clustNum <- as.numeric(argv[3])
#subNum   <- as.numeric(argv[4])
isReal <- 1
clustType<- 3
clustNum <- 5
subNum   <- 8
cat("isReal",isReal,"\n")
cat("clustType",clustType,"\n")
cat("clustNum ",clustNum ,"\n")
cat("subNum   ",subNum   ,"\n")
#cat("> read data \n")
print(Sys.time())
if(isReal)
{
    t<-read.csv(paste("~/file/weibo/sub/real/td",subNum,".txt",sep=""),header=T,sep="\t",quote="",comment="") 
    p<-read.csv(paste("~/file/weibo/sub/real/pd",subNum,".txt",sep=""),header=T,sep="\t",quote="",comment="")
}else
{
    t<-read.csv(paste("~/file/weibo/sub/test/td",subNum,".txt",sep=""),header=T,sep="\t",quote="",comment="") 
    p<-read.csv(paste("~/file/weibo/sub/test/pd",subNum,".txt",sep=""),header=T,sep="\t",quote="",comment="")
}
print(Sys.time())

maxDate=max(as.Date(t$time))
minDate=min(as.Date(p$time))
fcl=data.frame(mid=t$mid,foreward_count=t$foreward_count,comment_count=t$comment_count,like_count=t$like_count)

cat(">> split ")
print(Sys.time())
tt=rbind(data.frame(uid=t$uid,mid=t$mid,time=t$time,content=t$content),data.frame(uid=p$uid,mid=p$mid,time=p$time,content=p$content))
if(NROW(p)>0)
{

    tp<-split(tt,f=as.factor(tt$uid))

    rm(p,t,tt)
    gc()

    pcm=NULL
    tcm=NULL
    cat("==NROW(tp)",NROW(tp),"\n")
    for(tpi in 1:NROW(tp))
    {
        cat(">>>>>> ",tpi)
        if(tpi==72) next 
        tu=tp[tpi][[1]]
        cat(" [ ",NROW(tu)," docs ] ")
        print(Sys.time())
        ctNum=min(clustNum,NROW(tu))
        if(NROW(tu)>3)
        {
            doc=c(as.character(tu$content)) 
            tu$content=NULL
            doc=gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","",doc)                            
            tag=str_extract(doc,"#.+?#") 
            tag=na.omit(tag)  #鍘婚櫎NA
            tag=unique(tag)    #鍘婚噸
            if(length(tag)>0) insertWords(tag)
            docSeg=segmentCN(doc)
            docCor=Corpus(VectorSource(docSeg))
            stw=read.table(file="dict/stopWord.txt",quote="",colClasses="character")
            docCor=tm_map(docCor,tm::removeWords,stw[,1])
            #ctl=list(removePunctuation=T,minDocFreq=2,wordLengths = c(1, Inf),weighting = weightTfIdf)
            ctl=list(removePunctuation=T,minDocFreq=2,wordLengths = c(1, Inf),weighting = weightTf)
            docTdm=TermDocumentMatrix(docCor,control=ctl)
            #print(docTdm)
            d=removeSparseTerms(docTdm,0.9975)
            rm(docTdm,docCor,docSeg,doc)
            gc()
            dm=t(as.matrix(d))
            rm(d)
            gc()
            if(NCOL(dm)==0)
            {
                ct=rep(1,NROW(tu))
            }else
            {
                if(clustType==1)
                {
                    dist_tdm <- proxy::dist(dm)
                    hc <- hclust(dist_tdm)
                    ct = cutree(hc,k=ctNum)
                }
                if(clustType==2)
                {
                    ctNum=min(clustNum,NROW(unique(dm)),(NROW(dm)-1))
                    k<-kmeans(dm,ctNum)
                    ct=k$cluster
                }
                if(clustType==3)
                {
                    ctNum=min(clustNum,(NROW(dm)-1))
                    pp<-pam(dm,ctNum)
                    ct=pp$clustering
                }
                if(clustType==4)
                {
                    while(is.null(fm<-Mclust(dm,ctNum)) && ctNum>0) ctNum=ctNum-1
                    if(is.null(fm))
                    {
                        ct=rep(1,NROW(tu))
                    }else
                    {
                        ct=fm$class
                    }
                }
            }
        }else
        {
            tu$content=NULL
            ct=rep(1,NROW(tu))
        }
        tuc=cbind(clust=ct[1:NROW(tu)],tu)
        tc=tuc[as.Date(tuc$time)<=maxDate,]
        pc=tuc[as.Date(tuc$time)>=minDate,]
        if(NROW(tc)>0)
        {
            tc=merge(tc,fcl,by=c("mid"))
            foreward_mean=rep(0,ctNum)
            comment_mean=rep(0,ctNum)
            like_mean=rep(0,ctNum)
            for(cti in 1:ctNum)
            {
                f1=tc$foreward_count[tc$clust==cti]
                f1out=boxplot.stats(f1)$out
                if(NROW(f1)>0) foreward_mean[cti]=as.integer((sum(f1)-sum(f1out)+boxplot.stats(f1)$stats[5]*NROW(f1out))/NROW(f1))
                c1=tc$comment_count[tc$clust==cti]
                c1out=boxplot.stats(c1)$out
                if(NROW(c1)>0) comment_mean[cti]=as.integer((sum(c1)-sum(c1out)+boxplot.stats(c1)$stats[5]*NROW(c1out))/NROW(c1))
                l1=tc$like_count[tc$clust==cti]
                l1out=boxplot.stats(l1)$out
                if(NROW(l1)>0) like_mean[cti]=as.integer((sum(l1)-sum(l1out)+boxplot.stats(l1)$stats[5]*NROW(l1out))/NROW(l1))
            }
            tcm=rbind(tcm,data.frame(rep(as.character(tc$uid[1]),ctNum),seq(1:ctNum),foreward_mean,comment_mean,like_mean))
        }
        pcm=rbind(pcm,pc)
        tp[tpi][[1]]$content=NULL
        gc()
    }
    if(NROW(tcm)>0)
    {
        colnames(tcm)<-c("uid","clust","foreward_count","comment_count","like_count")
        pcu=data.frame(uid=pcm$uid,mid=pcm$mid,clust=pcm$clust)
        r<-merge(pcu,tcm,by=c("uid","clust"),all.x=T)
        r=r[,-which(names(r)=="clust")]
    }else
    {
        r=data.frame(uid=pcm$uid,mid=pcm$mid,foreward_count=rep(0,NROW(pcm)),comment_count=rep(0,NROW(pcm)),like_count=rep(0,NROW(pcm)))
    }
    print(Sys.time())

    cat(" done loop ...\n")
    #cat("> assign 0 to new uid \n")
    r$foreward_count[is.na(r$foreward_count)]=0
    r$comment_count[is.na(r$comment_count)]=0
    r$like_count[is.na(r$like_count)]=0
    #save into .txt
    write.csv(r,paste("~/file/weibo/job/sub/",isReal,clustType,clustNum,"/r",isReal,"_",clustType,"_",clustNum,"_",subNum,".txt",sep=""))
    #OS X ctrl+v+tab for tabs
    #system(paste("sed -e 's#^\"[0-9]*\",##g' -e 's#\",# #g' -e 's#\"##g' -e '/_/d' r.txt > ",fi,"/weibo_result.txt",sep=""))
    system(paste("sed -e 's#^\"[0-9]*\",##g' -e 's#\",# #g' -e 's#\"##g' -e '/_/d' ~/file/weibo/job/sub/",isReal,clustType,clustNum,"/r",isReal,"_",clustType,"_",clustNum,"_",subNum,".txt > ~/file/weibo/job/sub/",isReal,clustType,clustNum,"/",clustType,"_",clustNum,"_",subNum,"weibo_result.txt",sep=""))

    cat(" Finished this work ... ")

    #cat("> Calculating pricision , only for test \n")
    #if(0)
    #{
    #rp<-merge(pp,r,by=c("uid","mid"))
    #devf=abs(rp$foreward_count.y-rp$foreward_count.x)/(rp$foreward_count.x+5)
    #devc=abs(rp$comment_count.y-rp$comment_count.x)/(rp$comment_count.x+3)
    #devl=abs(rp$like_count.y-rp$like_count.x)/(rp$like_count.x+3)
    #prei=1-0.5*devf-0.25*devc-0.25*devl
    #pret=prei
    #prei[(prei-0.8)>0]=1
    #prei[(prei-0.8)<=0]=0
    #count=(rp$foreward_count.x+rp$comment_count.x+rp$like_count.x+1)
    #pre=sum(count*prei)/sum(count)
    ##print(Sys.time())
    #cat(">>> precision (",clustType,"_",clustNum," ) = ",pre,"\n")
    ##print(Sys.time())
    #}
}
