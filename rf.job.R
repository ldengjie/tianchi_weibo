cat(">> start ")
print(Sys.time())

if(1)
{
    #argv <- commandArgs(TRUE)
    #isReal <- as.numeric(argv[1])
    #fitType<- as.numeric(argv[2])
    #subNum   <- as.numeric(argv[3])
    isReal <- 1
    fitType<- 1
    subNum   <- 1
    if(isReal)
    {
     t<-read.csv(paste("~/file/weibo/sub/real120/td",subNum,".txt",sep=""),header=T,sep="\t",quote="",comment="") 
     p<-read.csv(paste("~/file/weibo/sub/real120/pd",subNum,".txt",sep=""),header=T,sep="\t",quote="",comment="")
    }else
    {
     t<-read.csv(paste("~/file/weibo/sub/test104/td",subNum,".txt",sep=""),header=T,sep="\t",quote="",comment="") 
     p<-read.csv(paste("~/file/weibo/sub/test104/pd",subNum,".txt",sep=""),header=T,sep="\t",quote="",comment="")
    }
    t=t[1:10,]
    p=p[1:10,]
   fcl=data.frame(mid=t$mid,foreward_count=t$foreward_count,comment_count=t$comment_count,like_count=t$like_count)
    tnum=NROW(t)
    pnum=NROW(p)
}

#@ URL # chars

#if has topic top 10
if(0)
{
    #timeCon=data.frame(t$time,content=t$content)
    #tDay=split(timeCon,f=as.factor(timeCon$time))
    timeCon=data.frame(time=t$time,content=t$content)
    tDay=split(timeCon,f=as.factor(timeCon$time))
    for(ti in NROW(tDay))
    {

        tu=tDay[ti][[1]]
        doc=c(as.character(tu$content)) 
        doc=gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","",doc)
        tag=str_extract(doc,"#.+?#") 
    }
}

#uid features
if(0)
{
    cat(">>  uid features")
    print(Sys.time())
    tu=data.frame(table(t$uid))
    tuid=as.character(tu[tu$Freq>0,1])
    fea_uid=matrix(rep(rep(0,tnum,length(tuid))),tnum,length(tuid))
    colnames(fea_uid)=tuid
    for(ri in 1:tnum)
    {
        fea_uid[ri,grep(as.character(t[ri,1]),colnames(fea_uid))]=1
    }
}

#time
if(0)
{
    isum=function(x)
    {
        if(is.factor(x))
        {
            result=NA
        }else
        {
            result=as.integer(sum(x))
        }
        return(result)
    }
    cat(">>  week")
    print(Sys.time())
    tw=aggregate(t,by=list(t$time),FUN=isum)
    pw=aggregate(p,by=list(p$time),FUN=isum)
    datelist=rbind(tw[1],pw[1])
    datelistweek=as.numeric(format(as.Date(datelist[,1]),"%w"))
    weekFea=matrix(rep(0,NROW(datelist)*7),NROW(datelist),6)
    weekFea[datelistweek==1,1]=1
    weekFea[datelistweek==2,2]=1
    weekFea[datelistweek==3,3]=1
    weekFea[datelistweek==4,4]=1
    weekFea[datelistweek==5,5]=1
    weekFea[datelistweek==6,6]=1
    cat(">> holiday ")
    print(Sys.time())
    holidayFea=rep(0,NROW(datelist))
    holidayList=c(
                  "2014-09-06"
                  ,"2014-09-07"
                  ,"2014-09-08"
                  ,"2014-09-05"

                  ,"2014-10-01"
                  ,"2014-10-02"
                  ,"2014-10-03"
                  ,"2014-10-04"
                  ,"2014-10-05"
                  ,"2014-10-06"
                  ,"2014-10-07"
                  ,"2014-11-30"

                  ,"2015-01-01"
                  ,"2015-01-02"
                  ,"2015-01-03"
                  ,"2014-12-31"
                  )
    holidayFea[datelist[,1]%in%holidayList]=1
    tiaoxiuList=c("2014-09-28","2014-10-11","2015-01-04")
    tiaoxiuFea=rep(0,NROW(datelist))
    tiaoxiuFea[datelist[,1]%in%tiaoxiuList]=1
    dateFea=cbind(weekFea,holidayFea,tiaoxiuFea)
}

if(NROW(p)>0)
{
    print(Sys.time())
    if(1)
    {
        library(stringr)
        library(cluster)
        #library(mclust)
        library(tm)
        library(rJava)
        library(Rwordseg)
        library(gbm)
        library("e1071")
        library("randomForest")
        library("glmnet")

        cat(">> split ")
        print(Sys.time())
        tt=rbind(data.frame(uid=t$uid,mid=t$mid,time=t$time,content=t$content,foreward_count=t$foreward_count,comment_count=t$comment_count,like_count=t$like_count),data.frame(uid=p$uid,mid=p$mid,time=p$time,content=p$content,foreward_count=0,comment_count=0,like_count=0))
        tp<-split(tt,f=as.factor(tt$uid))
        pmin=data.frame(table(p$time))$Var1[1]
        tmax=data.frame(table(t$time))$Var1[NROW(data.frame(table(t$time)))]
    }

    pcm=NULL
    tcm=NULL
    rf=NULL
    tpnum=NROW(tp)
    for(tpi in 1:NROW(tp))
    {
        tu=tp[tpi][[1]]
        if(NROW(tu)==0) next
        numf=sum(as.Date(tu$time)<=as.Date(tmax))
        cat(" \n")
        cat(">>>>>> ",tpi,"/",tpnum," ",numf,"\n")
        #cat(">> segment ")
        #print(Sys.time())
        doc=c(as.character(tu$content)) 
        tu$content=NULL
        doc=gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","",doc)
        tag=str_extract(doc,"#.+?#") 
        tag=na.omit(tag)  #去除NA
        tag=unique(tag)    #去重
        if(length(tag)>0) insertWords(tag)
        docSeg=segmentCN(doc)
        if(NROW(tu)==1) docSeg=list(docSeg)
        #cat(">> corpus ")
        #print(Sys.time())
        docCor=Corpus(VectorSource(docSeg))
        # remove numbers
        docCor=tm_map(docCor, removeNumbers)
        #cat(">> stopWord ")
        #print(Sys.time())
        # remove stop words
        stw=read.table(file="dict/stopWord.txt",quote="",colClasses="character")
        docCor=tm_map(docCor,tm::removeWords,stw[,1])
        #cat(">> tdm ")
        #print(Sys.time())
        ctl=list(removePunctuation=T,minDocFreq=2,wordLengths = c(1, Inf),weighting = weightTf)
        docTdm=DocumentTermMatrix(docCor,control=ctl)
        tdm_removed=removeSparseTerms(docTdm, 0.9998) # 1-去除了低于 99.98% 的稀疏条目项
        tum=as.matrix(docTdm)
        #rm(docTdm,docCor,docSeg)
        #gc()

        if(numf<40)
        {
            cat(">> clust")
            print(Sys.time())
            ctNum=min(3,NROW(tu))
            if(NROW(tu)>3)
            {
                if(NCOL(tum)==0)
                {
                    ct=rep(1,NROW(tu))
                }else
                {
                    ctNum=min(3,(NROW(tum)-1))
                    pp<-pam(tum,ctNum)
                    ct=pp$clustering
                }
            }else
            {
                tu$content=NULL
                ct=rep(1,NROW(tu))
            }
            tuc=cbind(clust=ct[1:NROW(tu)],tu)
            tc=tuc[as.Date(tuc$time)<=as.Date(tmax),]
            pc=tuc[as.Date(tuc$time)>=as.Date(pmin),]
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
        }else
        {
            if(1)
            {
                #number of @
                #cat(">>  number of @")
                #print(Sys.time())
                numAT=function(x)
                {
                    g=gregexpr("@",x)[[1]]
                    g=g[g>0]
                    return(length(g))
                }
                num_AT=lapply(doc,FUN=numAT)
                fea_AT=factor(unlist(num_AT))
                #number of URLs
                #cat(">>  number of URLs")
                #print(Sys.time())
                numURL=function(x)
                {
                    g=gregexpr("http:[a-zA-Z\\/\\.0-9]+",x)[[1]]
                    g=g[g>0]
                    return(length(g))
                }
                num_URL=lapply(doc,FUN=numURL)
                fea_URL=factor(unlist(num_URL))
                #number of characters
                #cat(">>  number of characters")
                #print(Sys.time())
                nourl=gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","",doc)
                num_CHAR=lapply(nourl,FUN=function(x){nchar(x)})
                fea_CHAR=factor(unlist(num_CHAR))
                #number of  ##
                #cat(">>  number of  ##")
                #print(Sys.time())
                numTOPIC=function(x)
                {
                    g=gregexpr("#.+?#",x)[[1]]
                    g=g[g>0]
                    return(length(g))
                }
                num_TOPIC=lapply(doc,FUN=numTOPIC)
                fea_TOPIC=factor(unlist(num_TOPIC))
            }
            tuc=data.frame(tu,tum)
            if(nlevels(fea_AT)>1) tuc=data.frame(tuc,fea_AT)
            if(nlevels(fea_URL)>1) tuc=data.frame(tuc,fea_URL)
            if(nlevels(fea_CHAR)>1) tuc=data.frame(tuc,fea_CHAR)
            if(nlevels(fea_TOPIC)>1) tuc=data.frame(tuc,fea_TOPIC)

            #tuc=data.frame(tu,tum,fea_AT,fea_URL,fea_CHAR,fea_TOPIC)
            #tuc=data.frame(tu,tum)
            tc=tuc[as.Date(tuc$time)<=as.Date(tmax),]
            pc=tuc[as.Date(tuc$time)>=as.Date(pmin),]

            #cat(">> gbm")
            print(Sys.time())
            if(NROW(tc)>0)
            {
                tco=tc[,-c(which(names(tc)=="time"),which(names(tc)=="mid"),which(names(tc)=="uid"),which(names(tc)=="content"),which(names(tc)=="foreward_count"),which(names(tc)=="comment_count"),which(names(tc)=="like_count"))]
                pco=pc[,-c(which(names(pc)=="time"),which(names(pc)=="mid"),which(names(pc)=="uid"),which(names(pc)=="content"),which(names(tc)=="foreward_count"),which(names(tc)=="comment_count"),which(names(tc)=="like_count"))]
                tcf=data.frame(foreward_count=tc$foreward_count,tco)
                pcf=data.frame(foreward_count=pc$foreward_count,pco)
                tcc=data.frame(comment_count=tc$comment_count,tco)
                pcc=data.frame(comment_count=pc$comment_count,pco)
                tcl=data.frame(like_count=tc$like_count,tco)
                pcl=data.frame(like_count=pc$like_count,pco)
                #gbm_char<- gbm(foreward_count~fea_CHAR, tcf, distribution = "gaussian", n.trees = 500, bag.fraction = 0.75, cv.folds = 5, interaction.depth = 3)
                #lm
                if(fitType==1)
                {
                    cat(">> lm ")
                    print(Sys.time())
                    OLS_modelf <- lm(foreward_count~ ., data = tcf)
                    fp<- predict(OLS_modelf, pcf[, -which(names(pcf)=="foreward_count")])
                    OLS_modelc <- lm(comment_count~ ., data = tcc)
                    cp<- predict(OLS_modelc, pcc[, -which(names(pcc)=="comment_count")])
                    OLS_modell <- lm(like_count~ ., data = tcl)
                    lp<- predict(OLS_modell, pcl[, -which(names(pcl)=="like_count")])
                }
                #LASSO
                #cat(">> lasso")
                #print(Sys.time())
                #lasso_model <- cv.glmnet(x = tcf[,-which(names(tcf)=="foreward_count")], y = tcf$foreward_count, alpha = 1)
                #ridge
                #cat(">> ridge")
                #print(Sys.time())
                #ridge_model <- cv.glmnet(x = tcf[,-which(names(tcf)=="foreward_count")], y = tcf$foreward_count, alpha = 0)
                #random forest
                if(fitType==2)
                {
                    cat(">> random forest")
                    print(Sys.time())
                    rff<-randomForest(foreward_count~.,tcf,importance=TRUE)
                    fp<-predict(rff,pcf)
                    rfc<-randomForest(comment_count~.,tcc,importance=TRUE)
                    cp<-predict(rfc,pcc)
                    rfl<-randomForest(like_count~.,tcl,importance=TRUE)
                    lp<-predict(rfl,pcl)
                }

                #svm
                #cat(">> svm")
                #print(Sys.time())
                #svmf<-svm(foreward_count~.,tcf,type="eps-regression",cross = NROW(tcf))
                #svmf<-svm(foreward_count~.,tcf,type="eps-regression",cross = length(tcf),epsilon=bep[[ti]],cost=bco[[ti]])
                if(fitType==3)
                {
                    cat(">> gbm")
                    print(Sys.time())
                    gbm_modelf <- gbm(foreward_count~., tcf, distribution = "gaussian", n.trees = 500, bag.fraction = 0.75, cv.folds = 5, interaction.depth = 3)
                    gbm_perff <- gbm.perf(gbm_modelf, method = "cv")
                    fp<- predict(gbm_modelf,pcf,gbm_perff)
                    gbm_modelc <- gbm(comment_count~., tcc, distribution = "gaussian", n.trees = 500, bag.fraction = 0.75, cv.folds = 5, interaction.depth = 3)
                    gbm_perfc <- gbm.perf(gbm_modelc, method = "cv")
                    cp<- predict(gbm_modelc,pcc,gbm_perfc)
                    gbm_modell <- gbm(like_count~., tcl, distribution = "gaussian", n.trees = 500, bag.fraction = 0.75, cv.folds = 5, interaction.depth = 3)
                    gbm_perfl <- gbm.perf(gbm_modell, method = "cv")
                    lp<- predict(gbm_modell,pcl,gbm_perfl)
                }
                cat(">> all done")
                print(Sys.time())
            }
            rf=rbind(rf,data.frame(pc$uid,pc$mid,fp,cp,lp))
        }
    }
    #clust part
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
    #total r
    r=rbind(r,rf)
    #print(Sys.time())
    r$foreward_count[is.na(r$foreward_count)]=0
    r$comment_count[is.na(r$comment_count)]=0
    r$like_count[is.na(r$like_count)]=0
    #save into .txt
    write.csv(r,paste("~/file/weibo/job/clust_fit/",isReal,fitType,"/r",isReal,"_",fitType,".txt",sep=""))
    #OS X ctrl+v+tab for tabs
    system(paste("sed -e 's#^\"[0-9]*\",##g' -e 's#\",# #g' -e 's#\"##g' -e '/_/d' ~/file/weibo/job/clust_fit/",isReal,fitType,"/r",isReal,"_",fitType,".txt > ~/file/weibo/job/clust_fit/",isReal,fitType,"/",isReal,"_",fitType,"weibo_result.txt",sep=""))

    cat(" Finished this work ... ")
}
