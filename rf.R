cat(">> start ")
print(Sys.time())

if(1)
{
    load("RDataTotal")
    t2=t[1:2000,]
    t2=t
    content=t2$content
}

#@ URL # chars
if(0)
{
    #number of @
    cat(">>  number of @")
    print(Sys.time())
    numAT=function(x)
    {
        g=gregexpr("@",x)[[1]]
        g=g[g>0]
        return(length(g))
    }
    num_AT=lapply(content,FUN=numAT)
    fea_AT=factor(unlist(num_AT))
    #number of URLs
    cat(">>  number of URLs")
    print(Sys.time())
    numURL=function(x)
    {
        g=gregexpr("http:[a-zA-Z\\/\\.0-9]+",x)[[1]]
        g=g[g>0]
        return(length(g))
    }
    num_URL=lapply(content,FUN=numURL)
    fea_URL=factor(unlist(num_URL))
    #number of characters
    cat(">>  number of characters")
    print(Sys.time())
    nourl=gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","",content)
    num_CHAR=lapply(nourl,FUN=function(x){nchar(x)})
    fea_CHAR=factor(unlist(num_CHAR)0
    #number of  ##
    cat(">>  number of  ##")
    print(Sys.time())
    numTOPIC=function(x)
    {
        g=gregexpr("#.+?#",x)[[1]]
        g=g[g>0]
        return(length(g))
    }
    num_TOPIC=lapply(content,FUN=numTOPIC)
    fea_TOPIC=factor(unlist(num_TOPIC))
}

#if has topic top 10
if(0)
{
    #timeCon=data.frame(t2$time,content=t2$content)
    #tDay=split(timeCon,f=as.factor(timeCon$time))
    timeCon=data.frame(time=t$time,content=t$content)
    tDay=split(timeCon,f=as.factor(timeCon$time))
    for(ti in NROW(tDay))
    {

        tu=tDay[ti][[1]]
        doc=c(as.character(tu$content)) 
        doc=gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","",doc)
        tag=str_extract(doc,"#.+?#") 
        #docSeg=segmentCN(doc)
        #keys = worker("keywords", topn = 10)
        #keys<=doc
    }
}

#uid features
if(0)
{
    cat(">>  uid features")
    print(Sys.time())
    tu=data.frame(table(t2$uid))
    tuid=as.character(tu[tu$Freq>0,1])
    fea_uid=matrix(rep(rep(0,NROW(t2),length(tuid))),NROW(t2),length(tuid))
    colnames(fea_uid)=tuid
    for(ri in 1:NROW(t2))
    {
        fea_uid[ri,grep(as.character(t2[ri,1]),colnames(fea_uid))]=1
    }
}

#term-file
if(0)
{
    cat(">>  term-file")
    print(Sys.time())
    doc=gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","",content)
    docSeg=segmentCN(doc)
    docCor=Corpus(VectorSource(docSeg))
    stw=read.table(file="dict/stopWord.txt",quote="",colClasses="character")
    docCor=tm_map(docCor,tm::removeWords,stw[,1])
    #ctl=list(removePunctuation=T,minDocFreq=2,wordLengths = c(1, Inf),weighting = weightTfIdf)
    ctl=list(removePunctuation=T,minDocFreq=1,wordLengths = c(1, Inf),weighting = weightTf)
    docTdm=DocumentTermMatrix(docCor,control=ctl)
    tdm_removed=removeSparseTerms(docTdm, 0.9998) # 1-去除了低于 99.98% 的稀疏条目项
    docTdm.matrix=as.matrix(tdm_removed)
}

#time
if(0)
{
    cat(">>  week")
    print(Sys.time())
    tc=aggregate(t,by=list(t$time),FUN=funl[[1]])
    pc=aggregate(p,by=list(p$time),FUN=funl[[1]])
    datelist=rbind(tc[1],pc[1])
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


#9
print(Sys.time())
if(1)
{
    library(stringr)
    library(cluster)
    library(mclust)
    library(tm)
    library(rJava)
    library(Rwordseg)
    library(gbm)
    library("e1071")

    cat(">> split ")
    print(Sys.time())
    tt=rbind(data.frame(uid=t$uid,mid=t$mid,time=t$time,content=t$content,foreward_count=t$foreward_count,comment_count=t$comment_count,like_count=t$like_count),data.frame(uid=p$uid,mid=p$mid,time=p$time,content=p$content,foreward_count=0,comment_count=0,like_count=0))
    tp<-split(tt,f=as.factor(tt$uid))
    pmin=data.frame(table(p$time))$Var1[1]
    tmax=data.frame(table(t$time))$Var1[NROW(data.frame(table(t$time)))]
}

pcm=NULL
tcm=NULL
for(tpi in 1:NROW(tp))
#for(tpi in 262)
{
    tu=tp[tpi][[1]]
    #meanf=tu[as.Date(tu$time)<=as.Date(tmax),which(names(tu)=="foreward_count")]
    #if(sum(as.Date(tu$time)<=as.Date(tmax)) < 100 && meanf<0.5) next
    #if(sum(as.Date(tu$time)<=as.Date(tmax)) < 40) next
    numf=sum(as.Date(tu$time)<=as.Date(tmax))
    if(numf<40) next

    #if(NROW(tu)<100 ) next
    cat(">>>>>> ",tpi,numf,"\n")
    cat(">> segment ")
    print(Sys.time())
    doc=c(as.character(tu$content)) 
    doc=gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","",doc)
    tag=str_extract(doc,"#.+?#") 
    tag=na.omit(tag)  #去除NA
    tag=unique(tag)    #去重
    if(length(tag)>0) insertWords(tag)
    docSeg=segmentCN(doc)
    if(NROW(tu)==1) docSeg=list(docSeg)
    cat(">> corpus ")
    print(Sys.time())
    docCor=Corpus(VectorSource(docSeg))
    # remove numbers
    docCor=tm_map(docCor, removeNumbers)
    cat(">> stopWord ")
    print(Sys.time())
    # remove stop words
    stw=read.table(file="dict/stopWord.txt",quote="",colClasses="character")
    docCor=tm_map(docCor,tm::removeWords,stw[,1])
    cat(">> tdm ")
    print(Sys.time())
    ctl=list(removePunctuation=T,minDocFreq=2,wordLengths = c(1, Inf),weighting = weightTf)
    docTdm=DocumentTermMatrix(docCor,control=ctl)
    tdm_removed=removeSparseTerms(docTdm, 0.9998) # 1-去除了低于 99.98% 的稀疏条目项
    tum=as.matrix(docTdm)

    tuc=cbind(tu,tum)
    tc=tuc[as.Date(tuc$time)<=as.Date(tmax),]
    pc=tuc[as.Date(tuc$time)>=as.Date(pmin),]

    cat(">> fit")
    print(Sys.time())
    if(NROW(tc)>0)
    {
        tcf=tc[,-c(which(names(tc)=="time"),which(names(tc)=="mid"),which(names(tc)=="uid"),which(names(tc)=="content"),which(names(tc)=="comment_count"),which(names(tc)=="like_count"))]
        pcf=pc[,-c(which(names(pc)=="time"),which(names(pc)=="mid"),which(names(pc)=="uid"),which(names(pc)=="content"),which(names(tc)=="comment_count"),which(names(tc)=="like_count"))]

        gbm_model <- gbm(foreward_count~., tcf, distribution = "gaussian", n.trees = 500, bag.fraction = 0.75, cv.folds = 5, interaction.depth = 3)
        #gbm_perf <- gbm.perf(gbm_model, method = "cv")
        #svmf<-svm(foreward_count~.,tcf,type="eps-regression",cross = length(tcf),epsilon=bep[[ti]],cost=bco[[ti]])
        #svmf<-svm(foreward_count~.,tcf,type="eps-regression",cross = NROW(tcf))
    }
    #if(NROW(pc)>0) pcm=predict(gbm_model, newdata = pcf, n.trees = gbm_perf, type = "response")
}
#colnames(tcm)<-c("uid","clust","foreward_count","comment_count","like_count")
#pcu=data.frame(uid=pcm$uid,mid=pcm$mid,clust=pcm$clust)
#if(NROW(pcu)>0)
#{
#r<-merge(pcu,tcm,by=c("uid","clust"),all.x=T)
#r=r[,-which(names(r)=="clust")]
#}
#print(Sys.time())
