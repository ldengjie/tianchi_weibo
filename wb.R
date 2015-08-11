#Rscript wb.R
#print(Sys.time())
#cat("> read data \n")
#t<-read.table("weibo_train_data.txt",header=T,sep="\t",quote="\n") 
#p<-read.table("weibo_predict_data.txt",header=T,sep="\t",quote="\n")
#load("RDataTotal")
#t<-read.table("train.txt",header=T,sep="\t",quote="\n") 
#p<-read.table("predict.txt",header=T,sep="\t",quote="\n")
load("RDataTest")
names(p)=c("uid","mid","time","foreward_count","comment_count","like_count","content")
#t<-read.table("t.txt",header=T,sep="\t",quote="\n") 
#p<-read.table("p.txt",header=T,sep="\t",quote="\n")

#p=p[p$uid=="82990720c0936340c7a17e712f549b30",]
#t=t[t$uid=="82990720c0936340c7a17e712f549b30",]
#p=p[p$uid=="3df25e570db062bab9cbf0782cf09630",]
#t=t[t$uid=="3df25e570db062bab9cbf0782cf09630",]



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

if(1)
{
library(rJava)
library(Rwordseg)
library("stringr")
doc=c(as.character(t$content),as.character(p$content)) 
doc=gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","",doc)                            
tag=str_extract(doc,"#.+?#") 
tag=na.omit(tag)  #去除NA
tag=unique(tag)    #去重
if(length(tag)>0) insertWords(tag)
docSeg=segmentCN(doc)
#detach("package:Rwordseg", unload=TRUE)
library("tm")
docCor=Corpus(VectorSource(docSeg))
# remove numbers
#docCor=tm_map(docCor, removeNumbers)
# remove stop words
stw=read.table(file="dict/stopWord.txt",quote="",colClasses="character")
docCor=tm_map(docCor,tm::removeWords,stw[,1])
control=list(removePunctuation=T,minDocFreq=5,wordLengths = c(1, Inf),weighting = weightTfIdf)
docTdm=TermDocumentMatrix(docCor,control)
#length(docTdm$dimnames$Terms)
#tdm_removed=removeSparseTerms(docTdm, 0.9998) # 1-去除了低于 99.98% 的稀疏条目项
#length(tdm_removed$dimnames$Terms)
#clust
dist_tdm <- proxy::dist(t(as.matrix(docTdm)), method = 'cosine')
hc <- hclust(dist_tdm, method = 'mcquitty')
ctNum=20
ct = cutree(hc,k=ctNum)
#doc=cbind(doc,clust=ct,fm=rep(NA,NROW(ct),cm=rep(NA,NROW(ct),lm=rep(NA,NROW(ct))
tc=cbind(clust=ct[1:NROW(t)],t)
pc=cbind(clust=ct[(1+NROW(t)):NROW(doc)],p)
#7
tp<-split(tc,f=as.factor(tc$uid))
tcm=NULL
print(NROW(tcm))
for(tpi in 1:NROW(tp))
{
    tu=tp[tpi][[1]]
    if(NROW(tu)==0) next
    tu2=tu
    cat(">>>>>> ",tpi,"\n")
    foreward_mean=rep(0,ctNum)
    comment_mean=rep(0,ctNum)
    like_mean=rep(0,ctNum)
    for(cti in 1:ctNum)
    {
        foreward_mean[cti]=as.integer(mean(na.omit(tu$foreward_count[tu$clust==cti])))
        comment_mean[cti]=as.integer(mean(na.omit(tu$comment_count[tu$clust==cti])))
        like_mean[cti]=as.integer(mean(na.omit(tu$like_count[tu$clust==cti])))
        cat(foreward_mean[cti],comment_mean[cti],like_mean[cti],"\n")
    }
    print(rep(tu$uid[1],ctNum))
    print(seq(1:ctNum))
    print(foreward_mean)
    print(comment_mean)
    print(like_mean)
    tcm=rbind(tcm,data.frame(rep(as.character(tu$uid[1]),ctNum),seq(1:ctNum),foreward_mean,comment_mean,like_mean))
}
print(NROW(tcm))
cat("1\n")
colnames(tcm)<-c("uid","clust","foreward_count","comment_count","like_count")
cat("2\n")
}

funl=c(izero,rmOutlier,rmOutlier2,rmOutlierMorethanZero,rmOutlierMorethanZero2,imean)
for(fi in 7:7)
{

    if(fi<7)
    {
        tm<-aggregate(t,by=list(t$uid),FUN=funl[[fi]])[,c(1,5:7)]
        names(tm)<-c("uid","foreward_count","comment_count","like_count")
        pu<-p[,1:2]
        r<-merge(pu,tm,by=c("uid"),all.x=T)
    }else
    {
        #pcu=pc[,1:3]
        #r<-merge(pcu,tcm,by=c("uid","clust"),all.x=T)
    }

cat("3\n")
    #print(Sys.time())
    #cat("> assign vaule to predict data \n")
    #print(Sys.time())
    #cat("> assign 0 to new uid \n")
    r$foreward_count[is.na(r$foreward_count)]=0
    r$comment_count[is.na(r$comment_count)]=0
    r$like_count[is.na(r$like_count)]=0
    #save into .txt
    #write.csv(r,"r.txt")
    #linux \t for tabs
    #system("sed -e 's#^\"[0-9]*\",##g' -e 's#\",#\t#g' -e 's#\"##g' -e '/_/d' r.txt > weibo_result.txt")
    #OS X ctrl+v+tab for tabs
    #if(fi==1) system("sed -e 's#^\"[0-9]*\",##g' -e 's#\",#	#g' -e 's#\"##g' -e '/_/d' r.txt > 1/weibo_result.txt")
    #if(fi==2) system("sed -e 's#^\"[0-9]*\",##g' -e 's#\",#	#g' -e 's#\"##g' -e '/_/d' r.txt > 2/weibo_result.txt")
    #if(fi==6) system("sed -e 's#^\"[0-9]*\",##g' -e 's#\",#	#g' -e 's#\"##g' -e '/_/d' r.txt > 6/weibo_result.txt")
    #print(Sys.time())
    #cat("> Calculating pricision , only for test \n")
    if(1)
    {
        devf=abs(r$foreward_count-p$foreward_count)/(p$foreward_count+5)
        devc=abs(r$comment_count-p$comment_count)/(p$comment_count+3)
        devl=abs(r$like_count-p$like_count)/(p$like_count+3)
        prei=1-0.5*devf-0.25*devc-0.25*devl
        pret=prei
        prei[(prei-0.8)>0]=1
        prei[(prei-0.8)<=0]=0
        count=(p$foreward_count+p$comment_count+p$like_count+1)
        pre=sum(count*prei)/sum(count)
        #print(Sys.time())
        cat(">>> precision = ",pre,"\n")
        #print(Sys.time())
    }
}
