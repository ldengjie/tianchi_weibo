load("RDataTotal")

t1=rbind(data.frame(t[,c(1,2,3)]),data.frame(p[,c(1,2,3)]))
month=as.numeric(format(as.Date(t1$time),"%m"))
t2=data.frame(t1,month=month)

#            1 2 3 4  5  6  7
monthlist=c( 7,8,9,10,11,12,1)
mi=5# mi<=6
pastt=t2[t2$month%in%monthlist[1:mi],]
nowt=t2[t2$month==monthlist[mi+1],]

pastu=as.data.frame(table(pastt$uid))
pastuid=pastu[pastu$Freq>0,1]

nowu=as.data.frame(table(nowt$uid))
nowuid=nowu[nowu$Freq>0,1]

newuid=nowuid[!nowuid%in%pastuid]
print(NROW(newuid))

newWeibo=nowt[nowt$uid%in%newuid,] #uid,mid,time

print(NROW(newWeibo))
WholeNewWeibo=t[t$mid%in%newWeibo$mid,]#uid,mid,time,f,c,l,content
count=WholeNewWeibo$foreward_count+WholeNewWeibo$comment_count+WholeNewWeibo$like_count

nu=data.frame(table(WholeNewWeibo$uid))
nu=nu[nu$Freq>0,]
mu<-aggregate(WholeNewWeibo,by=list(WholeNewWeibo$uid),FUN=mean)[,c(1,5:7)]
inum=function(x)
{
    if(is.factor(x))
    {
        result=NA
    }else
    {
        result=NROW(x)
    }
    return(result)
}
ms<-aggregate(WholeNewWeibo,by=list(WholeNewWeibo$uid),FUN=inum)[,c(1,5:7)]
