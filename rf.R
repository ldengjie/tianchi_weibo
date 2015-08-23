
load("RDdataTotal")

#time
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


#

