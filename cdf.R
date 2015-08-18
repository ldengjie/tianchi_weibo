load("RDataTest")
par(mfcol=c(2,1))
count=t$foreward_count+t$comment_count+t$like_count 
calCount=sort(count)+1
calCount[calCount>100]=100
calValue=cumsum(calCount/sum(calCount))
library(sROC)
plot(kCDF(count))
plot(sort(count),calValue)

#uniCount=unique(count)
#uniCalCount=uniCount+1
#uniCalCount[uniCalCount>100]=100
