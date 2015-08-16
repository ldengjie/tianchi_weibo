print("read predict.txt")
p=read.table("predict.txt",header=T,sep="\t",quote="",comment="")
p=p[,c(2,4,5,6)]

print("read r3.txt")
r3=read.table("3/testr3.txt",header=F,sep="\t",quote="",comment="")
colnames(r3)=c("uid","mid","foreward_count","comment_count","like_count")

print("1")
system("cat ~/file/weibo/job/sub120/135/*weibo_result.txt >  ~/file/weibo/job/sub120/135/135Temp")
print("2")
system("sed -e 's/,/	/g' -e 's/ /	/g'  ~/file/weibo/job/sub120/135/135Temp > ~/file/weibo/job/sub120/135/135.txt")
print("3")
r9=read.table("~/file/weibo/job/sub120/135/135.txt",header=F,sep="\t",quote="",comment="") 
print("4")
colnames(r9)=c("uid","mid","foreward_count","comment_count","like_count")

if(NROW(r9)>NROW(r3)) print("!!!Something was wrong, check it... ")
if(NROW(r9)==NROW(r3)) r=r9
if(NROW(r9)<NROW(r3))
{
    cat("r9<r3 : ",NROW(r9),NROW(r3),"\n")
    rsame=r3[r3$mid%in%r9$mid,]
    rdiff=r3[!r3$mid%in%r9$mid,]

    #rsame3=rsame[,c(1:5)]
    # step 1.merge
    rsame2=merge(rsame,r9,by=c("uid","mid"))
    f=data.frame(rsame2$foreward_count.x,rsame2$foreward_count.y)
    c=data.frame(rsame2$comment_count.x,rsame2$comment_count.y)
    l=data.frame(rsame2$like_count.x,rsame2$like_count.y)

    # clust method
    ff=rsame2$foreward_count.y
    cc=rsame2$comment_count.y
    ll=rsame2$like_count.y
    # delete outlier
    #ff=rsame2$foreward_count.x
    #cc=rsame2$comment_count.x
    #ll=rsame2$like_count.x
    # max
    #ff=apply(f,1,function(x)max(x))
    #cc=apply(c,1,function(x)max(x))
    #ll=apply(l,1,function(x)max(x))
    # min
    #ff=apply(f,1,function(x)min(x))
    #cc=apply(c,1,function(x)min(x))
    #ll=apply(l,1,function(x)min(x))
    # mean
    #ff=apply(f,1,function(x)mean(x))
    #cc=apply(c,1,function(x)mean(x))
    #ll=apply(l,1,function(x)mean(x))
    #
    #nr=data.frame(rsame2$uid,rsame2$mid,rsame2$foreward_count.y,rsame2$comment_count.y,rsame2$like_count.y)
    nr=data.frame(rsame2$uid,rsame2$mid,ff,cc,ll)
    colnames(nr)=c("uid","mid","foreward_count","comment_count","like_count")
    r=rbind(nr,rdiff)
    write.table(r,"135.txt",seq="\t",col.names=F,quote=F)
    
}

print("6")
print(NROW(r))
print(NROW(p))
        rp<-merge(p,r,by=c("mid"))
print(NROW(rp))
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
        cat(">>> precision = ",pre,"\n")
