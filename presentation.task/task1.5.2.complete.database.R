library(Rhipe)
options(digits=18)
rhinit()
rhoptions(runner=sprintf("%s/rhipe.runner.sh",Sys.getenv("HOME")))
###Create sub-database (complete data)
##Method 1
#1
m <- expression({
  keys<-unlist(map.keys)
  id<-seq_along(keys)
  v<-lapply(id,function(x){
    rhcollect(map.keys[[x]],map.values[[x]])
  })
})

mr<-rhmr(map=m,inout=c("map","map"),ifolder="/wsc/voip/call.traces",ofolder='/wsc/jrounds/tmp/voip/complete.data',jobname="complete data 1a")

complete.database.mr1a.time<-system.time(
complete.database.mr1a<-rhex(mr)
)

#2
mapsetup=expression({
load("complete.semi.call.RData") #Specify full local path if run local jobs, share won't work.
})

m <- expression({
  keys<-unlist(map.keys)
  id<-seq_along(keys)
  v<-lapply(id,function(x){
    rhcollect(map.keys[[x]],map.values[[x]])
  })
})

mr<-rhmr(map=m,setup=list(map=mapsetup),inout=c("map","map"),ifolder="/wsc/voip/call.traces",ofolder='/wsc/jrounds/tmp/voip/complete.data',shared="/wsc/voip/summary.tables/subdataset.index/complete.semi.call.RData",jobname="complete data 1b")

complete.database.mr1b.time<-system.time(
complete.database.mr1b<-rhex(mr)
)

#3
mapsetup=expression({
load("complete.semi.call.RData") #Specify full local path if run local jobs, share won't work.
})

m <- expression({
  keys<-unlist(map.keys)
  id<-seq_along(keys)
  id1<-id[keys %in% complete.semi.call]
  v<-lapply(id,function(x){
    rhcollect(map.keys[[x]],map.values[[x]])
  })
})

mr<-rhmr(map=m,setup=list(map=mapsetup),inout=c("map","map"),ifolder="/wsc/voip/call.traces",ofolder='/wsc/jrounds/tmp/voip/complete.data',shared="/wsc/voip/summary.tables/subdataset.index/complete.semi.call.RData",jobname="complete data 1c")

complete.database.mr1c.time<-system.time(
complete.database.mr1c<-rhex(mr)
)

#4
mapsetup=expression({
load("complete.semi.call.RData") #Specify full local path if run local jobs, share won't work.
})

m <- expression({
  keys<-unlist(map.keys)
#  id<-seq_along(keys)
#  id<-id[keys %in% complete.semi.call]
  id<-seq_along(keys)[keys %in% complete.semi.call]
  v<-lapply(id,function(x){
    rhcollect(map.keys[[x]],map.values[[x]])
  })
})

mr<-rhmr(map=m,setup=list(map=mapsetup),inout=c("map","map"),ifolder="/wsc/voip/call.traces",ofolder='/wsc/jrounds/voip/complete.data',shared="/wsc/voip/summary.tables/subdataset.index/complete.semi.call.RData",jobname="complete data 1d")

complete.database.mr1d.time<-system.time(
complete.database.mr1d<-rhex(mr)
)

#kv <- rhread(rhmap.sqs("/voip/call.traces/p*"),doloc=F)
#kv <- rhread(rhmap.sqs("/voip/complete.data/p*"),doloc=F)
#kv1 <- lapply(kv,function(r) r[[1]])

#Complete map to map
m <- expression({
  keys<-unlist(map.keys)
  id<-seq_along(keys)
  v<-lapply(id,function(x){
    rhcollect(map.keys[[x]],map.values[[x]])
  })
})

mr<-rhmr(map=m,inout=c("map","map"),ifolder="/wsc/voip/complete.data",ofolder='/wsc/jrounds/tmp/voip/complete.data',jobname="complete data m2m")

complete.database.mr.m2m.time<-system.time(
complete.database.mr.m2m<-rhex(mr)
)

#Using S2M
mapsetup=expression({
load("complete.semi.call.RData") #Specify full local path if run local jobs, share won't work.
})

m <- expression({
  keys<-unlist(map.keys)
#  id<-seq_along(keys)
#  id<-id[keys %in% complete.semi.call]
  id<-seq_along(keys)[keys %in% complete.semi.call]
  v<-lapply(id,function(x){
    rhcollect(map.keys[[x]],map.values[[x]])
  })
})

mr<-rhmr(map=m,setup=list(map=mapsetup),inout=c("map","sequence"),ifolder="/wsc/voip/call.traces",ofolder='/wsc/jrounds/tmp/voip/complete.data',shared="/wsc/voip/summary.tables/subdataset.index/complete.semi.call.RData",mapred=list(mapred.reduce.tasks=0),jobname="complete data 1d with rhS2M")

complete.database.mr1d.s2m.time<-system.time({
complete.database.mr1d.s2m<-rhex(mr)
#rhS2M("/tmp/voip/complete.data/p*","/tmp/jxia/complete.data",dolocal=F)
}
)

##Method 2
mapsetup=expression({
load("multiple.call.RData") #Specify full local path if run local jobs, share won't work.
})

m<-expression({
  lapply(seq_along(map.values),function(i){
    key0<-map.keys[[i]]
    tmp<-strsplit(key0,"\\.")[[1]]
    key<-paste(tmp[1:10],collapse=".")
    value<-list(dir=tmp[11],map.values[[i]])
    rhcollect(key,value)
  })
})

r<-expression(
  pre={
    mydata<-list()
  },
  reduce={
    mydata<-append(mydata,reduce.values)
  },
  post={
    if((length(mydata)==2) & !(reduce.key %in% multiple.call.id)){
      pkt1<-dim(mydata[[1]][[2]])[1]
      pkt2<-dim(mydata[[2]][[2]])[1]
      if(pkt1>20 | pkt2>20){
        dir1<-mydata[[1]][[1]]
        dir2<-mydata[[2]][[1]]
	key1<-paste(reduce.key,dir1,sep=".")
	key2<-paste(reduce.key,dir2,sep=".")
	rhcollect(key1,mydata[[1]][[2]])
	rhcollect(key2,mydata[[2]][[2]])
      }
    }
  }
)

mr<-rhmr(map=m,setup=list(reduce=mapsetup),reduce=r,ifolder="/wsc/voip/call.traces",ofolder="/wsc/jrounds/tmp/voip/F2",inout=c("map","sequence"),shared="/tmp/jxia/summary.tables/multiple.call.RData",jobname="complete data 2")

complete.database.mr2.time<-system.time(
{complete.database.mr2 = NA} #complete.database.mr2<-rhex(mr) not executed need to replace /tmp/jxia/summary.tables/multiple.call.Rdata
)

rhsave(complete.database.mr1a.time,complete.database.mr1a,complete.database.mr1b.time,complete.database.mr1b,complete.database.mr1c.time,complete.database.mr1c,complete.database.mr1d.time,complete.database.mr1d,complete.database.mr.m2m.time,complete.database.mr.m2m,complete.database.mr1d.s2m.time,complete.database.mr1d.s2m,complete.database.mr2.time,complete.database.mr2,file="/wsc/jrounds/voip/records/complete.database.mr.RData")




quit(save="no")


