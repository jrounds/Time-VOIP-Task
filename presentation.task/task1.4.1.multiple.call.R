library(Rhipe)
options(digits=18)
rhinit()
rhoptions(runner=sprintf("%s/rhipe.runner.sh",Sys.getenv("HOME")))
#Semi call gaps
m<-expression({
  lapply(seq_along(map.values),function(i){
    key0<-map.keys[[i]]
    value0<-map.values[[i]]
    arrival<-value0$time
    max.ia<-if(length(arrival)>1) max(diff(arrival)) else NA
    rhcollect(key0,max.ia)
  })
})

z<-rhmr(map=m,ifolder="/wsc/voip/call.traces",ofolder="/wsc/jrounds/tmp/voip/F1",inout=c("map","sequence"),jobname="semi-call max interarrival")

semi.call.max.ia.mr.time<-system.time(
semi.call.max.ia.mr<-rhex(z)
)

#rhsave(semi.call.max.ia.mr,semi.call.max.ia.mr.time,file="/voip/records/semi.call.max.ia.mr.RData")

kv <- rhread("/wsc/jrounds/tmp/voip/F1",doloc=F)

kv1 <- do.call("rbind",lapply(kv,function(r) c(r[[1]],r[[2]])))
#kv1 <- t(sapply(kv,function(r) c(r[[1]],r[[2]])))
semi.call.max.ia<-data.frame(flow.id=kv1[,1],max.ia=as.numeric(kv1[,2]),stringsAsFactors=F)

multiple.semi.call.id<-semi.call.max.ia$flow.id[semi.call.max.ia$max.ia>=3600 & !is.na(semi.call.max.ia$max.ia)]
#tmp<-strsplit(multiple.semi.call.id,"\\.")
#tmp1<-sapply(tmp,function(s) paste(s[1:10],collapse="."))
#n<-table(tmp1)
#multiple.call.id<-names(n)[n>1]

#Call gaps
m<-expression({
  lapply(seq_along(map.values),function(i){
    key0<-map.keys[[i]]
    value0<-map.values[[i]]
    tmp<-strsplit(key0,"\\.")[[1]]
    key<-paste(tmp[1:10],collapse=".")
    value<-value0$time
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
    mydata<-unlist(mydata)
    arrival<-mydata[order(mydata)]
    max.ia<-if(length(arrival)>1) max(diff(arrival)) else NA
    rhcollect(reduce.key,max.ia)
  }
)

z<-rhmr(map=m,reduce=r,ifolder="/wsc/voip/call.traces",ofolder="/wsc/jrounds/tmp/voip/call.gaps",inout=c("map","sequence"),jobname="call max interarrival")

call.max.ia.mr.time<-system.time(
call.max.ia.mr<-rhex(z)
)

call.max.ia.rhread.time<-system.time(
kv <- rhread("/wsc/jrounds/tmp/voip/call.gaps",doloc=F)
)

call.max.ia.rmodify.time<-system.time({
kv1 <- do.call("rbind",lapply(kv,function(r) c(r[[1]],r[[2]])))
call.max.ia<-data.frame(flow.id=kv1[,1],max.ia=as.numeric(kv1[,2]),stringsAsFactors=F)
})

multiple.call.time<-system.time(
multiple.call.id<-call.max.ia$flow.id[call.max.ia$max.ia>=3600 & !is.na(call.max.ia$max.ia)]
)

rhsave(semi.call.max.ia.mr,semi.call.max.ia.mr.time,call.max.ia.mr,call.max.ia.mr.time,call.max.ia.rhread.time,call.max.ia.rmodify.time,multiple.call.time,file="/wsc/jrounds/voip/records/multiple.call.mr.RData")

rhsave(semi.call.max.ia,multiple.semi.call.id,call.max.ia,multiple.call.id,file="/wsc/jrounds/voip/summary.tables/full.data/multiple.call.RData")

#rhload("/voip/summary.tables/full.data/multiple.call.RData",.GlobalEnv)

#26 multiple calls, including 67.17.50.213.12454.67.17.50.6.17754 which was not excluded
#May want to consider one more call, 67.17.50.213.15202.64.208.196.170.8322, with 17min gaps.

quit(save="no")


