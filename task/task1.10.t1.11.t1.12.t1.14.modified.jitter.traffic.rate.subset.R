library(Rhipe)
options(digits=18)
rhinit()
rhoptions(runner=sprintf("%s/rhipe.runner.sh",Sys.getenv("HOME")))
###1. Match each jitter value with traffic rate of the interval its first arrival lies in
##1.1 Figure out number of jitter values in each traffic rate group
##Also number of objects generated for each traffic rate group

mapsetup=expression({
load("multiplex.trace.bit.rate.complete.data.RData")
})

m<-expression({
  v<-lapply(seq_along(map.values),function(r) {
    value0<-map.values[[r]]
    key0<-map.keys[[r]]

    arrival1<-value0$arrival1
    jitter<-value0$jitter
    interval<-floor(arrival1/30)
    
    semi.call.id<-key0[1]
    direction<-strsplit(semi.call.id,"\\.")[[1]][11]

    n.value<-table(interval)
    tmp.interval<-names(n.value)
    tmp.traffic.rate<-if(direction=="in") as.character(multiplex.trace.bit.rate.complete.data.in[tmp.interval]) else as.character(multiplex.trace.bit.rate.complete.data.out[tmp.interval])

    w<-lapply(seq_along(tmp.traffic.rate),function(i){
      value<-n.value[i]
      key<-c(tmp.traffic.rate[i],direction)
      rhcollect(key,value)
    })
  })
})

r<-expression(
  pre={
    n.value<-0
    n.object<-0
  },
  reduce={
    n.value<-n.value+sum(unlist(reduce.values))
    n.object<-n.object+length(reduce.values)
  },
  post={
    rhcollect(reduce.key,c(n.value,n.object))
  }
)

mr<-rhmr(map=m,reduce=r,setup=list(map=mapsetup),inout=c("map","sequence"),ifolder="/wsc/voip/modified.jitter.database",ofolder="/wsc/jrounds/tmp/voip/traffic.rate",shared="/wsc/voip/summary.tables/complete.data/multiplex.trace.bit.rate.complete.data.RData",jobname="count traffic rate group size")

count.traffic.rate.group.size.mr.time<-system.time(
count.traffic.rate.group.size.mr<-rhex(mr)
)

count.traffic.rate.group.size.rhread.time<-system.time(
kv.list<-rhread("/wsc/jrounds/tmp/voip/traffic.rate",doloc=F)
)

count.traffic.rate.group.size.rmodify.time<-system.time({
kv<-do.call("rbind",lapply(kv.list,function(r) c(r[[1]],r[[2]])))
ord<-order(as.numeric(kv[,1]),kv[,2])
kv<-kv[ord,]
traffic.rate.group.size<-data.frame(traffic.rate=as.numeric(kv[,1]),direction=kv[,2],n.value=as.integer(kv[,3]),n.object=as.integer(kv[,4]),stringsAsFactors=F)
id<-traffic.rate.group.size$direction=="in"
tmp.in<-traffic.rate.group.size[id,c(1,3)]
traffic.rate.group.size.in<-tmp.in[,2]
names(traffic.rate.group.size.in)<-tmp.in[,1]
traffic.rate.cum.group.size.in<-cumsum(traffic.rate.group.size.in)
traffic.rate.cum.pre.group.size.in<-c(0,traffic.rate.cum.group.size.in[-length(traffic.rate.cum.group.size.in)])
names(traffic.rate.cum.pre.group.size.in)<-tmp.in[,1]
tmp.out<-traffic.rate.group.size[!id,c(1,3)]
traffic.rate.group.size.out<-tmp.out[,2]
names(traffic.rate.group.size.out)<-tmp.out[,1]
traffic.rate.cum.group.size.out<-cumsum(traffic.rate.group.size.out)
traffic.rate.cum.pre.group.size.out<-c(0,traffic.rate.cum.group.size.out[-length(traffic.rate.cum.group.size.out)])
names(traffic.rate.cum.pre.group.size.out)<-tmp.out[,1]
})

rhsave(count.traffic.rate.group.size.mr,count.traffic.rate.group.size.mr.time,count.traffic.rate.group.size.rhread.time,count.traffic.rate.group.size.rmodify.time,file="/wsc/jrounds/voip/records/count.traffic.rate.group.size.mr.RData")

#rhsave(traffic.rate.group.size,file="/voip/summary.tables/modified.jitter.database/traffic.rate.group.size.RData")
#rhsave(traffic.rate.group.size.in,traffic.rate.group.size.out,file="/voip/summary.tables/modified.jitter.database/traffic.rate.group.size.by.direction.RData")
#rhsave(traffic.rate.cum.group.size.in,traffic.rate.cum.group.size.out,file="/voip/summary.tables/modified.jitter.database/traffic.rate.cum.group.size.by.direction.RData")
#rhsave(traffic.rate.cum.pre.group.size.in,traffic.rate.cum.pre.group.size.out,file="/voip/summary.tables/modified.jitter.database/traffic.rate.cum.pre.group.size.by.direction.RData")

##1.2 Match each jitter value with traffic rate
##Number of subsets: 
##in: traffic.rate.cum.group.size.in[length(traffic.rate.cum.group.size.in)]/10000=45618.0965 -- 45618 subsets
##out: traffic.rate.cum.group.size.out[length(traffic.rate.cum.group.size.out)]/10000=42927.8095 -- 42927 subsets

map.setup=expression({
load("multiplex.trace.bit.rate.complete.data.RData")
})

reduce.setup=expression({
load("traffic.rate.cum.pre.group.size.by.direction.RData")
})

m<-expression({
  v<-lapply(seq_along(map.values),function(r) {
    value0<-map.values[[r]]
    key0<-map.keys[[r]]

    arrival1<-value0$arrival1
    jitter<-value0$jitter
    interval<-floor(arrival1/30)
    
    semi.call.id<-key0[1]
    direction<-strsplit(semi.call.id,"\\.")[[1]][11]
    traffic.rate<-get(paste("multiplex.trace.bit.rate.complete.data",direction,sep="."))

    tmp.value<-split(jitter,interval)
    tmp.interval<-names(tmp.value)
    tmp.traffic.rate<-as.character(traffic.rate[tmp.interval])
    
    w<-lapply(seq_along(tmp.value),function(i){
      value<-tmp.value[[i]]
      key<-c(tmp.traffic.rate[i],direction)
      rhcollect(key,value)
    })
  })
})

##in: 45618 subsets
##out: 42927 subsets
r<-expression(
  pre={
    traffic.rate<-reduce.key[1]
    direction<-reduce.key[2]
    mydata<-NULL
    cum.pre.group.size<-get(paste("traffic.rate.cum.pre.group.size",direction,sep="."))
    n0<-cum.pre.group.size[traffic.rate]
    n.subset<-if(direction=="in") 45618 else 42927
  },
  reduce={
    mydata<-unlist(reduce.values)
    n<-length(mydata)
    subset<-(1:n+n0-1)%%n.subset+1
    value<-data.frame(jitter=as.numeric(mydata),subset=as.integer(subset))
    rhcollect(reduce.key,value)
    n0<-n0+n
  }
)

mr<-rhmr(map=m,reduce=r,setup=list(map=map.setup,reduce=reduce.setup),inout=c("map","map"),ifolder="/wsc/voip/modified.jitter.database",ofolder="/wsc/jrounds/tmp/voip/modified.jitter.subset",shared=c("/wsc/voip/summary.tables/complete.data/multiplex.trace.bit.rate.complete.data.RData","/wsc/voip/summary.tables/modified.jitter.database/traffic.rate.cum.pre.group.size.by.direction.RData"),jobname="match jitter with traffic rate")

match.jitter.traffic.rate.mr.time<-system.time(
match.jitter.traffic.rate.mr<-rhex(mr)
)

#size
#n<-c(paste(0,0:9,sep=""),10:77)
#file<-paste("/tmp/voip/modified.jitter.subset/part-r-000",n,"/data",sep="")
#size.list<-lapply(file,function(f) rhls(f))
#size<-do.call("rbind",size.list)
#total.size<-sum(size$size)

###2. Create subsets
m<-expression({
  v<-lapply(seq_along(map.values),function(r) {
    value0<-map.values[[r]]
    key0<-map.keys[[r]]

    jitter<-value0$jitter
    subset<-value0$subset
    traffic.rate<-key0[1]
    direction<-key0[2]
    
    tmp.value<-split(jitter,subset)
    tmp.subset<-as.integer(names(tmp.value))
    
    w<-lapply(seq_along(tmp.value),function(i){
      value<-cbind(tmp.value[[i]],traffic.rate)
      key<-c(tmp.subset[i],direction)
      rhcollect(key,value)
    })
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
    mydata<-do.call("rbind",mydata)
    value<-data.frame(jitter=as.numeric(mydata[,1]),traffic.rate=as.numeric(mydata[,2]))
    rhcollect(reduce.key,value)
  }
)

mr<-rhmr(map=m,reduce=r,inout=c("map","map"),ifolder="/wsc/jrounds/tmp/voip/modified.jitter.subset/",ofolder="/wsc/jrounds/voip/modified.jitter.traffic.rate.subset",jobname="create jitter subsets")

generate.jitter.traffic.rate.subset.mr.time<-system.time(
generate.jitter.traffic.rate.subset.mr<-rhex(mr)
)

rhsave(match.jitter.traffic.rate.mr,match.jitter.traffic.rate.mr.time,generate.jitter.traffic.rate.subset.mr,generate.jitter.traffic.rate.subset.mr.time,file="/wsc/jrounds/voip/records/modified.jitter.traffic.rate.subset.mr.RData")












