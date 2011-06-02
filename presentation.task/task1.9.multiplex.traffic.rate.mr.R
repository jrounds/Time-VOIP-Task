###Test on 20ms intervals
###Obtain bit rate of the multiplexed packet trace for every 20ms interval on each direction
library(Rhipe)
options(digits=18)
rhinit()
rhoptions(runner=sprintf("%s/rhipe.runner.sh",Sys.getenv("HOME")))

m <- expression({
  ifile = Sys.getenv("mapred.input.file")
	#these look like: "hdfs://hadoop-00.rcac.purdue.edu/wsc/voip/iprtp.traces/20040311-095951-0.iprtp.in.gz"
	direction = strsplit(ifile,"\\.")[[1]]
	n = length(direction)
	direction = direction[n-1]
  v <- lapply(seq_along(map.values),function(r) {
    value0 <- strsplit(map.values[[r]]," +")[[1]]
    interval <- floor(as.numeric(value0[1])/0.02)
    key <- c(interval,direction)
    rhcollect(key,T)
  })
})

r <- expression(
  pre = {
    n.pkt<-0
  },
  reduce = {
    n.pkt <- n.pkt+sum(unlist(reduce.values))
  },
  post = {
#    traffic.rate <- n.pkt*1600/0.02
#    rhcollect(reduce.key,traffic.rate)
    rhcollect(reduce.key,n.pkt)
  }
)

mr<-rhmr(map=m,reduce=r,inout=c("text","sequence"),ifolder="/wsc/voip/iprtp.traces",ofolder="/wsc/jrounds/tmp/voip/traffic.rate",jobname="calculate num of pkt per 20ms")

multiplex.trace.bit.rate.mr.time<-system.time(
multiplex.trace.bit.rate.mr<-rhex(mr)
)

rhsave(multiplex.trace.bit.rate.mr,multiplex.trace.bit.rate.mr.time,file="/wsc/jrounds/voip/records/multiplex.trace.bit.rate.20ms.mr.RData")

multiplex.trace.bit.rate.rhread.time<-system.time(
kv.list <- rhread("/wsc/jrounds/tmp/voip/traffic.rate")
)



##Above numbers are too fluctuating. So consider 30s intervals.
m <- expression({
   ifile = Sys.getenv("mapred.input.file")
	#these look like: "hdfs://hadoop-00.rcac.purdue.edu/wsc/voip/iprtp.traces/20040311-095951-0.iprtp.in.gz"
	direction = strsplit(ifile,"\\.")[[1]]
	n = length(direction)
	direction = direction[n-1]
  v <- lapply(seq_along(map.values),function(r) {
    value0 <- strsplit(map.values[[r]]," +")[[1]]
    interval <- floor(as.numeric(value0[1])/30)
    key <- c(interval,direction)
    rhcollect(key,T)
  })
})

r <- expression(
  pre = {
    n.pkt<-0
  },
  reduce = {
    n.pkt <- n.pkt+sum(unlist(reduce.values))
  },
  post = {
#    traffic.rate <- n.pkt*1600/30
#    rhcollect(reduce.key,traffic.rate)
    rhcollect(reduce.key,n.pkt)
  }
)

mr<-rhmr(map=m,reduce=r,inout=c("text","sequence"),ifolder="/wsc/voip/iprtp.traces",ofolder="/wsc/jrounds/tmp/voip/traffic.rate",jobname="calculate num of pkt per 30s")

multiplex.trace.bit.rate.mr.time<-system.time(
multiplex.trace.bit.rate.mr<-rhex(mr)
)

#rhsave(multiplex.trace.bit.rate.mr,multiplex.trace.bit.rate.mr.time,file="/voip/records/multiplex.trace.bit.rate.30s.mr.RData")

multiplex.trace.bit.rate.rhread.time<-system.time(
kv.list <- rhread("/wsc/jrounds/tmp/voip/traffic.rate",doloc=F)
)

##Plot the number of pkts in every 30s
#'in' and 'out' have the same interval range
#Wrong. Didn't sort interval (interval<-interval[ord]) while n.pkt is sorted.



multiplex.trace.bit.rate.rmodify.time<-system.time({
kv <- do.call("rbind",lapply(kv.list,function(r) c(r[[1]],r[[2]])))
id <- (1:nrow(kv))[kv[,2]=="in"]
kv.in<-kv[id,c(1,3)]
kv.out<-kv[-id,c(1,3)]

interval.in<-as.numeric(kv.in[,1])
rate.in<-as.numeric(kv.in[,2])*1600/30
names(rate.in)<-interval.in
ord.in<-order(interval.in)
multiplex.trace.bit.rate.in<-rate.in[ord.in]

interval.out<-as.numeric(kv.out[,1])
rate.out<-as.numeric(kv.out[,2])*1600/30
names(rate.out)<-interval.out
ord.out<-order(interval.out)
multiplex.trace.bit.rate.out<-rate.out[ord.out]
})

multiplex.trace.bit.rate.complete.data.rmodify.time<-system.time({
id<-interval.in[ord.in]>=(1078920300/30)
multiplex.trace.bit.rate.complete.data.in<-multiplex.trace.bit.rate.in[id]
id<-interval.out[ord.out]>=(1078920300/30)
multiplex.trace.bit.rate.complete.data.out<-multiplex.trace.bit.rate.out[id]
})

rhsave(multiplex.trace.bit.rate.mr,multiplex.trace.bit.rate.mr.time,multiplex.trace.bit.rate.rhread.time,multiplex.trace.bit.rate.rmodify.time,multiplex.trace.bit.rate.complete.data.rmodify.time,file="/wsc/jrounds/voip/records/multiplex.trace.bit.rate.30s.mr.RData")

rhsave(multiplex.trace.bit.rate.in,multiplex.trace.bit.rate.out,file="/wsc/jrounds/voip/summary.tables/full.data/multiplex.trace.bit.rate.RData")
rhsave(multiplex.trace.bit.rate.complete.data.in,multiplex.trace.bit.rate.complete.data.out,file="/wsc/jrounds/voip/summary.tables/complete.data/multiplex.trace.bit.rate.complete.data.RData")


quit(save="no")



