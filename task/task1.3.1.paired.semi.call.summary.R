library(Rhipe)
options(digits=18)
rhinit()
rhoptions(runner=sprintf("%s/rhipe.runner.sh",Sys.getenv("HOME")))
m<-expression({
  lapply(seq_along(map.values),function(i){
    key0<-map.keys[[i]]
    value0<-map.values[[i]]
    tmp<-strsplit(key0,"\\.")[[1]]
    key<-paste(tmp[1:10],collapse=".")
    n.pkt<-dim(value0)[1]
    start<-value0[1,1]
    end<-value0[n.pkt,1]
    dur<-end-start+0.02
    value<-if(tmp[11]=="in") c(in.start=start,in.end=end,in.dur=dur,in.pkt=n.pkt) else c(out.start=start,out.end=end,out.dur=dur,out.pkt=n.pkt)
#    value<-list(n.pkt,tmp[11])
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
    in.start<-if(!is.null(mydata['in.start'])) mydata['in.start'] else NA
    in.end<-if(!is.null(mydata['in.end'])) mydata['in.end'] else NA
    in.dur<-if(!is.null(mydata['in.dur'])) mydata['in.dur'] else NA
    in.pkt<-if(!is.null(mydata['in.pkt'])) mydata['in.pkt'] else NA
    out.pkt<-if(!is.null(mydata['out.pkt'])) mydata['out.pkt'] else NA
    out.dur<-if(!is.null(mydata['out.dur'])) mydata['out.dur'] else NA
    out.end<-if(!is.null(mydata['out.end'])) mydata['out.end'] else NA
    out.start<-if(!is.null(mydata['out.start'])) mydata['out.start'] else NA
    value<-c(in.start,in.end,in.dur,in.pkt,out.start,out.end,out.dur,out.pkt)
    rhcollect(reduce.key,value)
  }
)

z<-rhmr(map=m,reduce=r,ifolder="/wsc/voip/call.traces",ofolder="/wsc/jrounds/tmp/voip/summary",inout=c("map","sequence"),jobname="paired semi-calls summary")

paired.semi.call.summary.mr.time<-system.time(
paired.semi.call.summary.mr<-rhex(z)
)

#rhsave(paired.semi.call.summary.mr,paired.semi.call.summary.mr.time,file="/voip/records/paired.semi.call.summary.mr.RData")

paired.semi.call.summary.rhread.time<-system.time(
summary <- rhread("/wsc/jrounds/tmp/voip/summary")
)

paired.semi.call.summary.rmodify.time<-system.time({
summary1 <- t(sapply(summary,function(r) r[[2]]))
flow.id <- sapply(summary,function(r) r[[1]])
#paired.semi.call.summary<-data.frame(flow.id,summary1,stringsAsFactors=F)
paired.semi.call.summary<-data.frame(summary1,row.names=flow.id,stringsAsFactors=F)
paired.semi.call.summary$in.pkt<-as.integer(paired.semi.call.summary$in.pkt)
paired.semi.call.summary$out.pkt<-as.integer(paired.semi.call.summary$out.pkt)
paired.semi.call.summary$start<-pmin(paired.semi.call.summary$in.start,paired.semi.call.summary$out.start)
paired.semi.call.summary$end<-pmax(paired.semi.call.summary$in.end,paired.semi.call.summary$out.end)
paired.semi.call.summary$dur<-paired.semi.call.summary$end-paired.semi.call.summary$start+0.02
})

rhsave(paired.semi.call.summary.mr,paired.semi.call.summary.mr.time,paired.semi.call.summary.rhread.time,paired.semi.call.summary.rmodify.time,file="/wsc/jrounds/voip/records/paired.semi.call.summary.RData")

rhsave(paired.semi.call.summary,file="/wsc/jrounds/voip/summary.tables/full.data/paired.semi.call.summary.RData")
#save(paired.semi.call.summary,file="/ln/voip/usr/jxia/RHIPE/summary.tables/paired.semi.call.summary.RData")

#rhget("/voip/summary.tables/paired.semi.call.summary.RData","/ln/voip/usr/jxia/RHIPE/summary.tables")
#rhload("/voip/records/paired.semi.call.summary.RData",.GlobalEnv)

#Old (only num of pkt): Hadoop job_201001121512_0048 on spica
#5mins, 53sec
#deneb failed

#New: Hadoop job_201001121512_0050 on spica
#1mins, 38sec

#start, end, dur, n.pkt: Hadoop job_201001141332_0014 on spica
#1mins, 22sec

#size

