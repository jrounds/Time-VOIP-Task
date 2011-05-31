library(Rhipe)
options(digits=18)

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

z<-rhmr(map=m,reduce=r,ifolder="/wsc/voip/call.traces",ofolder="/wsc/jrounds/voip.timings/summary",inout=c("map","sequence"),jobname="paired semi-calls summary")

time<-system.time(paired.semi.call.summary.mr<-rhex(z))


