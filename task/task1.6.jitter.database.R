library(Rhipe)
options(digits=18)
rhinit()
rhoptions(runner=sprintf("%s/rhipe.runner.sh",Sys.getenv("HOME")))

###Check whether each transmission interval is complete, i.e. last packet is a silence packet
m<-expression({
  lapply(seq_along(map.values),function(i){
    key0<-map.keys[[i]]
    value0<-map.values[[i]]
    n.pkt<-dim(value0)[1]
    flag<-value0[n.pkt,2]
    value<-flag==0 | flag==8
    rhcollect(key0,value)
  })
})

z<-rhmr(map=m,ifolder="/wsc/voip/complete.data",ofolder="/wsc/jrounds/tmp/voip/F3",inout=c("map","sequence"),mapred=list(mapred.reduce.tasks=0),jobname="completeness of transmission intervals")

on.interval.complete.mr.time<-system.time(
on.interval.complete.mr<-rhex(z)
)

#rhsave(on.interval.complete.mr,on.interval.complete.mr.time,file="/voip/records/on.interval.complete.mr.RData")

#kv <- rhread("/wsc/jrounds/tmp/voip/F3",doloc=F)
#kv1 <- do.call("rbind",lapply(kv,function(r) c(r[[1]],r[[2]])))
#semi.call.incomplete.on.interval<-data.frame(flow.id=kv1[,1],incomplete=as.logical(kv1[,2]),stringsAsFactors=F)

#rhsave(semi.call.incomplete.on.interval,file="/wsc/jrounds/voip/summary.tables/complete.data/semi.call.incomplete.on.interval.RData")


###Generate jitter database
##1st Method
m<-expression({
  lapply(seq_along(map.values),function(i){
    key0<-map.keys[[i]]
    value0<-map.values[[i]]
    n.pkt<-dim(value0)[1]
    if(n.pkt>1){
      arrival<-value0$time
      flag<-value0$rtpPT==0 | value0$rtpPT==8
      tmp.jitter<-diff(arrival)
      tmp.arrival1<-arrival[-n.pkt]
      tmp.flag1<-flag[-n.pkt]
      tmp.flag2<-flag[2:n.pkt]
      tmp.on<-tmp.flag1 & tmp.flag2
      jitter<-round(tmp.jitter[tmp.on]-0.02,6)*1000  #jitter is in ms
      arrival1<-tmp.arrival1[tmp.on]
      tmp.id<-diff(c(F,tmp.on,F))
      on.start<-seq_along(tmp.id)[tmp.id==1]
      on.end<-seq_along(tmp.id)[tmp.id==-1]
      if(length(on.start)>0){
        group<-rep(seq_along(on.start),on.end-on.start)
        lapply(seq_along(group),function(i){
          key<-c(key0,group[i])
	  value<-c(arrival1[i],jitter[i])
          rhcollect(key,value)
        })
      }
    }
  })
})

r<-expression(
  pre = {
    mydata<-list()
  },
  reduce = {
    mydata <- append(mydata,reduce.values)
  },
  post = {
    mydata <- do.call("rbind",mydata)
    colnames(mydata) <- c("arrival1","jitter")
    mydata <- mydata[order(mydata[,'arrival1']),,drop=F]
    mydata <- data.frame(arrival1 = as.numeric(mydata[,'arrival1']),
                         jitter = as.numeric(mydata[,'jitter']))
    rhcollect(reduce.key,mydata)
  }
)

z<-rhmr(map=m,reduce=r,ifolder="/wsc/voip/complete.data",ofolder="/wsc/jrounds/voip/jitter.database",inout=c("map","map"),jobname="jitter database 1st method")
#z<-rhmr(map=m,reduce=r,ifolder=rhmap.sqs("/voip/complete.data/p*"),ofolder="/tmp/voip/jitter.database",inout=c("sequence","map"),jobname="jitter database 1st method")
#z<-rhmr(map=m,reduce=r,ifolder="/voip/complete.data/part-r-00000/data",ofolder="/tmp/voip/jitter.database",inout=c("sequence","map"),jobname="jitter database 1st method")

jitter.database.mr1.time<-system.time(
jitter.database.mr1<-rhex(z)
)

#Without rhcollect
m<-expression({
  lapply(seq_along(map.values),function(i){
    key0<-map.keys[[i]]
    value0<-map.values[[i]]
    n.pkt<-dim(value0)[1]
    if(n.pkt>1){
      arrival<-value0$time
      flag<-value0$rtpPT==0 | value0$rtpPT==8
      tmp.jitter<-diff(arrival)
      tmp.arrival1<-arrival[-n.pkt]
      tmp.flag1<-flag[-n.pkt]
      tmp.flag2<-flag[2:n.pkt]
      tmp.on<-tmp.flag1 & tmp.flag2
      jitter<-round(tmp.jitter[tmp.on]-0.02,6)*1000  #jitter is in ms
      arrival1<-tmp.arrival1[tmp.on]
      tmp.id<-diff(c(F,tmp.on,F))
      on.start<-seq_along(tmp.id)[tmp.id==1]
      on.end<-seq_along(tmp.id)[tmp.id==-1]
      if(length(on.start)>0){
        group<-rep(seq_along(on.start),on.end-on.start)
        lapply(seq_along(group),function(i){
          key<-c(key0,group[i])
	  value<-c(arrival1[i],jitter[i])
#          rhcollect(key,value)
        })
      }
    }
  })
})

r<-expression(
  pre = {
    mydata<-list()
  },
  reduce = {
    mydata <- append(mydata,reduce.values)
  },
  post = {
    mydata <- do.call("rbind",mydata)
    colnames(mydata) <- c("arrival1","jitter")
    mydata <- mydata[order(mydata[,'arrival1']),,drop=F]
    mydata <- data.frame(arrival1 = as.numeric(mydata[,'arrival1']),
                         jitter = as.numeric(mydata[,'jitter']))
    rhcollect(reduce.key,mydata)
  }
)

z<-rhmr(map=m,reduce=r,ifolder="/wsc/voip/complete.data",ofolder="/wsc/jrounds/tmp/voip/jitter.database",inout=c("map","map"),jobname="jitter database 1st method no rhcollect")

jitter.database.mr1.norhcol.time<-system.time(
jitter.database.mr1.norhcol<-rhex(z)
)

##2nd Method
m<-expression({
  lapply(seq_along(map.values),function(i){
    key0<-map.keys[[i]]
    value0<-map.values[[i]]
    n.pkt<-dim(value0)[1]
    rhcounter("X","foo",1)
    if(n.pkt>1){
      arrival<-value0$time
      flag<-value0$rtpPT==0 | value0$rtpPT==8
      tmp.jitter<-diff(arrival)
      tmp.arrival1<-arrival[-n.pkt]
      tmp.flag1<-flag[-n.pkt]
      tmp.flag2<-flag[2:n.pkt]
      tmp.on<-tmp.flag1 & tmp.flag2
      jitter<-round(tmp.jitter[tmp.on]-0.02,6)*1000  #jitter is in ms
      arrival1<-tmp.arrival1[tmp.on]
      tmp.id<-diff(c(F,tmp.on,F))
      on.start<-seq_along(tmp.id)[tmp.id==1]
      on.end<-seq_along(tmp.id)[tmp.id==-1]
      if(length(on.start)>0){
        group0<-seq_along(on.start)
        group<-rep(group0,on.end-on.start)
	tmp.value<-split(data.frame(arrival1,jitter),group)
	lapply(group0,function(r){
	  key<-c(key0,r)
	  value<-tmp.value[[r]]
	  row.names(value)<-1:dim(value)[1]
          rhcounter("X","n.pkt",1)	
	  rhcollect(key,value)
	})
      }
    }
  })
})

z<-rhmr(map=m,ifolder="/wsc/voip/complete.data",ofolder="/wsc/jrounds/tmp/voip/jitter.database",inout=c("map","map"),jobname="jitter database 2nd method")

jitter.database.mr2.time<-system.time(
jitter.database.mr2<-rhex(z)
)

#Without rhcollect
m<-expression({
  lapply(seq_along(map.values),function(i){
    key0<-map.keys[[i]]
    value0<-map.values[[i]]
    n.pkt<-dim(value0)[1]
    if(n.pkt>1){
      arrival<-value0$time
      flag<-value0$rtpPT==0 | value0$rtpPT==8
      tmp.jitter<-diff(arrival)
      tmp.arrival1<-arrival[-n.pkt]
      tmp.flag1<-flag[-n.pkt]
      tmp.flag2<-flag[2:n.pkt]
      tmp.on<-tmp.flag1 & tmp.flag2
      jitter<-round(tmp.jitter[tmp.on]-0.02,6)*1000  #jitter is in ms
      arrival1<-tmp.arrival1[tmp.on]
      tmp.id<-diff(c(F,tmp.on,F))
      on.start<-seq_along(tmp.id)[tmp.id==1]
      on.end<-seq_along(tmp.id)[tmp.id==-1]
      if(length(on.start)>0){
        group0<-seq_along(on.start)
        group<-rep(group0,on.end-on.start)
	tmp.value<-split(data.frame(arrival1,jitter),group)
	lapply(group0,function(r){
	  key<-c(key0,r)
	  value<-tmp.value[[r]]
	  row.names(value)<-1:dim(value)[1]
#	  rhcollect(key,value)
	})
      }
    }
  })
})

z<-rhmr(map=m,ifolder="/wsc/voip/complete.data",ofolder="/wsc/jrounds/tmp/voip/jitter.database",inout=c("map","map"),jobname="jitter database 2nd method norhcollect")

jitter.database.mr2.norhcol.time<-system.time(
jitter.database.mr2.norhcol<-rhex(z)
)

rhsave(jitter.database.mr1,jitter.database.mr1.time,jitter.database.mr2,jitter.database.mr2.time,jitter.database.mr1.norhcol,jitter.database.mr1.norhcol.time,jitter.database.mr2.norhcol,jitter.database.mr2.norhcol.time,file="/wsc/jrounds/voip/records/jitter.database.mr.RData")


