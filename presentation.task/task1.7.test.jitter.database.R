library(Rhipe)
options(digits=18)
rhinit()
rhoptions(runner=sprintf("%s/rhipe.runner.sh",Sys.getenv("HOME")))


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
      n.jitter<-on.end-on.start
      lapply(seq_along(on.start),function(i){
        key<-c(key0,i)
	value<-n.jitter[i]
        rhcollect(key,value)
      })
    }
  })
})

z<-rhmr(map=m,ifolder="/wsc/voip/complete.data",ofolder="/wsc/jrounds/tmp/voip/jitter",inout=c("map","sequence"),jobname="number of jitter(complete)")

n.jitter.complete.data.mr.time<-system.time(
n.jitter.complete.data.mr<-rhex(z)
)


#Jitter database created from method 1
m <- expression({
  lapply(seq_along(map.values),function(i){
    key0<-map.keys[[i]]
    value0<-map.values[[i]]
    value<-dim(value0)[1]
    rhcollect(key0,value)
  })
})

z<-rhmr(map=m,ifolder="/wsc/voip/jitter.database",ofolder="/wsc/jrounds/tmp/voip/jitter",inout=c("map","sequence"),jobname="number of jitter(jitter)")

n.jitter.jitter.database.mr.time<-system.time(
n.jitter.jitter.database.mr<-rhex(z)
)



rhsave(n.jitter.complete.data.mr,n.jitter.complete.data.mr.time,n.jitter.jitter.database.mr,n.jitter.jitter.database.mr.time,file="/wsc/jrounds/voip/records/n.jitter.mr.RData")



#Jitter database created from method 2
##a
m <- expression({
  lapply(seq_along(map.values),function(i){
    key0<-map.keys[[i]]
    value0<-map.values[[i]]
    value<-dim(value0)[1]
    rhcollect(key0,value)
  })
})

z<-rhmr(map=m,ifolder="/wsc/voip/jitter.database",ofolder="/wsc/jrounds/tmp/voip/jitter",inout=c("map","sequence"),jobname="number of jitter(jitter 2a)")

n.jitter.mra.time<-system.time(
n.jitter.mra<-rhex(z)
)

n.jitter.rhreada.time<-system.time( {x = 1}
#kv <- rhread("/wsc/jrounds/tmp/voip/jitter",doloc=F)
)

n.jitter.rmodifya.time<-system.time({
x = 1
#kv1 <- do.call("rbind",lapply(kv,function(r) c(r[[1]],r[[2]])))
#ord<-order(kv1[,1],as.integer(kv1[,2]))
#n.jitter<-data.frame(flow.id=kv1[ord,1],interval=as.integer(kv1[ord,2]),number=as.integer(kv1[ord,3]),stringsAsFactors=F)
#kv1<-kv1[ord,]
#n.jitter<-data.frame(flow.id=kv1[,1],interval=as.integer(kv1[,2]),number=as.integer(kv1[,3]),stringsAsFactors=F)
})

#rhsave(n.jitter,file="/wsc/jrounds/voip/summary.tables/jitter.database/n.jitter.RData")

##b
m <- expression({
  lapply(seq_along(map.values),function(i){
    key0<-map.keys[[i]]
    value0<-map.values[[i]]
    key<-key0[1]
    value<-c(key0[2],dim(value0)[1])
    rhcollect(key,value)
  })
})

r <- expression(
  pre={
    mydata<-list()
  },
  reduce={
    mydata<-append(mydata,reduce.values)
  },
  post={
    mydata<-do.call("rbind",mydata)
    colnames(mydata) <- c("interval","number")
#    mydata <- mydata[order(mydata[,'interval']),,drop=F]
    mydata <- data.frame(interval = as.numeric(mydata[,'interval']),
                         number = as.integer(mydata[,'number']),
			 stringsAsFactors=F)
    mydata <- mydata[order(mydata$interval),,drop=F]
    rhcollect(reduce.key,mydata)
  }
)

z<-rhmr(map=m,reduce=r,ifolder="/wsc/voip/jitter.database",ofolder="/wsc/jrounds/tmp/voip/jitter1",inout=c("map","map"),jobname="number of jitter(jitter 2b1)")

n.jitter.mrb1.time<-system.time(
n.jitter.mrb1<-rhex(z)
)

m <- expression({
  keys<-ceiling(runif(length(map.values),0,500))
  lapply(seq_along(map.values),function(i){
    key0<-map.keys[[i]]
    value0<-map.values[[i]]
    value<-cbind(key0,value0)
    rhcollect(keys[i],value)
  })
})

r <- expression(
  pre={
    mydata<-list()
  },
  reduce={
    mydata<-append(mydata,reduce.values)
  },
  post={
    mydata<-do.call("rbind",mydata)
    colnames(mydata) <- c("flow.id","interval","number")
    mydata <- data.frame(flow.id=as.character(mydata[,'flow.id']),
			 interval = as.numeric(mydata[,'interval']),
                         number = as.integer(mydata[,'number']),
			 stringsAsFactors=F)
    rhcollect(reduce.key,mydata)
  }
)

z<-rhmr(map=m,reduce=r,ifolder="/wsc/jrounds/tmp/voip/jitter1",ofolder="/wsc/jrounds/tmp/voip/jitter2",inout=c("map","sequence"),jobname="number of jitter(jitter 2b2)")

n.jitter.mrb2.time<-system.time(
n.jitter.mrb2<-rhex(z)
)

n.jitter.rhreadb.time<-system.time({x=1}
#kv <- rhread("/wsc/jrounds/tmp/voip/jitter2",doloc=F)
)

#kv1 <- do.call("rbind",lapply(kv,function(r) nrow(r[[2]])))

n.jitter.rmodifyb.time<-system.time({
x = 1
#kv1 <- do.call("rbind",lapply(kv,function(r) r[[2]]))
#n.jitter<-data.frame(flow.id=kv1[,1],interval=as.integer(kv1[,2]),number=as.integer(kv1[,3]),stringsAsFactors=F)
})

rhsave(n.jitter.mra,n.jitter.mra.time,n.jitter.rhreada.time,n.jitter.rmodifya.time,n.jitter.mrb1,n.jitter.mrb1.time,n.jitter.mrb2,n.jitter.mrb2.time,n.jitter.rhreadb.time,n.jitter.rmodifyb.time,file="/wsc/jrounds/voip/records/n.jitter.mr.strata.RData")

quit(save="no")

