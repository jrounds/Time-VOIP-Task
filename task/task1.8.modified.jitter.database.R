library(Rhipe)
options(digits=18)
rhinit()
rhoptions(runner=sprintf("%s/rhipe.runner.sh",Sys.getenv("HOME")))

m<-expression({
  X9<-diag(rep(1,9)) 
  mywt.bisquare <- function(u, c = 6){
    U <- abs(u/c)
    w <- ((1 + U) * (1 - U))^2
    w[U > 1] <- 0
    w
  }
  lapply(seq_along(map.values),function(i){
    key0<-map.keys[[i]]
    value0<-map.values[[i]]
    n.jitter<-nrow(value0)
    if(n.jitter>=90){
      jitter<-value0$jitter
      id<-c(0:(n.jitter-1))%%9+1
      xx<-X9[id,]
      wt<-rep(1,n.jitter)
      fit.lm <- lm.wfit(xx,jitter,wt)   #lm(jitter ~ xx -1, weights=wt) #no intercept
      for(j in 1:3){
        res<-fit.lm$residuals
        wt<-mywt.bisquare(res/median(abs(res)),c=6)
#	error<-tapply(wt,id,sum)
	tmp.wt<-split(wt,id)
	error<-unlist(lapply(tmp.wt,sum),use.names=F)
	if(all(!is.na(error) & error>5)){
	  fit.lm<-lm.wfit(xx,jitter,wt)
	} else break
      }
      if(all(!is.na(error) & error>5)){
        value<-data.frame(arrival1=value0$arrival1,jitter=fit.lm$residuals)
	attr(value,"magnitude")<-max(fit.lm$coefficients)-min(fit.lm$coefficients)
	rhcollect(key0,value)
      }
    }
  })
})

z<-rhmr(map=m,ifolder="/wsc/voip/jitter.database",ofolder="/wsc/jrounds/voip/modified.jitter.database",inout=c("map","map"),jobname="modified jitter database")
#z<-rhmr(map=m,ifolder="/voip/jitter.database/part-r-00000/data",ofolder="/tmp/voip/modified.jitter.database",inout=c("sequence","map"),jobname="modified jitter database")

modified.jitter.database.mr.time<-system.time(
modified.jitter.database.mr<-rhex(z)
)

###Only for time interest
m<-expression({
  X9<-diag(rep(1,9)) 
  mywt.bisquare <- function(u, c = 6){
    U <- abs(u/c)
    w <- ((1 + U) * (1 - U))^2
    w[U > 1] <- 0
    w
  }
  lapply(seq_along(map.values),function(i){
    key0<-map.keys[[i]]
    value0<-map.values[[i]]
    n.jitter<-nrow(value0)
    if(n.jitter>=45){
      jitter<-value0$jitter
      id<-c(0:(n.jitter-1))%%9+1
      xx<-X9[id,]
      wt<-rep(1,n.jitter)
      fit.lm <- lm.wfit(xx,jitter,wt)   #lm(jitter ~ xx -1, weights=wt) #no intercept
      for(j in 1:3){
        res<-fit.lm$residuals
        wt<-mywt.bisquare(res/median(abs(res)),c=6)
#	error<-tapply(wt,id,sum)
	tmp.wt<-split(wt,id)
	error<-unlist(lapply(tmp.wt,sum),use.names=F)
	if(all(!is.na(error) & error>5)){
	  fit.lm<-lm.wfit(xx,jitter,wt)
	} else break
      }
      if(all(!is.na(error) & error>5)){
        value<-data.frame(arrival1=value0$arrival1,jitter=fit.lm$residuals)
	attr(value,"magnitude")<-max(fit.lm$coefficients)-min(fit.lm$coefficients)
	rhcollect(key0,value)
      }
    }
  })
})

z<-rhmr(map=m,ifolder="/wsc/voip/jitter.database",ofolder="/wsc/jrounds/voip/modified.jitter.database",inout=c("map","map"),jobname="modified jitter database")

modified.jitter.database.mr.45.time<-system.time(
modified.jitter.database.mr.45<-rhex(z)
)

rhsave(modified.jitter.database.mr,modified.jitter.database.mr.time,modified.jitter.database.mr.45,modified.jitter.database.mr.45.time,file="/wsc/jrounds/voip/records/modified.jitter.database.mr.RData")






