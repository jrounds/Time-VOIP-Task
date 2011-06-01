
library(Rhipe)
options(digits=18)
rhinit()
rhoptions(runner=sprintf("%s/rhipe.runner.sh",Sys.getenv("HOME")))8)

###############################################################################
#Perform polynomial regression on near-replicate subsets generated in modified.jitter.remote.site.traffic.rate.subset.R.
###############################################################################
##1. Only plot 1st subset for each remote site
###############################################################################
#1.1 Jitter vs traffic rate
###############################################################################
map.setup=expression({
  library(lattice)
})

m<-expression({
  v<-lapply(seq_along(map.values),function(r) {
    value0<-map.values[[r]]
    key0<-map.keys[[r]]

    jitter<-abs(value0$jitter)^(1/5)
    traffic.rate<-value0$traffic.rate/10^6
    rm.site<-key0[1]
    subset<-key0[2]
    
    if(subset==1){
    jitter1<-jitter
    jitter1[jitter<0.1]<-0.1
    jitter1[jitter>0.9]<-0.9
    plt<-xyplot(jitter1~traffic.rate,
      main=paste("Plot of Residual Jitter vs. Traffic Rate\nRemote Site=",rm.site,", Subset=",subset,", No. of Points=",length(jitter),sep=""),
      xlab="Traffic Rate (mbps)",
      ylab="Quintic-root Absolute Jitter (ms^(1/5))",
      pch=".",col=1,cex=1.3,
      panel=function(x,y,...){
        panel.xyplot(x,y,...)
	design.matrix<-cbind(1,traffic.rate)
	lfit<-lm.fit(design.matrix,jitter)
## 	traffic.rate.min<-min(traffic.rate)
## 	traffic.rate.max<-max(traffic.rate)
## 	traffic.rate.sample<-seq(traffic.rate.min,traffic.rate.max,length.out=300)
## 	sample.matrix<-cbind(1,traffic.rate.sample)
## 	fitted<-sample.matrix%*%lfit$coefficients
##         panel.xyplot(traffic.rate.sample,fitted,type="l",col=2,lwd=0)
        panel.abline(lfit$coefficients,type="l",col=2,lwd=0)
      }
    )
    value<-serialize(plt,NULL)
    rhcollect(key0,value)
    }
  })
})

mr<-rhmr(map=m,setup=map.setup,inout=c("map","map"),ifolder="/wsc/voip/modified.jitter.remote.site.traffic.rate.subset",ofolder="/wsc/jrounds/tmp/voip/modified.jitter.subset",jobname="sample plot of jitter vs traffic rate")

qnrt.abs.jitter.vs.trfc.rate.monic.reg.sample.plot.by.rm.site.mr.time<-system.time(
qnrt.abs.jitter.vs.trfc.rate.monic.reg.sample.plot.by.rm.site.mr<-rhex(mr)
)





rhsave(qnrt.abs.jitter.vs.trfc.rate.monic.reg.sample.plot.by.rm.site.mr,qnrt.abs.jitter.vs.trfc.rate.monic.reg.sample.plot.by.rm.site.mr.time,file="/wsc/jrounds/voip/records/qnrt.abs.jitter.vs.trfc.rate.monic.reg.sample.plot.by.rm.site.mr.RData")

###############################################################################
#1.2 Regression residual
###############################################################################
#1.2.1 Regression residual vs traffic rate
###############################################################################
map.setup=expression({
  library(lattice)
})

m<-expression({
  v<-lapply(seq_along(map.values),function(r) {
    value0<-map.values[[r]]
    key0<-map.keys[[r]]

    rm.site<-key0[1]
    subset<-key0[2]

    if(subset==1){
    jitter<-abs(value0$jitter)^(1/5)
    traffic.rate<-value0$traffic.rate/10^6
    
    design.matrix<-cbind(1,traffic.rate)
    lfit<-lm.fit(design.matrix,jitter)
    residual<-lfit$residuals

    plt<-xyplot(residual~traffic.rate,
      main=paste("Plot of Regression Residual vs. Traffic Rate\nRemote Site=",rm.site,", Subset=",subset,", No. of Points=",length(residual),sep=""),
      xlab="Traffic Rate (mbps)",
      ylab="Residual (ms^(1/5))",
      pch=".",col=1,cex=1.3,
      panel=function(x,y,...){
        panel.abline(h=0,col=2,lwd=0)
	panel.loess(traffic.rate,residual,span=1/4,col=4,lwd=0)
        panel.xyplot(x,y,...)
      }
    )
    value<-serialize(plt,NULL)
    rhcollect(key0,value)
    }
  })
})

mr<-rhmr(map=m,setup=map.setup,inout=c("map","map"),ifolder="/wsc/voip/modified.jitter.remote.site.traffic.rate.subset",ofolder="/wsc/jrounds/tmp/voip/modified.jitter.subset",jobname="sample plot of monic.reg residual vs traffic rate")

qnrt.abs.jitter.monic.reg.residual.vs.trfc.rate.sample.plot.by.rm.site.mr.time<-system.time(
qnrt.abs.jitter.monic.reg.residual.vs.trfc.rate.sample.plot.by.rm.site.mr<-rhex(mr)
)

qnrt.abs.jitter.monic.reg.residual.vs.trfc.rate.sample.plot.by.rm.site.rhread.time<-system.time(
{x=1}
#kv.list<-rhread("/tmp/voip/modified.jitter.subset",type='map')
)

qnrt.abs.jitter.monic.reg.residual.vs.trfc.rate.sample.plot.by.rm.site.n.rmodify.time<-system.time({
x = 1

})

qnrt.abs.jitter.monic.reg.residual.vs.trfc.rate.sample.plot.by.rm.site.distance.rmodify.time<-system.time({
x = 1

})

rhsave(
qnrt.abs.jitter.monic.reg.residual.vs.trfc.rate.sample.plot.by.rm.site.mr,
qnrt.abs.jitter.monic.reg.residual.vs.trfc.rate.sample.plot.by.rm.site.mr.time,
qnrt.abs.jitter.monic.reg.residual.vs.trfc.rate.sample.plot.by.rm.site.rhread.time,
qnrt.abs.jitter.monic.reg.residual.vs.trfc.rate.sample.plot.by.rm.site.n.rmodify.time,
qnrt.abs.jitter.monic.reg.residual.vs.trfc.rate.sample.plot.by.rm.site.distance.rmodify.time,
file="/wsc/jrounds/voip/records/qnrt.abs.jitter.monic.reg.residual.vs.trfc.rate.sample.plot.by.rm.site.mr.RData"
)

###############################################################################
#1.2.2 Absolute regression residual vs traffic rate
###############################################################################
map.setup=expression({
  library(lattice)
})

m<-expression({
  v<-lapply(seq_along(map.values),function(r) {
    value0<-map.values[[r]]
    key0<-map.keys[[r]]

    rm.site<-key0[1]
    subset<-key0[2]

    if(subset==1){
    jitter<-abs(value0$jitter)^(1/5)
    traffic.rate<-value0$traffic.rate/10^6
    
    design.matrix<-cbind(1,traffic.rate)
    lfit<-lm.fit(design.matrix,jitter)
    residual<-abs(lfit$residuals)

    plt<-xyplot(residual~traffic.rate,
      main=paste("Plot of Absolute Regression Residual vs. Traffic Rate\nRemote Site=",rm.site,", Subset=",subset,", No. of Points=",length(residual),sep=""),
      xlab="Traffic Rate (mbps)",
      ylab="Absolute Residual (ms^(1/5))",
      pch=".",col=1,cex=1.3,
      panel=function(x,y,...){
        panel.abline(h=0,col=2,lwd=0)
	panel.loess(traffic.rate,residual,span=1/4,col=4,lwd=0)
        panel.xyplot(x,y,...)
      }
    )
    value<-serialize(plt,NULL)
    rhcollect(key0,value)
    }
  })
})

mr<-rhmr(map=m,setup=map.setup,inout=c("map","map"),ifolder="/wsc/voip/modified.jitter.remote.site.traffic.rate.subset",ofolder="/wsc/jrounds/tmp/voip/modified.jitter.subset",jobname="sample plot of monic.reg abs.residual vs traffic rate")

qnrt.abs.jitter.monic.reg.abs.residual.vs.trfc.rate.sample.plot.by.rm.site.mr.time<-system.time(
qnrt.abs.jitter.monic.reg.abs.residual.vs.trfc.rate.sample.plot.by.rm.site.mr<-rhex(mr)
)

qnrt.abs.jitter.monic.reg.abs.residual.vs.trfc.rate.sample.plot.by.rm.site.rhread.time<-system.time(
{x = 1}
#kv.list<-rhread("/tmp/voip/modified.jitter.subset",type='map')
)

qnrt.abs.jitter.monic.reg.abs.residual.vs.trfc.rate.sample.plot.by.rm.site.n.rmodify.time<-system.time({
 x= 1
})

qnrt.abs.jitter.monic.reg.abs.residual.vs.trfc.rate.sample.plot.by.rm.site.distance.rmodify.time<-system.time({
x = 1
})

rhsave(
qnrt.abs.jitter.monic.reg.abs.residual.vs.trfc.rate.sample.plot.by.rm.site.mr,
qnrt.abs.jitter.monic.reg.abs.residual.vs.trfc.rate.sample.plot.by.rm.site.mr.time,
qnrt.abs.jitter.monic.reg.abs.residual.vs.trfc.rate.sample.plot.by.rm.site.rhread.time,
qnrt.abs.jitter.monic.reg.abs.residual.vs.trfc.rate.sample.plot.by.rm.site.n.rmodify.time,
qnrt.abs.jitter.monic.reg.abs.residual.vs.trfc.rate.sample.plot.by.rm.site.distance.rmodify.time,
file="/wsc/jrounds/voip/records/qnrt.abs.jitter.monic.reg.abs.residual.vs.trfc.rate.sample.plot.by.rm.site.mr.RData"
)

###############################################################################
#1.2.3 Regression residual normal qqplot
###############################################################################
#1.2.3.1 Quintic-root absolute jitter
###############################################################################
map.setup=expression({
  library(lattice)
})

m<-expression({
  v<-lapply(seq_along(map.values),function(r) {
    value0<-map.values[[r]]
    key0<-map.keys[[r]]

    rm.site<-key0[1]
    subset<-key0[2]

    if(subset==1){
    jitter<-abs(value0$jitter)^(1/5)
    traffic.rate<-value0$traffic.rate/10^6
    
    design.matrix<-cbind(1,traffic.rate)
    lfit<-lm.fit(design.matrix,jitter)
    residual<-lfit$residuals
    n.qntl<-1000
    qntl.resid<-quantile(residual,ppoints(n.qntl))
    qntl.norm<-qnorm(ppoints(n.qntl))
    ref.resid<-quantile(residual,c(0.01,0.05,0.25,0.75,0.95,0.99))
    ref.norm<-qnorm(c(0.01,0.05,0.25,0.75,0.95,0.99))

    plt<-xyplot(qntl.resid~qntl.norm,
      main=paste("Normal Quantile-quantile Plot of Regression Residual\nRemote Site=",rm.site,", Subset=",subset,sep=""),
      xlab="Standard Normal",
      ylab="Residual (ms^(1/5))",
      pch=".",col=1,cex=1.3,
      panel=function(x,y,...){
        slope<-(ref.resid[5]-ref.resid[2])/(ref.norm[5]-ref.norm[2])
	int<-ref.resid[5]-slope*ref.norm[5]
	panel.abline(int,slope,col=2,lwd=0)
        panel.abline(h=ref.resid,col=c(3,4,5,5,4,3),lwd=0)
        panel.xyplot(x,y,...)
      }
    )
    value<-serialize(plt,NULL)
    rhcollect(key0,value)
    }
  })
})

mr<-rhmr(map=m,setup=map.setup,inout=c("map","map"),ifolder="/wsc/voip/modified.jitter.remote.site.traffic.rate.subset",ofolder="/wsc/jrounds/tmp/voip/modified.jitter.subset",jobname="sample normal qqplot of monic.reg residual")

qnrt.abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.mr.time<-system.time(
qnrt.abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.mr<-rhex(mr)
)

qnrt.abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.rhread.time<-system.time(
{x=1}
#kv.list<-rhread("/tmp/voip/modified.jitter.subset",type='map')
)

qnrt.abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.rmodify.time<-system.time({
x = 1

})

qnrt.abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.rmodify.time<-system.time({
x = 1
})

rhsave(
qnrt.abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.mr,
qnrt.abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.mr.time,
qnrt.abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.rhread.time,
qnrt.abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.rmodify.time,
file="/wsc/jrounds/voip/records/qnrt.abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.mr.RData")

###############################################################################
#1.2.3.1 Raw absolute jitter - a comparison to the above showing the necessity of quintic-root transformation
###############################################################################
map.setup=expression({
  library(lattice)
})

m<-expression({
  v<-lapply(seq_along(map.values),function(r) {
    value0<-map.values[[r]]
    key0<-map.keys[[r]]

    rm.site<-key0[1]
    subset<-key0[2]

    if(subset==1){
    jitter<-abs(value0$jitter)
    traffic.rate<-value0$traffic.rate/10^6
    
    design.matrix<-cbind(1,traffic.rate)
    lfit<-lm.fit(design.matrix,jitter)
    residual<-lfit$residuals
    n.qntl<-1000
    qntl.resid<-quantile(residual,ppoints(n.qntl))
    qntl.norm<-qnorm(ppoints(n.qntl))
    ref.resid<-quantile(residual,c(0.01,0.05,0.25,0.75,0.95,0.99))
    ref.norm<-qnorm(c(0.01,0.05,0.25,0.75,0.95,0.99))

    plt<-xyplot(qntl.resid~qntl.norm,
      main=paste("Normal Quantile-quantile Plot of Regression Residual\nRemote Site=",rm.site,", Subset=",subset,sep=""),
      xlab="Standard Normal",
      ylab="Residual (ms)",
      pch=".",col=1,cex=1.3,
      panel=function(x,y,...){
        slope<-(ref.resid[5]-ref.resid[2])/(ref.norm[5]-ref.norm[2])
	int<-ref.resid[5]-slope*ref.norm[5]
	panel.abline(int,slope,col=2,lwd=0)
        panel.abline(h=ref.resid,col=c(3,4,5,5,4,3),lwd=0)
        panel.xyplot(x,y,...)
      }
    )
    value<-serialize(plt,NULL)
    rhcollect(key0,value)
    }
  })
})

mr<-rhmr(map=m,setup=map.setup,inout=c("map","map"),ifolder="/wsc/voip/modified.jitter.remote.site.traffic.rate.subset",ofolder="/wsc/jrounds/tmp/voip/modified.jitter.subset",jobname="sample normal qqplot of monic.reg residual")

abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.mr.time<-system.time(
abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.mr<-rhex(mr)
)

abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.rhread.time<-system.time(
{x=1}
#kv.list<-rhread("/tmp/voip/modified.jitter.subset",type='map')
)

abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.rmodify.time<-system.time({
x = 1
})

abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.rmodify.time<-system.time({
x = 1
})

rhsave(
abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.mr,
abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.mr.time,
abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.rhread.time,
abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.rmodify.time,
file="/wsc/jrounds/voip/records/abs.jitter.monic.reg.residual.sample.normal.qqplot.by.rm.site.mr.RData"
)

###############################################################################
##2. Plot 100 samples for each remote site
###############################################################################
#2.1 Jitter vs traffic rate
###############################################################################
mrsetup=expression({
  library(lattice)
})

m<-expression({
  v<-lapply(seq_along(map.values),function(r) {
    value0<-map.values[[r]]
    key0<-map.keys[[r]]

    jitter<-abs(value0$jitter)^(1/5)
    traffic.rate<-value0$traffic.rate/10^6
    rm.site<-key0[1]
    subset<-key0[2]
    
    jitter1<-jitter
    jitter1[jitter<0.1]<-0.1
    jitter1[jitter>0.9]<-0.9
    plt<-xyplot(jitter1~traffic.rate,
      main=paste("Plot of Residual Jitter vs. Traffic Rate\nSubset=",subset,", No. of Points=",length(jitter),", Mean=",round(attr(value0,"mean"),5),sep=""),
      xlab="Traffic Rate (mbps)",
      ylab="Quintic-root Absolute Jitter (ms^(1/5))",
      ylim=c(0.05,0.95),
      pch=".",col=1,cex=1.3,
      panel=function(x,y,...){
        panel.xyplot(x,y,...)
	design.matrix<-cbind(1,traffic.rate)
	lfit<-lm.fit(design.matrix,jitter)
## 	traffic.rate.min<-min(traffic.rate)
## 	traffic.rate.max<-max(traffic.rate)
## 	traffic.rate.sample<-seq(traffic.rate.min,traffic.rate.max,length.out=300)
## 	sample.matrix<-cbind(1,traffic.rate.sample)
## 	fitted<-sample.matrix%*%lfit$coefficients
##         panel.xyplot(traffic.rate.sample,fitted,type="l",col=2,lwd=0)
        panel.abline(lfit$coefficients,type="l",col=2,lwd=0)
      }
    )
    value<-list(plt=serialize(plt,NULL),mean.rank=attr(value0,"mean.rank"))
    rhcollect(rm.site,value)
  })
})

r<-expression(
  pre={
    plt<-NULL
    mean.rank<-NULL
  },
  reduce={
    plt<-append(plt,lapply(reduce.values,function(r) r$plt))
    mean.rank<-append(mean.rank,lapply(reduce.values,function(r) r$mean.rank))
  },
  post={
    mean.rank<-unlist(mean.rank)
    ord<-order(mean.rank)
    trellis.device("postscript",color=T,print.it=F,file=paste("tmp/qnrt.abs.residual.jitter.vs.traffic.rate.sample.plot",reduce.key,"ps",sep="."))
    lapply(ord,function(i){
      tmp.plt<-unserialize(plt[[i]])
      print(tmp.plt)
    })
    dev.off()
  }
)

mr<-rhmr(map=m,reduce=r,setup=mrsetup,inout=c("map","map"),ifolder="/wsc/voip/modified.jitter.remote.site.traffic.rate.sample.subset",ofolder="/wsc/jrounds/tmp/voip/modified.jitter.subset",copyFiles=T,jobname="sample plot of jitter vs traffic rate")

qnrt.abs.jitter.vs.trfc.rate.sample.plot.monic.reg.mr.time<-system.time(
qnrt.abs.jitter.vs.trfc.rate.sample.plot.monic.reg.mr<-rhex(mr)
)

qnrt.abs.jitter.vs.trfc.rate.sample.plot.monic.reg.rhget.time<-system.time(
{x= 1}
#rhget("/tmp/voip/modified.jitter.subset/qnrt*","/ln/dr.voip/jxia/jitter/qnrt.abs.residual.jitter.vs.traffic.rate.plot/monic.reg")
)

rhsave(
qnrt.abs.jitter.vs.trfc.rate.sample.plot.monic.reg.mr,
qnrt.abs.jitter.vs.trfc.rate.sample.plot.monic.reg.mr.time,
qnrt.abs.jitter.vs.trfc.rate.sample.plot.monic.reg.rhget.time,
file="/wsc/jrounds/voip/records/qnrt.abs.jitter.vs.trfc.rate.sample.plot.monic.reg.mr.RData"
)

###############################################################################
#2.2 Regression residual
###############################################################################
#2.2.1 Regression residual vs traffic rate
###############################################################################
mrsetup=expression({
  library(lattice)
})

m<-expression({
  v<-lapply(seq_along(map.values),function(r) {
    value0<-map.values[[r]]
    key0<-map.keys[[r]]

    jitter<-abs(value0$jitter)^(1/5)
    traffic.rate<-value0$traffic.rate/10^6
    rm.site<-key0[1]
    subset<-key0[2]
    
    design.matrix<-cbind(1,traffic.rate)
    lfit<-lm.fit(design.matrix,jitter)
    residual<-lfit$residuals

    plt<-xyplot(residual~traffic.rate,
      main=paste("Plot of Regression Residual vs. Traffic Rate\nSubset=",subset,", No. of Points=",length(residual),", Jitter Mean=",round(attr(value0,"mean"),5),sep=""),
      xlab="Traffic Rate (mbps)",
      ylab="Residual (ms^(1/5))",
      ylim=0.5*c(-1,1),
      pch=".",col=1,cex=1.3,
      panel=function(x,y,...){
        panel.abline(h=0,col=2,lwd=0)
	panel.loess(traffic.rate,residual,span=1/4,col=4,lwd=0)
        panel.xyplot(x,y,...)
      }
    )
    value<-list(plt=serialize(plt,NULL),mean.rank=attr(value0,"mean.rank"))
    rhcollect(rm.site,value)
  })
})

r<-expression(
  pre={
    plt<-NULL
    mean.rank<-NULL
  },
  reduce={
    plt<-append(plt,lapply(reduce.values,function(r) r$plt))
    mean.rank<-append(mean.rank,lapply(reduce.values,function(r) r$mean.rank))
  },
  post={
    mean.rank<-unlist(mean.rank)
    ord<-order(mean.rank)
    trellis.device("postscript",color=T,print.it=F,file=paste("tmp/qnrt.abs.residual.jitter.monic.reg.residual.vs.traffic.rate",reduce.key,"plot.ps",sep="."))
    lapply(ord,function(i){
      tmp.plt<-unserialize(plt[[i]])
      print(tmp.plt)
    })
    dev.off()
  }
)

mr<-rhmr(map=m,reduce=r,setup=mrsetup,inout=c("map","map"),ifolder="/wsc/voip/modified.jitter.remote.site.traffic.rate.sample.subset",ofolder="/wsc/jrounds/tmp/voip/modified.jitter.subset",copyFiles=T,jobname="sample plot of jitter residual vs traffic rate")

qnrt.abs.jitter.monic.reg.residual.vs.trfc.rate.sample.plot.mr.time<-system.time(
qnrt.abs.jitter.monic.reg.residual.vs.trfc.rate.sample.plot.mr<-rhex(mr)
)

qnrt.abs.jitter.monic.reg.residual.vs.trfc.rate.sample.plot.rhget.time<-system.time(
{x=1}
#rhget("/tmp/voip/modified.jitter.subset/qnrt*","/ln/dr.voip/jxia/jitter/qnrt.abs.residual.jitter.monic.reg.residual.vs.traffic.rate.plot")
)

rhsave(
qnrt.abs.jitter.monic.reg.residual.vs.trfc.rate.sample.plot.mr,
qnrt.abs.jitter.monic.reg.residual.vs.trfc.rate.sample.plot.mr.time,
qnrt.abs.jitter.monic.reg.residual.vs.trfc.rate.sample.plot.rhget.time,
file="/wsc/jrounds/voip/records/qnrt.abs.jitter.monic.reg.residual.vs.trfc.rate.sample.plot.mr.RData"
)

###############################################################################
#2.2.2 Absolute regression residual vs traffic rate
###############################################################################
mrsetup=expression({
  library(lattice)
})

m<-expression({
  v<-lapply(seq_along(map.values),function(r) {
    value0<-map.values[[r]]
    key0<-map.keys[[r]]

    jitter<-abs(value0$jitter)^(1/5)
    traffic.rate<-value0$traffic.rate/10^6
    rm.site<-key0[1]
    subset<-key0[2]
    
    design.matrix<-cbind(1,traffic.rate)
    lfit<-lm.fit(design.matrix,jitter)
    residual<-abs(lfit$residuals)

    plt<-xyplot(residual~traffic.rate,
      main=paste("Plot of Absolute Regression Residual vs. Traffic Rate\nSubset=",subset,", No. of Points=",length(residual),", Jitter Mean=",round(attr(value0,"mean"),5),sep=""),
      xlab="Traffic Rate (mbps)",
      ylab="Residual (ms^(1/5))",
      ylim=c(0,0.5),
      pch=".",col=1,cex=1.3,
      panel=function(x,y,...){
        panel.abline(h=0,col=2,lwd=0)
	panel.loess(traffic.rate,residual,span=1/4,col=4,lwd=0)
        panel.xyplot(x,y,...)
      }
    )
    value<-list(plt=serialize(plt,NULL),mean.rank=attr(value0,"mean.rank"))
    rhcollect(rm.site,value)
  })
})

r<-expression(
  pre={
    plt<-NULL
    mean.rank<-NULL
  },
  reduce={
    plt<-append(plt,lapply(reduce.values,function(r) r$plt))
    mean.rank<-append(mean.rank,lapply(reduce.values,function(r) r$mean.rank))
  },
  post={
    mean.rank<-unlist(mean.rank)
    ord<-order(mean.rank)
    trellis.device("postscript",color=T,print.it=F,file=paste("tmp/qnrt.abs.residual.jitter.monic.reg.abs.residual.vs.traffic.rate",reduce.key,"plot.ps",sep="."))
    lapply(ord,function(i){
      tmp.plt<-unserialize(plt[[i]])
      print(tmp.plt)
    })
    dev.off()
  }
)

mr<-rhmr(map=m,reduce=r,setup=mrsetup,inout=c("map","map"),ifolder="/wsc/voip/modified.jitter.remote.site.traffic.rate.sample.subset",ofolder="/wsc/jrounds/tmp/voip/modified.jitter.subset",copyFiles=T,jobname="sample plot of abs jitter residual vs traffic rate")

qnrt.abs.jitter.monic.reg.abs.residual.vs.trfc.rate.sample.plot.mr.time<-system.time(
qnrt.abs.jitter.monic.reg.abs.residual.vs.trfc.rate.sample.plot.mr<-rhex(mr)
)

qnrt.abs.jitter.monic.reg.abs.residual.vs.trfc.rate.sample.plot.rhget.time<-system.time(
{x= 1}
#rhget("/tmp/voip/modified.jitter.subset/qnrt*","/ln/dr.voip/jxia/jitter/qnrt.abs.residual.jitter.monic.reg.abs.residual.vs.traffic.rate.plot")
)

rhsave(
qnrt.abs.jitter.monic.reg.abs.residual.vs.trfc.rate.sample.plot.mr,
qnrt.abs.jitter.monic.reg.abs.residual.vs.trfc.rate.sample.plot.mr.time,
qnrt.abs.jitter.monic.reg.abs.residual.vs.trfc.rate.sample.plot.rhget.time,
file="/wsc/jrounds/voip/records/qnrt.abs.jitter.monic.reg.abs.residual.vs.trfc.rate.sample.plot.mr.RData"
)

###############################################################################
#2.2.3 Regression residual normal qqplot
###############################################################################
#2.2.3.1 Quintic-root absolute jitter
###############################################################################
mrsetup=expression({
  library(lattice)
})

m<-expression({
  v<-lapply(seq_along(map.values),function(r) {
    value0<-map.values[[r]]
    key0<-map.keys[[r]]

    jitter<-abs(value0$jitter)^(1/5)
    traffic.rate<-value0$traffic.rate/10^6
    rm.site<-key0[1]
    subset<-as.integer(key0[2])
    
    design.matrix<-cbind(1,traffic.rate)
    lfit<-lm.fit(design.matrix,jitter)
    residual<-lfit$residuals

    n.qntl<-1000
    qntl.resid<-quantile(residual,ppoints(n.qntl))
    ref.resid<-quantile(residual,c(0.01,0.05,0.25,0.75,0.95,0.99))

    value<-list(qntl.resid=qntl.resid,ref.resid=ref.resid,subset=subset,mean.rank=attr(value0,"mean.rank"))
    rhcollect(rm.site,value)
  })
})

r<-expression(
  pre={
    qntl.resid<-NULL
    ref.resid<-NULL
    subset<-NULL
    mean.rank<-NULL
    n.qntl<-1000
  },
  reduce={
    qntl.resid<-append(qntl.resid,lapply(reduce.values,function(r) r$qntl.resid))
    ref.resid<-append(ref.resid,lapply(reduce.values,function(r) r$ref.resid))
    subset<-append(subset,lapply(reduce.values,function(r) r$subset))
    mean.rank<-append(mean.rank,lapply(reduce.values,function(r) r$mean.rank))
  },
  post={
    qntl.resid<-unlist(qntl.resid)
#    ref.resid<-unlist(ref.resid)
    subset<-unlist(subset)
    names(ref.resid)<-subset
    mean.rank<-unlist(mean.rank)

    n.subset<-length(subset)
    qntl.norm<-rep(qnorm(ppoints(n.qntl)),n.subset)
    ref.norm<-qnorm(c(0.01,0.05,0.25,0.75,0.95,0.99))
    ord<-order(mean.rank)
    tmp.subset<-factor(rep(subset,each=n.qntl),levels=subset[ord])
    
    trellis.device("postscript",color=T,print.it=F,file=paste("tmp/qnrt.abs.residual.jitter.monic.reg.residual.normal.qqplot",reduce.key,"ps",sep="."))
    plt<-xyplot(qntl.resid~qntl.norm|tmp.subset,
      main=paste("Normal Quantile-quantile Plot of Regression Residual\nNo. of Quantiles=",n.qntl,sep=""),
      xlab="Standard Normal",
      ylab="Residual (ms^(1/5))",
      pch=".",col=1,cex=1.3,
      layout=c(5,4),
      panel=function(x,y,subscripts,...){
        subset0<-unique(tmp.subset[subscripts])
	ref.resid0<-ref.resid[as.character(subset0)][[1]]
        slope<-(ref.resid0[5]-ref.resid0[2])/(ref.norm[5]-ref.norm[2])
	int<-ref.resid0[5]-slope*ref.norm[5]
	panel.abline(int,slope,col=2,lwd=0)
        panel.abline(h=ref.resid0,col=c(3,4,5,5,4,3),lwd=0)
        panel.xyplot(x,y,...)
      }
    )
    print(plt)
    dev.off()
  }
)

mr<-rhmr(map=m,reduce=r,setup=mrsetup,inout=c("map","map"),ifolder="/wsc/jrounds/voip/modified.jitter.remote.site.traffic.rate.sample.subset",ofolder="/tmp/voip/modified.jitter.subset",copyFiles=T,jobname="sample plot of jitter residual normal qqplot")

qnrt.abs.jitter.monic.reg.residual.sample.normal.qqplot.mr.time<-system.time(
qnrt.abs.jitter.monic.reg.residual.sample.normal.qqplot.mr<-rhex(mr)
)

qnrt.abs.jitter.monic.reg.residual.sample.normal.qqplot.rhget.time<-system.time(
{x = 1}
#rhget("/tmp/voip/modified.jitter.subset/qnrt*","/ln/dr.voip/jxia/jitter/qnrt.abs.residual.jitter.monic.reg.residual.normal.qqplot")
)

rhsave(qnrt.abs.jitter.monic.reg.residual.sample.normal.qqplot.mr,qnrt.abs.jitter.monic.reg.residual.sample.normal.qqplot.mr.time,qnrt.abs.jitter.monic.reg.residual.sample.normal.qqplot.rhget.time,file="/wsc/jrounds/voip/records/qnrt.abs.jitter.monic.reg.residual.sample.normal.qqplot.mr.RData")

###############################################################################
#2.2.3.1 Raw absolute jitter - a comparison to the above showing the necessity of quintic-root transformation
###############################################################################
mrsetup=expression({
  library(lattice)
})

m<-expression({
  v<-lapply(seq_along(map.values),function(r) {
    value0<-map.values[[r]]
    key0<-map.keys[[r]]

    jitter<-abs(value0$jitter)
    traffic.rate<-value0$traffic.rate/10^6
    rm.site<-key0[1]
    subset<-as.integer(key0[2])
    
    design.matrix<-cbind(1,traffic.rate)
    lfit<-lm.fit(design.matrix,jitter)
    residual<-lfit$residuals

    n.qntl<-1000
    qntl.resid<-quantile(residual,ppoints(n.qntl))
    ref.resid<-quantile(residual,c(0.01,0.05,0.25,0.75,0.95,0.99))

    value<-list(qntl.resid=qntl.resid,ref.resid=ref.resid,subset=subset,mean.rank=attr(value0,"mean.rank"))
    rhcollect(rm.site,value)
  })
})

r<-expression(
  pre={
    qntl.resid<-NULL
    ref.resid<-NULL
    subset<-NULL
    mean.rank<-NULL
    n.qntl<-1000
  },
  reduce={
    qntl.resid<-append(qntl.resid,lapply(reduce.values,function(r) r$qntl.resid))
    ref.resid<-append(ref.resid,lapply(reduce.values,function(r) r$ref.resid))
    subset<-append(subset,lapply(reduce.values,function(r) r$subset))
    mean.rank<-append(mean.rank,lapply(reduce.values,function(r) r$mean.rank))
  },
  post={
    qntl.resid<-unlist(qntl.resid)
#    ref.resid<-unlist(ref.resid)
    subset<-unlist(subset)
    names(ref.resid)<-subset
    mean.rank<-unlist(mean.rank)

    n.subset<-length(subset)
    qntl.norm<-rep(qnorm(ppoints(n.qntl)),n.subset)
    ref.norm<-qnorm(c(0.01,0.05,0.25,0.75,0.95,0.99))
    ord<-order(mean.rank)
    tmp.subset<-factor(rep(subset,each=n.qntl),levels=subset[ord])
    
    trellis.device("postscript",color=T,print.it=F,file=paste("tmp/abs.residual.jitter.monic.reg.residual.normal.qqplot",reduce.key,"ps",sep="."))
    plt<-xyplot(qntl.resid~qntl.norm|tmp.subset,
      main=paste("Normal Quantile-quantile Plot of Regression Residual\nNo. of Quantiles=",n.qntl,sep=""),
      xlab="Standard Normal",
      ylab="Residual (ms)",
      pch=".",col=1,cex=1.3,
      layout=c(5,4),
      panel=function(x,y,subscripts,...){
        subset0<-unique(tmp.subset[subscripts])
	ref.resid0<-ref.resid[as.character(subset0)][[1]]
        slope<-(ref.resid0[5]-ref.resid0[2])/(ref.norm[5]-ref.norm[2])
	int<-ref.resid0[5]-slope*ref.norm[5]
	panel.abline(int,slope,col=2,lwd=0)
        panel.abline(h=ref.resid0,col=c(3,4,5,5,4,3),lwd=0)
        panel.xyplot(x,y,...)
      }
    )
    print(plt)
    dev.off()
  }
)

mr<-rhmr(map=m,reduce=r,setup=mrsetup,inout=c("map","map"),ifolder="/wsc/voip/modified.jitter.remote.site.traffic.rate.sample.subset",ofolder="/wsc/jrounds/tmp/voip/modified.jitter.subset",copyFiles=T,jobname="sample plot of jitter residual normal qqplot")

abs.jitter.monic.reg.residual.sample.normal.qqplot.mr.time<-system.time(
abs.jitter.monic.reg.residual.sample.normal.qqplot.mr<-rhex(mr)
)

abs.jitter.monic.reg.residual.sample.normal.qqplot.rhget.time<-system.time(
{x = 1}
#rhget("/tmp/voip/modified.jitter.subset/abs*","/ln/dr.voip/jxia/jitter/abs.residual.jitter.monic.reg.residual.normal.qqplot")
)

rhsave(abs.jitter.monic.reg.residual.sample.normal.qqplot.mr,abs.jitter.monic.reg.residual.sample.normal.qqplot.mr.time,abs.jitter.monic.reg.residual.sample.normal.qqplot.rhget.time,file="/wsc/jrounds/voip/records/abs.jitter.monic.reg.residual.sample.normal.qqplot.mr.RData")

###############################################################################
#2.3 Regression fit vs traffic rate
###############################################################################
#2.3.1 Juxtapose
###############################################################################
mrsetup=expression({
  library(lattice)
})

m<-expression({
  v<-lapply(seq_along(map.values),function(r) {
    value0<-map.values[[r]]
    key0<-map.keys[[r]]

    jitter<-abs(value0$jitter)^(1/5)
    traffic.rate<-value0$traffic.rate/10^6
    rm.site<-key0[1]
    subset<-as.integer(key0[2])
    
    design.matrix<-cbind(1,traffic.rate)
    lfit<-lm.fit(design.matrix,jitter)
    traffic.rate.min<-min(traffic.rate)
    traffic.rate.max<-max(traffic.rate)
    traffic.rate.sample<-seq(traffic.rate.min,traffic.rate.max,length.out=300)
    sample.matrix<-cbind(1,traffic.rate.sample)
    fitted<-sample.matrix%*%lfit$coefficients
    value<-list(fit=data.frame(fitted,traffic.rate=traffic.rate.sample,subset=subset),subset=subset,mean.rank=attr(value0,"mean.rank"))
    rhcollect(rm.site,value)
  })
})

r<-expression(
  pre={
    fit<-NULL
    subset<-NULL
    mean.rank<-NULL
  },
  reduce={
    fit<-append(fit,lapply(reduce.values,function(r) r$fit))
    subset<-append(subset,lapply(reduce.values,function(r) r$subset))
    mean.rank<-append(mean.rank,lapply(reduce.values,function(r) r$mean.rank))
  },
  post={
    tmp<-do.call("rbind",fit)
    subset<-unlist(subset)
    mean.rank<-unlist(mean.rank)
    fit<-data.frame(fitted=as.numeric(tmp[,1]),traffic.rate=as.numeric(tmp[,2]),subset=factor(as.integer(tmp[,3]),levels=subset[order(mean.rank)]))
    
    trellis.device("postscript",color=T,print.it=F,file=paste("tmp/qnrt.abs.residual.jitter.monic.reg.fit.vs.traffic.rate.juxtapose.plot",reduce.key,"ps",sep="."))
    plt<-xyplot(fitted~traffic.rate|subset,
      data=fit,
      main=paste("Plot of Regression Fit vs. Traffic Rate\nRemote Site=",reduce.key,sep=""),
      xlab="Traffic Rate (mbps)",
      ylab="Regression Fit (ms^(1/5))",
      type="l",col=1,
#      lwd=0,
      layout=c(5,4)
    )
    print(plt)
    dev.off()
  }
)

mr<-rhmr(map=m,reduce=r,setup=mrsetup,inout=c("map","map"),ifolder="/wsc/voip/modified.jitter.remote.site.traffic.rate.sample.subset",ofolder="/wsc/jrounds/tmp/voip/modified.jitter.subset",copyFiles=T,jobname="sample plot of jitter regression fit")

qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.juxtapose.plot.mr.time<-system.time(
qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.juxtapose.plot.mr<-rhex(mr)
)

qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.juxtapose.plot.rhget.time<-system.time(
{x = 1}
#rhget("/tmp/voip/modified.jitter.subset/qnrt*","/ln/dr.voip/jxia/jitter/qnrt.abs.residual.jitter.monic.reg.fit.vs.traffic.rate.juxtapose.plot")
)

rhsave(qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.juxtapose.plot.mr,
qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.juxtapose.plot.mr.time,qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.juxtapose.plot.rhget.time,file="/wsc/jrounds/voip/records/qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.juxtapose.plot.mr.RData")

###############################################################################
#2.3.2 Superpose
###############################################################################
#2.3.2.1 Separate files
###############################################################################
mrsetup=expression({
  library(lattice)
})

m<-expression({
  v<-lapply(seq_along(map.values),function(r) {
    value0<-map.values[[r]]
    key0<-map.keys[[r]]

    jitter<-abs(value0$jitter)^(1/5)
    traffic.rate<-value0$traffic.rate/10^6
    rm.site<-key0[1]
    subset<-as.integer(key0[2])
    
    design.matrix<-cbind(1,traffic.rate)
    lfit<-lm.fit(design.matrix,jitter)
    traffic.rate.min<-min(traffic.rate)
    traffic.rate.max<-max(traffic.rate)
    traffic.rate.sample<-seq(traffic.rate.min,traffic.rate.max,length.out=300)
    sample.matrix<-cbind(1,traffic.rate.sample)
    fitted<-sample.matrix%*%lfit$coefficients
    value<-data.frame(fitted,traffic.rate=traffic.rate.sample,subset=subset)
    rhcollect(rm.site,value)
  })
})

r<-expression(
  pre={
    fit<-NULL
  },
  reduce={
    fit<-append(fit,reduce.values)
  },
  post={
    tmp<-do.call("rbind",fit)
    fit<-data.frame(fitted=as.numeric(tmp[,1]),traffic.rate=as.numeric(tmp[,2]),subset=as.integer(tmp[,3]))
    
    trellis.device("postscript",color=T,print.it=F,file=paste("tmp/qnrt.abs.residual.jitter.monic.reg.fit.vs.traffic.rate.superpose.plot",reduce.key,"ps",sep="."))
    plt<-xyplot(fitted~traffic.rate,
      data=fit,
      group=subset,
      main=paste("Plot of Regression Fit of Residual Jitter vs. Traffic Rate\nRemote Site=",reduce.key,sep=""),
      xlab="Traffic Rate (mbps)",
      ylab="Regression Fit (ms^(1/5))",
      type="l",lwd=0
    )
    print(plt)
    dev.off()
  }
)

mr<-rhmr(map=m,reduce=r,setup=mrsetup,inout=c("map","map"),ifolder="/wsc/voip/modified.jitter.remote.site.traffic.rate.sample.subset",ofolder="/wsc/jrounds/tmp/voip/modified.jitter.subset",copyFiles=T,jobname="sample plot of jitter regression fit")

qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.superpose.plot.mr.time<-system.time(
qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.superpose.plot.mr<-rhex(mr)
)

qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.superpose.plot.rhget.time<-system.time(
{x=1}
#rhget("/tmp/voip/modified.jitter.subset/qnrt*","/ln/dr.voip/jxia/jitter/qnrt.abs.residual.jitter.monic.reg.fit.vs.traffic.rate.superpose.plot")
)

rhsave(qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.superpose.plot.mr,qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.superpose.plot.mr.time,qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.superpose.plot.rhget.time,file="/wsc/jrounds/voip/records/qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.superpose.plot.mr.RData")

###############################################################################
#2.4.2.2 One file
###############################################################################
mrsetup=expression({
  library(lattice)
})

m<-expression({
  v<-lapply(seq_along(map.values),function(r) {
    value0<-map.values[[r]]
    key0<-map.keys[[r]]

    jitter<-abs(value0$jitter)^(1/5)
    traffic.rate<-value0$traffic.rate/10^6
    rm.site<-key0[1]
    subset<-as.integer(key0[2])
    
    design.matrix<-cbind(1,traffic.rate)
    lfit<-lm.fit(design.matrix,jitter)
    traffic.rate.min<-min(traffic.rate)
    traffic.rate.max<-max(traffic.rate)
    traffic.rate.sample<-seq(traffic.rate.min,traffic.rate.max,length.out=300)
    sample.matrix<-cbind(1,traffic.rate.sample)
    fitted<-sample.matrix%*%lfit$coefficients
    value<-data.frame(fitted,traffic.rate=traffic.rate.sample,subset=subset)
    rhcollect(rm.site,value)
  })
})

r<-expression(
  pre={
    fit<-NULL
  },
  reduce={
    fit<-append(fit,reduce.values)
  },
  post={
    tmp<-do.call("rbind",fit)
    fit<-data.frame(fitted=as.numeric(tmp[,1]),traffic.rate=as.numeric(tmp[,2]),subset=as.integer(tmp[,3]))
    
    plt<-xyplot(fitted~traffic.rate,
      data=fit,
      group=subset,
      main=paste("Plot of Regression Fit of Residual Jitter vs. Traffic Rate\nRemote Site=",reduce.key,sep=""),
      xlab="Traffic Rate (mbps)",
      ylab="Regression Fit (ms^(1/5))",
      type="l",lwd=0
    )
    value<-serialize(plt,NULL)
    rhcollect(reduce.key,value)
  }
)

mr<-rhmr(map=m,reduce=r,setup=mrsetup,inout=c("map","map"),ifolder="/wsc/voip/modified.jitter.remote.site.traffic.rate.sample.subset",ofolder="/wsc/jrounds/tmp/voip/modified.jitter.subset",copyFiles=T,jobname="sample plot of jitter regression fit")

qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.superpose.plot.by.rm.site.mr.time<-system.time(
qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.superpose.plot.by.rm.site.mr<-rhex(mr)
)

qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.superpose.plot.by.rm.site.rhread.time<-system.time(
{x=1}
#kv.list<-rhread("/tmp/voip/modified.jitter.subset",type='map')
)

qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.superpose.plot.by.rm.site.rmodify.time<-system.time({
x = 1
})



rhsave(qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.superpose.plot.by.rm.site.mr,qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.superpose.plot.by.rm.site.mr.time,qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.superpose.plot.by.rm.site.rhread.time,qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.superpose.plot.by.rm.site.rmodify.time,file="/wsc/jrounds/voip/records/qnrt.abs.jitter.monic.reg.fit.vs.trfc.rate.sample.superpose.plot.by.rm.site.mr.RData")

###############################################################################
##3. Check regression results of all subsets for each remote site
###############################################################################
##3.1 Regression coeffecients
###############################################################################
m<-expression({
  v<-lapply(seq_along(map.values),function(r) {
    value0<-map.values[[r]]
    key0<-map.keys[[r]]

    rm.site<-key0[1]
    
    if(!(rm.site %in% c("Indianapolis","Chicago","Brussels","Kansas City","Southfield","Denver","Boston"))){
      jitter<-abs(value0$jitter)^(1/5)
      traffic.rate<-value0$traffic.rate/10^6
      subset<-as.integer(key0[2])

      design.matrix<-cbind(1,traffic.rate)
      lfit<-lm.fit(design.matrix,jitter)
      value<-c(subset,lfit$coefficients)
      rhcollect(rm.site,value)
    }
  })
})

r<-expression(
  pre={
    mydata<-NULL
  },
  reduce={
    mydata<-append(mydata,reduce.values)
  },
  post={
    mydata<-do.call("rbind",mydata)
    ord<-order(as.numeric(mydata[,1]))
    mydata<-mydata[ord,,drop=F]
    value<-data.frame(subset=as.integer(mydata[,1]),b0=as.numeric(mydata[,2]),b1=as.numeric(mydata[,3]),stringsAsFactors=F)
    rhcollect(reduce.key,value)
  }
)

mr<-rhmr(map=m,reduce=r,inout=c("map","map"),ifolder="/wsc/voip/modified.jitter.remote.site.traffic.rate.subset",ofolder="/wsc/jrounds/tmp/voip/modified.jitter.subset",jobname="monic reg coeffecients")

qnrt.abs.jitter.monic.reg.coef.by.rm.site.mr.time<-system.time(
qnrt.abs.jitter.monic.reg.coef.by.rm.site.mr<-rhex(mr)
)

qnrt.abs.jitter.monic.reg.coef.by.rm.site.rhread.time<-system.time(
{ x = 1}
#kv.list<-rhread("/wsc/jrounds/tmp/voip/modified.jitter.subset",type="map")
)


###############################################################################
#Average polynomial for each site
###############################################################################
#rhload(file="/voip/summary.tables/modified.jitter.database/jitter.remote.site.subset.monic.reg.coef.RData")

qnrt.abs.jitter.monic.reg.avg.coef.by.rm.site.time<-system.time({
 x= 1
})

rhsave(qnrt.abs.jitter.monic.reg.coef.by.rm.site.mr,qnrt.abs.jitter.monic.reg.coef.by.rm.site.mr.time,qnrt.abs.jitter.monic.reg.coef.by.rm.site.rhread.time,qnrt.abs.jitter.monic.reg.coef.by.rm.site.rmodify.time,qnrt.abs.jitter.monic.reg.avg.coef.by.rm.site.time,file="/wsc/jrounds/voip/records/qnrt.abs.jitter.monic.reg.coef.by.rm.site.mr.RData")






