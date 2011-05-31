library(Rhipe)

raw.data.to.hdfs.time<-system.time(
rhput("/scratch/lustreA/j/jrounds/inout","/wsc/jrounds/voip/iprtp.traces"))
)
#rhput("/ln/voip/usr/jxia/RHIPE/summary.stats/all.trace.summary.RData","/voip/other")

rhls("/")

rhsave(raw.data.to.hdfs.time,file="/wsc/jrounds/voip/records/raw.data.to.hdfs.RData")

