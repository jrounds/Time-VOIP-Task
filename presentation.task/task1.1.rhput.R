library(Rhipe)
options(digits=18)
rhinit()
rhoptions(runner=sprintf("%s/rhipe.runner.sh",Sys.getenv("HOME")))

raw.data.to.hdfs.time<-system.time(
rhput("/scratch/lustreA/j/jrounds/inout","/wsc/voip/iprtp.traces")
)


rhsave(raw.data.to.hdfs.time,file="/wsc/jrounds/voip/records/raw.data.to.hdfs.RData")
quit(save="no")

