###Separate each call trace from the multiplexed trace files.
library(Rhipe)
options(digits=18)
rhoptions(runner=sprintf("%s/rhipe.runner.sh",Sys.getenv("HOME")))

map = expression({
	ifile = Sys.getenv("mapred.input.file")
	#these look like: "hdfs://hadoop-00.rcac.purdue.edu/wsc/voip/iprtp.traces/20040311-095951-0.iprtp.in.gz"
	direction = strsplit(ifile,"\\.")[[1]]
	n = length(direction)
	direction = direction[n-1]
	id.ip = if(direction=="in") c(7,5) else c(5,7)
	id.port = if(direction=="in") c(8,6) else c(6,8)
	for(r in seq_along(map.values)) {
		v = strsplit(map.values[[r]]," +")[[1]]
		key = paste(v[id.ip[1]],v[id.port[1]], v[id.ip[2]],v[id.port[2]], direction,sep=".")
		value = c(as.numeric(v[1]),as.integer(v[9]))
		rhcollect(key,value)
	}
})

reduce = expression(
	pre = {
		data=list()
	},
	reduce = {
		data = append(data,reduce.values)
	},
	post = {
		data = do.call("rbind",data)
		colnames(data) = c("time","rtpPT")
		data = data[order(data[,'time']),,drop=F]
		data = data.frame(time = as.numeric(data[,'time']), rtpPT = as.integer(data[,'rtpPT']))
		rhcollect(reduce.key,data)
	}
)

mr=rhmr(map=map,reduce=reduce,inout=c("text","map"),ifolder="/wsc/voip/iprtp.traces",ofolder="/wsc/jrounds/voip.timings/call.traces",jobname="create call trace database")


time=system.time({call.trace.mr=rhex(mr)})

