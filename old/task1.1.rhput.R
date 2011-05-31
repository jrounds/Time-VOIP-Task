#ssh -L 50030:hadoop-01:50030 rossmann.rcac.purdue.edu
library(Rhipe)
rhinit()
rhoptions(runner=sprintf("%s/rhipe.runner.sh",Sys.getenv("HOME")))

time = system.time(rhput("/scratch/lustreA/j/jrounds/inout","/wsc/jrounds/voip.timings/iprtp.traces"))


