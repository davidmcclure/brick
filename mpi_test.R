
library(Rmpi)
library(parallel)

hello.works <- function(i) {
  print(i, mpi.comm.rank())
}

cluster <- makeCluster(mpi.universe.size(), type='MPI')

output.lines <- clusterApply(cl=cl, x=(1:10), fun=hello.world)
cat(unlist(output.lines), sep='\n')

stopCluster(cl)
mpi.exit()
