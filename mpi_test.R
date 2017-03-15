
library(Rmpi)
library(parallel)

hello.world <- function(i) {
  Sys.sleep(1)
  return(i+1)
}

cluster <- makeCluster(mpi.universe.size(), type='MPI')

res <- clusterApply(cl=cl, x=(1:4), fun=hello.world)
print(res)

stopCluster(cl)
mpi.exit()
