
library(Rmpi)
library(parallel)

work <- function(i) {
  Sys.sleep(1)
  return(i+1)
}

cl <- makeCluster(mpi.universe.size(), type='MPI')

system.time({
  res <- clusterApply(cl=cl, x=(1:4), fun=work)
  print(res)
})

stopCluster(cl)
mpi.exit()
