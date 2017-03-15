
library(Rmpi)
library(parallel)

work <- function(i) {
  Sys.sleep(1)
  return(c(i, T))
}

np <- mpi.universe.size() - 1
cl <- makeCluster(np, type='MPI')

system.time({
  res <- clusterApply(cl=cl, x=(1:np), fun=work)
  print(res)
})

stopCluster(cl)
mpi.exit()
