
library(foreach)
library(snow)
library(doSNOW)

work <- function(i) {
  Sys.sleep(1)
  return(i+1)
}

cl <- makeCluster(mpi.universe.size(), type='MPI')
registerDoSNOW(cl)

res <- foreach(i = (1:4) %dopar% {
  work(i)
})

print(res)

stopCluster(cl)
mpi.exit()
