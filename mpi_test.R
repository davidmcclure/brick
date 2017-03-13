

library(Rmpi)

print(mpi.comm.size())

#mpi.remote.exec(paste(mpi.comm.size(), mpi.comm.rank()))

mpi.close.Rslaves()
mpi.exit()
