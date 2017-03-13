

library("Rmpi")

mpi.spawn.Rslaves()

mpi.remote.exec(paste(mpi.comm.size(), mpi.comm.rank()))

mpi.close.Rslaves()
