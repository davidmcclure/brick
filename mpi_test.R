

library(Rmpi)

ns <- mpi.universe.size() - 1
mpi.spawn.Rslaves(nslaves=ns)

print(mpi.universe.size(), mpi.comm.size())

#mpi.remote.exec(paste(mpi.comm.size(), mpi.comm.rank()))

mpi.close.Rslaves()
mpi.exit()
