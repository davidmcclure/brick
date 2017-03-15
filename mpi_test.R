

library(Rmpi)

mpi.spawn.Rslaves(nslaves=4)

print(mpi.universe.size())
print(mpi.comm.size())

mpi.remote.exec(paste(mpi.comm.size(), mpi.comm.rank()))

mpi.close.Rslaves()
mpi.exit()
