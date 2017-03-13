

if (!is.loaded("mpi_initialize")) {
    library("Rmpi")
}

mpi.spawn.Rslaves(nslaves=mpi.universe.size()-1)

mpi.remote.exec(paste(mpi.comm.size(), mpi.comm.rank()))

mpi.close.Rslaves()
