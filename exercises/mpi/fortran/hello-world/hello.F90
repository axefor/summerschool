program hello
  use mpi
  implicit none
  integer :: rc, rank, p_size

  call mpi_init(rc)

  call mpi_comm_rank(mpi_comm_world, rank, rc)
  print *, rank
  
  if (rank==0) then
    call mpi_comm_size(mpi_comm_world, p_size, rc)
    print *, 'Num of processes: ', p_size
  end if

  call mpi_finalize(rc)

end program hello
