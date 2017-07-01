program exchange
  use mpi 
  implicit none
  integer, parameter :: size = 10000
  integer :: rc, myid, ntasks, count, source, destination, tag
  integer :: status(MPI_STATUS_SIZE)
  integer :: message(size)
  integer :: receiveBuffer(size)

  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, myid, rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)

  message = myid

  ! TODO: Implement sending and receiving as defined in the assignment
  if ( myid == 0 ) then
    source = 1
    destination = 1
    tag = 10
    call mpi_send(message, size, MPI_INTEGER, destination, tag, MPI_COMM_WORLD, rc)
    call mpi_recv(receiveBuffer, size, MPI_INTEGER, source, tag, MPI_COMM_WORLD, status, rc)

    write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
      ' received ', receiveBuffer(1)
  else if (myid == 1) then
    source = 0
    destination = 0
    tag = 10
    call mpi_send(message, size, MPI_INTEGER, destination, tag, MPI_COMM_WORLD, rc)
    call mpi_recv(receiveBuffer, size, MPI_INTEGER, source, tag, MPI_COMM_WORLD, status, rc)

    write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
      ' received ', receiveBuffer(1)
  end if

  call mpi_finalize(rc) 

end program exchange
