program basic
  use mpi
  use iso_fortran_env, only : REAL64

  implicit none
  integer, parameter :: size = 100000
  integer :: rc, myid, ntasks, count, dest, source, sendTag, recvTag
  integer :: status(MPI_STATUS_SIZE)
  integer :: message(size)
  integer :: receiveBuffer(size)

  real(REAL64) :: t0, t1 

  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, myid, rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)

  message = myid

  ! Start measuring the time spent in communication
  call mpi_barrier(mpi_comm_world, rc)
  t0 = mpi_wtime()

  ! Send and receive as defined in the assignment
  if ( myid == 0 ) then
    dest = myid + 1
    sendTag = 10 + myid
    call mpi_send(message, count, MPI_INTEGER, dest, sendTag, MPI_COMM_WORLD, rc)

    write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
      ' Sent elements: ',size, &
      '. Tag: ', sendTag, '. Receiver: ', myid+1
  end if

  if ( myid < ntasks-1 .and. myid > 0) then
    dest = myid + 1
    source = myid - 1
    sendTag = 10 + myid
    recvTag = 9 + myid
    call mpi_sendrecv(message, count, MPI_INTEGER, dest, sendTag, &
      receiveBuffer, count, MPI_INTEGER, source, recvTag, MPI_COMM_WORLD, status, rc)

    write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
      ' Sent elements: ',size, &
      '. Tag: ', sendTag, '. Receiver: ', myid+1
    write(*,'(A10,I3,A,I3)') 'Receiver: ', myid, &
      ' First element: ', receiveBuffer(1)
  end if

  if ( myid == ntasks-1 ) then
    source = myid - 1
    recvTag = 9 + myid
    call mpi_recv(receiveBuffer, count, MPI_INTEGER, source, recvTag, MPI_COMM_WORLD, status, rc)

    write(*,'(A10,I3,A,I3)') 'Receiver: ', myid, &
      ' First element: ', receiveBuffer(1)
  end if

  ! Finalize measuring the time and print it out
  t1 = mpi_wtime()
  call mpi_barrier(mpi_comm_world, rc)
  call flush(6)

  write(*, '(A20, I3, A, E12.6)') 'Time elapsed in rank', myid, ':', t1-t0

  call mpi_finalize(rc)

end program basic
