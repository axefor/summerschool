program basic
  use mpi
  use omp_lib
  use iso_fortran_env, only : REAL64

  implicit none
  integer, parameter :: size = 10000000
  character(len=25) :: arg
  integer :: rc, mpi_rank, omp_rank, mpi_ntasks, dest(2), source, sendTag, recvTag
  integer :: status(MPI_STATUS_SIZE)
  !integer :: thrdComm
  integer :: receiveBuffer, mpi_thread_safety

  real(REAL64) :: t0, t1 

  call mpi_init_thread(MPI_THREAD_MULTIPLE, mpi_thread_safety, rc)
  call mpi_comm_rank(MPI_COMM_WORLD, mpi_rank, rc)
  call mpi_comm_size(MPI_COMM_WORLD, mpi_ntasks, rc)

  if (mpi_thread_safety /= MPI_THREAD_MULTIPLE) then 
    print *, 'MPI thread safety MULTIPLE not set'
    call mpi_finalize(rc)
    call abort()
  end if
  if (mpi_ntasks /= 3) then 
    print *, 'MPI processes has to be 3'
    call mpi_finalize(rc)
    call abort()
  end if

  dest = (/ 1, 2 /)    ! sedn to process 1 and 2
  source = 0

  ! Send and receive as defined in the assignment
  !$omp parallel private(omp_rank, sendTag, recvTag, receiveBuffer, status, rc)
  omp_rank = omp_get_thread_num()
  sendTag = 10 + omp_rank
  recvTag = 10 + omp_rank
  if ( mpi_rank == 0 ) then
    print *, dest(1), dest(2), sendTag, recvTag
    call mpi_send(omp_rank, 1, MPI_INTEGER, dest(1), sendTag, MPI_COMM_WORLD, status, rc)
    call mpi_send(omp_rank, 1, MPI_INTEGER, dest(2), sendTag, MPI_COMM_WORLD, status, rc)
  else
    call mpi_recv(receiveBuffer, 1, MPI_INTEGER, source, recvTag, MPI_COMM_WORLD, status, rc)
  end if

  if ( mpi_rank == 0 ) then 
    write(*,*) 'Sender: ', mpi_rank, ', ', omp_rank
  else
    write(*,*) 'Receiver; sender omp rank: ', mpi_rank, ', ', omp_rank, '; ', &
    receiveBuffer
  end if
  !$omp end parallel

  call mpi_finalize(rc)

end program basic
