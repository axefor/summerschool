program pario
  use mpi
  use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
  implicit none

  integer, parameter :: datasize = 64, writer_id = 0
  integer :: rc, my_id, ntasks, localsize, i
  integer, dimension(:), allocatable :: localvector
  integer, dimension(datasize) :: fullvector
  integer :: status(MPI_STATUS_SIZE)

  call mpi_init(rc)
  call mpi_comm_size(mpi_comm_world, ntasks, rc)
  call mpi_comm_rank(mpi_comm_world, my_id, rc)

  if (ntasks > 64) then
    write(error_unit, *) 'Maximum number of tasks is 64!'
    call mpi_abort(MPI_COMM_WORLD, -1, rc)
  end if

  if (mod(datasize, ntasks) /= 0) then
    write(error_unit,*) 'Datasize (64) should be divisible by number of tasks'
    call mpi_abort(MPI_COMM_WORLD, -1, rc)
  end if

  localsize = datasize / ntasks
  allocate(localvector(localsize))

  localvector = [(i + my_id * localsize, i=1,localsize)]

  call single_writer()

  deallocate(localvector)
  call mpi_finalize(rc)

contains

  subroutine single_writer()
    implicit none
    integer :: dest, source, sendTag, recvTag, i

    !  Implement a function that writers the whole array of elements
    !  to a file so that single process is responsible for the file io
    dest=0
    sendTag=11
    recvTag=11
    if (my_id >0 ) then 
      call mpi_send(localvector, size(localvector), MPI_INTEGER, dest, sendTag, MPI_COMM_WORLD, rc)
    else
      do i=1, ntasks-1
        source=i
        write(*,*) 'rank, source', my_id, source
        call mpi_recv(fullvector(i*size(localvector)), size(localvector), MPI_INTEGER, source, recvTag, MPI_COMM_WORLD, status, rc)
      end do
      open(11, file='output.out', access="stream", action="write", form="unformatted")
      write(11, *) fullvector
      close(11)
    end if

  end subroutine single_writer

end program pario
