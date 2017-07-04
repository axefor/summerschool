program datatype1
  use mpi
  implicit none

  integer, dimension(8,8) :: array
  integer :: rank, ierr, dest, source, tag
  ! declare variable for datatype
  integer :: mpiDataType, mpiStatus(MPI_STATUS_SIZE)
  integer :: i, j

  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, rank ,ierr)

  ! initialize arrays
  if (rank == 0) then
     do i=1,8
        do j=1,8
           array(i,j) = i*10 + j
        end do
     end do
  else
     array(:,:) = 0
  end if

  ! create datatype describing one row, use mpi_type_vector
  call mpi_type_vector( size(array, 2), 1, size(array,1), MPI_INTEGER, mpiDataType, ierr)
  call mpi_type_commit( mpiDataType, ierr)

  ! send first row of matrix from rank 0 to 1
  dest = 1
  source = 0
  tag = 11

  if (rank == 0) then
    call mpi_send( array, 1, mpiDataType, dest, tag, MPI_COMM_WORLD, ierr)
  else
    call mpi_recv( array, 1, mpiDataType, source, tag, MPI_COMM_WORLD, mpiStatus, ierr)
  end if

  ! Print out the result
  if (rank == 1 .or. rank == 0) then
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if

  write(*,*) 'Im here  ', rank
  ! free datatype
  call mpi_type_free( mpiDataType, ierr)

  call mpi_finalize(ierr)

end program datatype1
