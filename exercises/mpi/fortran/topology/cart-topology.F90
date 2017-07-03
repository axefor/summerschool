program cart_test
  use mpi
  implicit none
  
  integer :: ntask,  & ! number of MPI tasks
       my_id,        & ! MPI rank of the task
       my_cart_id,   & ! MPI rank for 2d comm
       ierr,         & ! return code
       comm2d,       & ! Cartesian communicator
       neighbors(4), & ! neighbors in the 2D grid
       coord(0:1),   & ! coordinates in the grid
       dims(0:1)       ! dimensions of the grid
  logical, dimension(2) :: period = (/ .true., .true. /)
  integer :: irank
  
  call mpi_init(ierr)
  call mpi_comm_size(MPI_COMM_WORLD, ntask, ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, my_id, ierr)
  
  ! Determine the process grid (dims(0) x dims(1) = ntask)
  if (ntask < 16) then
     dims(0) = 2
  else if (ntask >= 16 .and. ntask < 64) then
     dims(0) = 4
  else if (ntask >= 64 .and. ntask < 256) then
     dims(0) = 8
  else
     dims(0) = 16
  end if
  
  dims(1) = ntask / dims(0)
  if (dims(0) * dims(1) /= ntask) then
     write(*,'(A,I3,A1,I3,A2,I4)') 'sorry, no go', dims(0), 'x', &
          dims(1),'/=', ntask
     call mpi_abort(mpi_comm_world, 1, ierr)
  end if

  ! Create the 2D Cartesian communicator
  call mpi_cart_create(MPI_COMM_WORLD, 2, dims, period, .false., comm2d, ierr)

  ! Find out & store the neighboring ranks 
  call mpi_cart_shift(comm2d, 0, 1, neighbors(1), neighbors(2), ierr)
  call mpi_cart_shift(comm2d, 1, 1, neighbors(3), neighbors(4), ierr)

  ! Find out & store also the Cartesian coordinates of a rank
  call mpi_comm_rank(comm2d, my_cart_id, ierr)
  call mpi_cart_coords(comm2d, my_cart_id, 2, coord, ierr)

  
  do irank = 0, ntask-1
     if (my_id == irank) print '(I3,A,2I2,A,4I3)', &
          my_id, '=', coord, ' neighbors=', neighbors(:)
     call mpi_barrier(mpi_comm_world, ierr)
  end do
  
  call mpi_finalize(ierr)

end program cart_test
