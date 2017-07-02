! Main solver routines for heat equation solver
module core
    use heat

contains

  ! Exchange the boundary data between MPI tasks
  subroutine exchange(field0, parallel)
    use mpi

    implicit none

    type(field), intent(inout) :: field0
    type(parallel_data), intent(in) :: parallel

    integer :: ierr, sendTag, recvTag
    integer :: status(MPI_STATUS_SIZE)
    integer, parameter :: comm = MPI_COMM_WORLD
    integer, parameter :: fieldDataType = MPI_DOUBLE_PRECISION
    integer :: myRank

    call mpi_comm_rank(comm, myRank, ierr)
    !  start: implement halo exchange
    ! Send to left, receive from right
    sendTag = 10
    recvTag = 10
    call mpi_isend(field0%data(0,1), field0%nx+2, fieldDataType, parallel%nleft, sendTag, comm, status, ierr)
    call mpi_irecv(field0%data(0,field0%ny+1), field0%nx+2, fieldDataType, parallel%nright, recvTag, comm, status, ierr)

    ! Send to right, receive from left
    sendTag = 11
    recvTag = 11
    call mpi_isend(field0%data(0,field0%ny), field0%nx+2, fieldDataType, parallel%nright, sendTag, status, ierr)
    call mpi_irecv(field0%data(0,0), field0%nx+2, fieldDataType, parallel%nleft, recvTag, comm, status, ierr)

    !  end

  end subroutine exchange

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value
  subroutine evolve(curr, prev, a, dt)

    implicit none

    type(field), intent(in) :: prev
    type(field), intent(inout) :: curr
    real(dp) :: a, dt
    integer :: i, j, nx, ny

    nx = curr%nx
    ny = curr%ny

    do j = 1, ny
       do i = 1, nx
          curr%data(i, j) = prev%data(i, j) + a * dt * &
               & ((prev%data(i-1, j) - 2.0 * prev%data(i, j) + &
               &   prev%data(i+1, j)) / curr%dx**2 + &
               &  (prev%data(i, j-1) - 2.0 * prev%data(i, j) + &
               &   prev%data(i, j+1)) / curr%dy**2)
       end do
    end do
  end subroutine evolve

end module core
