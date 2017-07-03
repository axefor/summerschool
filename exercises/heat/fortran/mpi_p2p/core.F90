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

    integer :: ierr, sendTag, recvTag, a1, a2, a3, a4
    integer :: request(4)
    integer :: status(MPI_STATUS_SIZE,4)
    integer, parameter :: comm = MPI_COMM_WORLD
    integer, parameter :: fieldDataType = MPI_DOUBLE_PRECISION
    integer :: myRank

    call mpi_comm_rank(comm, myRank, ierr)
    !  start: implement halo exchange
    ! Send to left, receive from right
    sendTag = 10
    recvTag = 10
    call mpi_isend(field0%data(0,1), field0%nx+2, fieldDataType, parallel%nleft, sendTag, comm, request(1), ierr)
!    call mpi_isend(field0%data(0,1), field0%nx+2, fieldDataType, parallel%nleft, sendTag, comm, a1, ierr)
    call mpi_irecv(field0%data(0,field0%ny+1), field0%nx+2, fieldDataType, parallel%nright, recvTag, comm, request(2), ierr)
!    call mpi_irecv(field0%data(0,field0%ny+1), field0%nx+2, fieldDataType, parallel%nright, recvTag, comm, a2, ierr)

    ! Send to right, receive from left
    sendTag = 11
    recvTag = 11
    call mpi_isend(field0%data(0,field0%ny), field0%nx+2, fieldDataType, parallel%nright, sendTag, comm, request(3), ierr)
!    call mpi_isend(field0%data(0,field0%ny), field0%nx+2, fieldDataType, parallel%nright, sendTag, comm, a3, ierr)
    call mpi_irecv(field0%data(0,0), field0%nx+2, fieldDataType, parallel%nleft, recvTag, comm, request(4), ierr)
!    call mpi_irecv(field0%data(0,0), field0%nx+2, fieldDataType, parallel%nleft, recvTag, comm, a4, ierr)

!    call mpi_waitall(4, (/ a1, a2, a3, a4 /), status, ierr)
    call MPI_WAITALL(4, REQUEST, STATUS, ierr)
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
