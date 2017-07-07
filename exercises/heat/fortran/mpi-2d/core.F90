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

    integer :: ierr

    integer :: mpiStatus(MPI_STATUS_SIZE)
    integer, parameter :: comm = MPI_COMM_WORLD
    integer :: fieldDataType = MPI_DOUBLE_PRECISION
    integer :: myRank
    
    ! TODO start: implement 2D halo exchange using MPI datatypes

    ! Send to left, receive from right
    sendTag = 10
    recvTag = 10
    fieldDataType = MPI_DOUBLE_PRECISION
    call mpi_sendrecv( field0%data(0,1), field0%nx+2, fieldDataType, parallel%nleft, sendTag, comm, &
      field0%data(0,field0%ny+1), field0%nx+2, fieldDataType, parallel%nright, recvTag, comm, mpiStatus, ierr)

    ! Send to right, receive from left
    sendTag = 11
    recvTag = 11
    fieldDataType = MPI_DOUBLE_PRECISION
    call mpi_sendrecv( field0%data(0,field0%ny), field0%nx+2, fieldDataType, parallel%nright, sendTag, comm, &
      field0%data(0,0), field0%nx+2, fieldDataType, parallel%nleft, recvTag, comm, mpiStatus, ierr)

    ! Create new datatype before sending/receiving to/from up/down
    blocklen = 1
    stride = field0%nx+2
    call mpi_type_vector( field0%ny+2, blocklen, stride, fieldDataType)
    ! Send to up receive from down
    sendTag = 12
    recvTag = 12
    call mpi_sendrecv( field0%data(1,0), field0%ny+2, fieldDataType, parallel%nup, sendTag, comm, &
      field0%data(field0%nx+1,0), field0%ny+2, fieldDataType, parallel%ndown, recvTag, comm, mpiStatus, ierr)

    ! Send to the down, receive from up
    sendTag = 13
    recvTag = 13
    call mpi_sendrecv( field0%data(field0%nx,0), field0%ny+2, fieldDataType, parallel%ndown, sendTag, comm, &
      field0%data(0,0), field0%ny+2, fieldDataType, parallel%nup, recvTag, comm, mpiStatus, ierr)

    ! TODO end

  end subroutine exchange

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value
  subroutine evolve(curr, prev, a, dt)

    implicit none

    type(field), intent(inout) :: curr, prev
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
