program arrays
  implicit none
  integer :: nx, ny
  integer :: i, alloc_stat
  ! TODO: define allocatable array A
  real, allocatable :: A(:,:)

  write (*,*) 'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny

  ! TODO allocate A now that we know nx and ny
  allocate (A(nx,ny), stat=alloc_stat)
  if (alloc_stat /= 0) call abort()

  ! TODO Use array syntax to initialize the array
  do i=1,ny
    A(:,i)=i
  end do






  !--------------------------------------------------
  ! Print out the array
  do i = 1, nx
    write(*,'(*(F6.1))') A(i,:)
  end do


end program arrays
