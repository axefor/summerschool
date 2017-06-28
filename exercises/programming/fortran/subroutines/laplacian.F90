module laplacian_mod
  implicit none
  real, parameter :: dx = 0.01, dy = 0.01
  
contains
  
  subroutine initialize(field0)
! TODO: implement a subroutine that initializes the input array
    real, intent(inout) :: field0(:,:)
    real :: nx, ny 
    nx=size(field0,1)
    ny=size(field0,2)
    field0(:,:)  = 65.0 ! middle
    field0(:,1)  = 20.0 ! left
    field0(:,ny) = 70.0 ! right
    field0(1,:)  = 85.0 ! top
    field0(nx,:) = 5.0  ! bottom
  end subroutine initialize
   
  subroutine laplacian(curr, prev)
    real :: i, j
    real, intent(out) :: curr(:,:)
    real, intent(in) :: prev(:,:)
    real :: nx, ny
    if (size(curr,1)==size(prev,1) .and. size(curr,2)==size(prev,2)) then
      nx=size(curr,1)
      ny=size(curr,2)
    else
      write(*,*) 'Error !!! matrices for laplacian not of the same size'
      call abort()
    end if
! TODO: insert a subroutine that computes a laplacian of the
! array "prev" and returns it as an array "curr"
    do j=2, ny-1
      do i=2, nx-1
        curr(i,j) = (prev(i-1,j)-2*prev(i,j)+prev(i+1,j))/dx**2 + (prev(i,j-1)-2*prev(i,j)+prev(i,j+1))/dy**2
      end do
    end do
  end subroutine laplacian

  subroutine write_field(array)
    real :: i
    real, intent(in) :: array(:,:)
    integer :: nx
    nx=size(array,1)
! TODO: write a subroutine that prints "array" on screen
    do i = 1, nx
      write(*,'(*(G10.1))') array(i,:)
    end do
    ! TODO
  end subroutine write_field

end module laplacian_mod
