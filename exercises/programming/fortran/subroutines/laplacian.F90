module laplacian_mod
  implicit none
  real, parameter :: dx = 0.01, dy = 0.01
  
contains
  
  subroutine initialize(field0)
! TODO: implement a subroutine that initializes the input array
    field0(:,:)  = 65.0 ! middle
    field0(:,1)  = 20.0 ! left
    field0(:,ny) = 70.0 ! right
    field0(1,:)  = 85.0 ! top
    field0(nx,:) = 5.0  ! bottom
  end subroutine initialize
   
  subroutine laplacian(curr, prev)
! TODO: insert a subroutine that computes a laplacian of the
! array "prev" and returns it as an array "curr"
  
  end subroutine laplacian

  subroutine write_field(array)
! TODO: write a subroutine that prints "array" on screen
  end subroutine write_field

end module laplacian_mod
