program vectorsum
  use iso_fortran_env, only: int64
  use omp_lib
  implicit none
  integer, parameter :: ik = int64
  integer(kind=ik), parameter :: nx = 10240_ik

  integer(kind=ik), dimension(nx) :: vecA
  integer(kind=ik) :: sum, psum, sumex
  integer(kind=ik) :: i,j
  real :: stime

  ! Initialization of vector
  do i = 1, nx
     vecA(i) = i
  end do
  
  stime = omp_get_wtime()

  sum = 0
  ! TODO: Parallelize the computation
  !$omp parallel do private(i,j) shared(sum,vecA)
  do i = 1, nx
    do j = 1, nx
     sum = sum + vecA(i)
   end do
  end do
  !$omp end parallel do
  
  write(*,*) omp_get_wtime()-stime

  write(*,*) 'Sum: ', sum
end program vectorsum
