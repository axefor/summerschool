program vectorsum
  use iso_fortran_env, only: int64
  use omp_lib
  implicit none
  integer, parameter :: ik = int64
  integer(kind=ik), parameter :: nx = 102400_ik

  integer(kind=ik), dimension(nx) :: vecA
  integer(kind=ik) :: sum, psum, sumex
  integer(kind=ik) :: i
  real :: stime

  ! Initialization of vector
  do i = 1, nx
     vecA(i) = i
  end do
  
  stime = omp_get_wtime()

  sum = 0
  
  !$omp parallel private(i,psum) shared(vecA) 

    psum = 0

    !$omp do schedule(guided)
    do i = 1, nx
      psum = psum + vecA(i)
    end do
    !$omp end do
  
    !$omp critical(dosum)
      sum = sum + psum
    !$omp end critical(dosum)

  !$omp end parallel

  stime= omp_get_wtime()-stime
  write(*,*) stime

  write(*,*) 'Sum: ', sum
end program vectorsum
