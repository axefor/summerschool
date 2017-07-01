program vectorsum
  use omp_lib
  use iso_fortran_env, only: int64, real64
  implicit none
  integer, parameter :: ik = int64
  integer(kind=ik), parameter :: nx = 102400_ik

  integer(kind=ik), dimension(nx) :: vecA
  integer(kind=ik) :: sum, psum, sumex
  integer(kind=ik) :: i
  real(kind=real64) :: stime, ftime, stime_cpu, ftime_cpu

  ! Initialization of vector
  do i = 1, nx
     vecA(i) = i
  end do
  
  call cpu_time(stime_cpu)
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

  ftime = omp_get_wtime()
  call cpu_time(ftime_cpu)
  write(*,*) 'omp time: ', ftime-stime
  write(*,*) 'cpu time: ', ftime_cpu-stime_cpu

  write(*,*) 'Sum: ', sum
end program vectorsum
