program vectorsum
  use omp_lib
  use iso_fortran_env, only: int64, real64
  implicit none
  integer, parameter :: rk = real64
  integer, parameter :: ik = selected_int_kind(9)
  integer, parameter :: nx = 102400

  real(kind=rk), dimension(nx) :: vecA, vecB, vecC
  real(kind=rk)    :: sum
  real(kind=rk)    :: stime, ftime, stime_cpu, ftime_cpu
  integer(kind=ik) :: i

  ! Initialization of vectors
  do i = 1, nx
    vecA(i) = 1.0_rk/(real(nx - i + 1, kind=rk))
    vecB(i) = vecA(i)**2
  end do

  stime = omp_get_wtime()
  call cpu_time(stime_cpu)
  ! TODO:
  !   Implement here the parallelized version of vector addition,
  !   vecC = vecA + vecB
  vecC(:) = vecA(:) + vecB(:)

  ftime = omp_get_wtime()
  call cpu_time(ftime_cpu)
  write(*,*) 'Elapsed time for vector addition: ', ftime-stime
  write(*,*) 'CPU time for vector addition: ', ftime_cpu-stime_cpu

  ! Compute the check value
  write(*,*) 'Reduction sum: ', sum(vecC)

end program vectorsum
