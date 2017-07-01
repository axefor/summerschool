program hello
  use omp_lib
  implicit none
  integer :: thread_num, num_thread

  print *, 'Hello world!'
  !$omp parallel
  thread_num = omp_get_thread_num()
  num_thread = omp_get_num_threads()
  print *, thread_num, ' (', num_thread, ')'
  !$omp end parallel

end program hello
