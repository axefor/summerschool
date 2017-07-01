program exer1
  implicit none
  integer :: var1, var2, var3
  var1 = 1
  var2 = 2

  ! TODO:
  !   Test different data sharing clauses here
  
  !$omp parallel shared(var1) firstprivate(var2, var3)
    !var2 = 1
    print *, 'Region 1:       var1=', var1, 'var2=', var2
    var1 = var1 + 1
    var2 = var2 + 1
  !$omp end parallel

  print *, 'After region 1: var1=', var1, 'var2=', var2
  print *

end program exer1
