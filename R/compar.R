
compar = function(f1, f2, dat, times=5) {
 n1 = deparse(substitute(f1))
 n2 = deparse(substitute(f2))
 ans = lapply(list(f1, f2), 
      function(f) microbenchmark(f(dat), times=times,
         unit="ms"))
 names(ans) = c(n1,n2)
 ans
}

