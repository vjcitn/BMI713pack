
myst = function(ivec) {
  nu = length(unique(ivec))
  ans = rep(0, length(unique(ivec)))
  names(ans) = sort(unique(ivec))
  ac = as.character
  for (j in ivec) ans[ac(j)] = ans[ac(j)]+1
  ans[ans>0]
}

