# we define a factor structure for a biallelic genotype
# define the punnett square, and use the frequencies
# obtained from this to simulate genotypes in families and
# populations
# 
asGT = function(x) {
  stopifnot(x %in% c("AA", "Aa", "aa"))
  factor(x, levels=c("AA", "Aa", "aa"))
}
isGT = function(x) is.factor(x) & all(levels(x) == c("AA", "Aa", "aa")) &
   all(as.character(x) %in% c("AA", "Aa", "aa"))
GT2vec = function(x) strsplit(as.character(x), "")


punnett = function(g1, g2) {
#
# very simple case -- genotype is a string %in% c('AA', 'Aa', 'aa')
# asGT converts the string to a factor, and such must be input here
# GT2vec converts it back to character
# this will return a table of genotype frequencies of the mating
#
 stopifnot(isGT(g1), isGT(g2), length(g1)==1, length(g2)==1)
 v1 = GT2vec(g1)[[1]]
 v2 = GT2vec(g2)[[1]]
 poss = expand.grid(v1, v2, stringsAsFactors=FALSE)
 sx = function(x) sort(x, decreasing=TRUE) # expand.grid will create ("a", "A")
 table(asGT(apply(poss,1,function(x) paste(sx(x), collapse=""))))/4
}

make_pop1 = function(n, gtf=c(.5,.4,.1)) {
  sample(asGT(c("AA", "Aa", "aa")), size=n, replace=TRUE,
     prob=gtf)
}

make_family = function(g1, g2, size) {
  sample( asGT(c("AA", "Aa", "aa")), size=size, replace=TRUE,
     prob=punnett(g1, g2) )
}

random_mate = function(ncoup=500, gtf=c(.7,.2,.1), famsize=1) {
  pop1 = make_pop1(n=ncoup, gtf=gtf)
  pop2 = make_pop1(n=ncoup, gtf=gtf)
  offs = vector("list", length=ncoup)
  for (i in 1:ncoup) 
    offs[[i]] = make_family(pop1[i], pop2[i], size=famsize)
  list(moms=pop1, dads=pop2, offs=offs)
}


