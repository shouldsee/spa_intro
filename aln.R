# # devtools::use_testthat()
# library(testthat) 
# 
# source("path/to/fibo.R")
# 
# test_results <- test_dir("path/to/tests", reporter="summary")
# 
# test_that
library(assertthat)
are_equal(1,1)


# assert_that()

# ?dnorm
dp = pnorm(seq(-5,5,length = 100))
dp = dnorm(seq(-5,5,length = 100), mean = 10)
plot (dp,type = 'l')
# 
# Biostrings::substitution.matrices 
# browseVignettes("Biostrings")
# source("https://bioconductor.org/biocLite.R")
# biocLite("Biostrings")

# ?rnorm
set.seed(0)


mfrow = c(2,2)
plot.new()
par( mfrow = mfrow)
xs = seq(-5,5,length = 100)
param = c(100,400,600,700)

#### Starting plotting
assert_that( length(param) ==  prod(mfrow))
for (p in param){
  cnt = p
  vct <- rnorm( cnt )
  plot( density(vct) )
  lines(xs,dnorm(xs) , col = 2)
  # ?hist
  ?lines
}

####
plot.new()
par( mfrow = mfrow)
param = rep(0,100)
cnt = 100
?rnorm
mat = matrix(rnorm( 100*cnt ), 100,100)
mvct = apply( mat, MARGIN = 2, mean)
sdvct = apply( mat, MARGIN = 2, sd)
plot( density(rbind(mvct,sdvct)) )
plot(mvct,sdvct,xlim = range(xs))
# ?pnorm(0.99)

########
tout = t.test(mvct,conf.level = c(0.05))
tout$conf.int
# assert_that()
