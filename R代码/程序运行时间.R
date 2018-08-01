ptm <- proc.time()
for (i in 1:10000) x <- rnorm(1000)
proc.time() - ptm

system.time(for (i in 1:10000) x <- rnorm(1000))
