# Delta = 1
importance = 1

# Delta = 2 
importance = 1(A_{t+1}=0)/(1-p_{t+1})

# Delta = 3
importance = importance * 1(A_{t+2}=0)/(1-p_{t+2})



process.dat <- function(data, Delta = 1) {
  
}


beta.hat.list <- list()
for (i in 1:5) {
  dat.prc <- process.dat(data, Delta=i)
  beta.hat.list[i] <- weighted_centered_least_square(dat.prc, )
  
}