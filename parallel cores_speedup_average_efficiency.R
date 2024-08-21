library(foreach)
library(doParallel)

timeconsumingfun <- function(){
  Sys.sleep(0.1)
}

# Setting up the parallel backend
N_of_processors <- 10
cl <- makeCluster(N_of_processors)
registerDoParallel(cl)

print(N_of_processors)

iteration <- c(10, 100, 300, 500, 700, 1000)
Sp <- c()
Ep <- c()

for (m in iteration) {
  
  # Parallel Computing
  parallelTime1 <- Sys.time()
  
  foreach(i=1:m, .combine='c', .packages='doParallel') %dopar% {
    timeconsumingfun()
  }
  
  parallelTime <- as.numeric(difftime(Sys.time(), parallelTime1, units="secs"))
  cat("Parallel computational time: ", parallelTime, "\n")
  
  # Serial Computing
  SeriTime1 <- Sys.time()
  
  for(i in 1:m) {
    timeconsumingfun()
  }
  
  SeriTime <- as.numeric(difftime(Sys.time(), SeriTime1, units="secs"))
  cat("Serial computational time: ", SeriTime, "\n")
  
  # Computing Speedup and Average efficiency for each iteration
  Sp <- c(Sp, SeriTime/parallelTime)
  Ep <- c(Ep, SeriTime/(N_of_processors*parallelTime))
}

cat("SpeedUp: ", Sp, "\n")
cat("Average Efficiency: ", Ep, "\n")

plot(iteration, Sp, type='o', col='green', xlab='Iterations', ylab='SpeedUp', main='Using 10 Processors')
grid(col="gray")

plot(iteration, Ep, type='o', col='red', xlab='Iterations', ylab='Efficiency', main='Using 10 Processors', ylim=c(0,1))
grid(col="gray")



