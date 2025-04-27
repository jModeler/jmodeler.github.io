library(rextendr)
library(parallel)

# checking the rust installation using the instructions here: https://github.com/extendr/rextendr
# rust_sitrep()

# write code to calculate the nth fibonacci term
rust_code <- "
#[extendr]
fn fibonacci_rust(n:i32) -> i64 {
  let mut fibn:i64 = 0;
  if n==0 {
    return 0;
  } else if n==1 {
    return 1;
  } else {
    return fibonacci_rust(n-1) + fibonacci_rust(n-2);
  }
}
"

# compile source code
rust_source(code = rust_code)

# run the rust function
rust_fn_result = fibonacci_rust(15)

# write r function
fibonacci <- function(n) {
  if(n==0) {
    return(0)
  } else if(n==1) {
    return(1)
  } else {
    return(fibonacci(n-1) + fibonacci(n-2))
  }
}

# run the R function
r_fn_result = fibonacci(15)

# speed test, choosing one value only since microbenchmark might take too much time
system.time(fibonacci_rust(40))
# user  system elapsed 
# 1.432   0.005   1.441 
system.time(fibonacci(40))
# user  system elapsed 
# 164.995   0.451 166.031 

# collecting times for a plot

# create functions that collects elapsed times
elapsed_time_rust <- function(x) {
  system.time(fibonacci_rust(x))['elapsed']
}

elapsed_time_r <- function(x) {
  system.time(fibonacci(x))['elapsed']
}

# set input values
n <- 1:40

# call rust function, also note the time it takes to run for all inputs
rust_total_runtime <- system.time({
  rust_times <- sapply(n, FUN = elapsed_time_rust) 
})

# running the R function in parallel, since it will take longer as n gets larger
num_cores <- max(1, detectCores() - 1)

print(num_cores)

# Create a cluster with the specified number of worker processes
cl <- makeCluster(num_cores)

# Export the helper function to all workers
clusterExport(cl, varlist = c("fibonacci"))

# Use parLapply to apply the function in parallel
r_total_runtime <- system.time({
  r_times <- parLapply(cl, n, fun = elapsed_time_r)
})

# user  system elapsed 
# 0.375   0.332 330.877 

# Simplify the result into a vector
r_times <- unlist(r_times)

# stop cluster
stopCluster(cl)

# plot this
# Create the plot with the first line (y1)
plot(n, rust_times, type = "l", col = "#D84B16", lwd = 2, xlab = "Fibonacci Term Number", 
     ylab = "Time Taken (Seconds)", main = "Rust vs R Time Comparison",
     ylim = c(0, max(rust_times, r_times) + 1))

# Add the second line (y2) to the same plot
lines(n, r_times, col = "blue", lwd = 2)

# Add a legend with custom labels for each line
legend("topleft", legend = c("Rust", "R"), col = c("#D84B16", "blue"), lwd = 2)


save.image("results.RData")