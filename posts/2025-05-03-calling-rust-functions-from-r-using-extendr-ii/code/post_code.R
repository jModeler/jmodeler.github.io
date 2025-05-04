# cleaned up version of the demo code
library(rextendr)
library(ggplot2)
library(microbenchmark)

# write memoized r function
fibonacci <- function(n) {
  fib <- vector('numeric', n)
  for(ii in 1:(n+1)) { 
    # R vector indices start at 1
    if(ii==1) {
      fib[ii] <- 0
    } else if(ii==2) {
      fib[ii] <- 1
    } else {
      fib[ii] <- fib[ii-1] + fib[ii-2]
    }
  }
  return(fib[n+1])
}

# run the R function
r_fn_result = fibonacci(15)

print(r_fn_result)

# test run time for a larger term number
system.time(fibonacci(40))

# write memoized Rust function
rust_memoized_code <- "
#[extendr]
fn fibonacci_rust(n:usize) -> i64 {
  let mut fibn: Vec<i64> = vec![0;(n+1)];
  for ii in 0..=n {
    if ii==0 {
    fibn[ii] = 0;
  } else if ii==1 {
    fibn[ii] = 1;
  } else {
    fibn[ii] = fibn[ii-1] + fibn[ii-2];
  }
  }
  return fibn[n];
}"

# compile source code
rust_source(code = rust_memoized_code)

# run the rust function
rustm_fn_result = fibonacci_rust(15)

print(rustm_fn_result)

# use microbenchmark to compare runtimes
# list of test values
values <- c(40, 50, 60)
# list to store microbenchmark results
compare <- vector('list', length(values))
for (ii in 1:length(values)) {
  compare[[ii]] <- microbenchmark(fibonacci(values[ii]), fibonacci_rust(values[ii]), times = 1000)
  # Change labels for plotting convenience later
  # Convert expressions to character strings
  expr_char <- as.character(compare[[ii]]$expr)
  
  # Map to new labels
  expr_char[expr_char == "fibonacci(values[ii])"] <- "R"
  expr_char[expr_char == "fibonacci_rust(values[ii])"] <- "Rust"
  
  # Assign the new factor with correct levels
  compare[[ii]]$expr <- factor(expr_char, levels = c("R", "Rust"))
}

# plot benchmark times, term number = 40
autoplot(compare[[1]]) +
  ggtitle(sprintf("Rust vs R: Microbenchmark Results for Term Number: %d", values[1]))
# plot benchmark times, term number = 50
autoplot(compare[[2]]) +
  ggtitle(sprintf("Rust vs R: Microbenchmark Results for Term Number: %d", values[2]))
# plot benchmark times, term number = 60
autoplot(compare[[3]]) +
  ggtitle(sprintf("Rust vs R: Microbenchmark Results for Term Number: %d", values[3]))
