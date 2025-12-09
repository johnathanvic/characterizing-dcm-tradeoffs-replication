# Simple benchmarking function using system.time()
calc_func_eval_time <- function(choice_prob_func, choice_set, n_iterations = 10000) {
  # cat("Evaluating Function ", n_iterations, "times...\n")
  
  # Measure execution time
  timing <- system.time({
    for (i in 1:n_iterations) {
      result <- choice_prob_func(choice_set)
    }
  })
  
  # Calculate statistics using CPU time (user + system)
  cpu_time <- timing["user.self"] + timing["sys.self"]
  wall_time <- timing["elapsed"]
  avg_cpu_time_per_call <- cpu_time / n_iterations
  calls_per_second_cpu <- n_iterations / cpu_time
  
  # Print results
  # cat("Benchmark Results:\n")
  # cat("Total CPU time:", round(cpu_time, 4), "seconds\n")
  
  return(cpu_time)
  # return(timing)
  # # Return results as a list
  # return(list(
  #   cpu_time = cpu_time,
  #   wall_time = wall_time,
  #   avg_cpu_time_per_call = avg_cpu_time_per_call,
  #   calls_per_second_cpu = calls_per_second_cpu,
  #   n_iterations = n_iterations
  # ))
}