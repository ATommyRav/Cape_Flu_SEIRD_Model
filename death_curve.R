
library(deSolve)
epsilon_doctor <- 0.00327
epsilon_no_doctor <- 0.01935
times <- seq(0, 365, by = 1)
run_model <- function(epsilon1_value) {
  
  param_vals <- c(
    beta = beta_calc(100),
    N = N0,
    sigma = 1/2,
    gamma = 0.36,
    epsilon1 = epsilon1_value
  )
  
  tc <- data.frame(lsoda(init, times, seir, param_vals))
  return(tc)
}

tc_doctor <- run_model(epsilon_doctor)

tc_no_doctor <- run_model(epsilon_no_doctor)


plot(
  tc_doctor$time/365,
  tc_doctor$D,
  type = "l",
  col = "blue",
  lwd = 2,
  main = "Cumulative Deaths of Patients With and Without Doctors",
  xlab = "Time (years)",
  ylab = "Cumulative deaths",
  bty = "n",
  ylim = c(0, max(tc_no_doctor$D))
)


lines(
  tc_no_doctor$time/365,
  tc_no_doctor$D,
  col = "red",
  lwd = 2
)

legend(
  "bottomright",
  legend = c("With doctors (0.9%)", "Without doctors (5.1%)"),
  col = c("blue", "red"),
  lwd = 2,
  bty = "n"
)
