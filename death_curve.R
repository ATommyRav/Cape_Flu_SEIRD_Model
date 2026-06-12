beta_calc <- function(
    R_0,  sigma = 365/2, gamma = 0.36, epsilon1 = 0.01935)
{ R_0 / ((sigma / ( sigma)) * (1 / ( gamma + epsilon1)))}
#we only have to define R_0 , as other param are set 

S.star <- function(
    beta, N, sigma = 365/2, gamma = 0.36, epsilon1 = 0.01935){
  N * (( sigma) / beta) * (( gamma + epsilon1 ) / sigma)
}
#only have to define beta and N rest are defined 

E.star<- function(
    beta, N,  sigma = 365/2, gamma = 0.36, epsilon1 = 0.01935){
  ( N - S.star(beta, N,  sigma, gamma))/ ( sigma)
}
#only have to define beta and N rest are defined 

I.star<- function(
    beta, N,  sigma = 365/2, gamma = 0.36, epsilon1 = 0.01935){
  (sigma / (gamma + epsilon1)) * E.star(beta, N, sigma, gamma)}
#only have to define beta and N rest are defined 

R.star <- function(
    beta, N,  sigma = 365/2, gamma = 0.36, epsilon1 = 0.01935){
  (gamma) * I.star(beta, N , sigma, gamma)}
#only have to define beta and N rest are defined 

N0 <- 6000000

beta_calc(15) #find beta 

S.star(beta_calc(15), N0)+ 
  E.star(beta_calc(15), N0)+
  I.star(beta_calc(15), N0)+
  R.star(beta_calc(15), N0)

E.star(beta_calc(15), N0) + I.star(beta_calc(15), N0)

#E.star(beta_calc(18), 6000000) + I.star(beta_calc(18), 6000000)

#E.star(beta_calc(13), 7200) + I.star(beta_calc(13), 7200)

#E.star(beta_calc(18), 7200) + I.star(beta_calc(18), 7200)


#visualize dynam 
#install.packages("deSolve")
library(deSolve)

seir <- function(t, y, params) {
  S <- y[1]
  E <- y[2]
  I <- y[3]
  R <- y[4]
  D <- y[5]
  
  #defining param 
  beta <- params["beta"]
  N <- params["N"]
  sigma <- params["sigma"]
  gamma <- params["gamma"]
  epsilon1 <- params["epsilon1"]
  
  #stating ODEs 
  dSdt <- - (beta * S * I / N) 
  dEdt <- (beta * S * I / N) - (sigma * E)
  dIdt <- (sigma * E) - (gamma * I) - (epsilon1 * I)
  dRdt <- (gamma * I) 
  dDdt <- ( epsilon1 * I) 
  return(list(c(dSdt, dEdt, dIdt, dRdt, dDdt)))
}

N0 <- 6000000
S0 <- 6000000-7-35
E0 <- 35
I0 <- 7
D0 <- 0
R0 <- 0

init <- c(sus = S0, exp = E0, inf = I0, rec = 0, D=0)


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

beta_calc(100)

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
