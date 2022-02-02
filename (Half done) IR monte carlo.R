require("quantmod");require("lubridate")

IR <- ts(FEDFUNDS$IR, start = c(2000,1,1), frequency = 12)
mean(IR)
sd(IR)

IRfor <- function(IR, N, MEAN, STDEV){
  delta_t = 1/N #1 period
  for(i in seq(N)){
    epsilon <- runif(n=1,min=0,max=1) #arbitrary probabilities
    IR <- IR * (1+qnorm(epsilon, MEAN*delta_t, STDEV*sqrt(delta_t)))
  }
  IR
}

last(IR)
simulations <- 1000
N = 20
#IR <- as.numeric(coredata(IR[Sys.Date()-days(20)]))
MEAN = mean(IR)
STDEV = sd(IR)

simIR <- c()
for(i in seq(simulations)){
  IR <- c(IR,IRfor(IR=IR, N=N, MEAN=MEAN, STDEV=STDEV))
}

quantile(IR)


