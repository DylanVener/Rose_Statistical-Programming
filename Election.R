#Because of the use of only default functions, this code should work universally
#Setting the seed to ensure consistent results
set.seed(19120823)
#Number of signed votes in favor
s.pro <- 3208
#Number of signed votes against
s.con <- 3143
#Number of unsigned votes for
u.pro <- 730
#Number of unsigned votes against
u.con <- 942

#Total Probability of voting in favor of the tax, disregarding signing
t.prob <- (s.pro+u.pro)/(s.pro+s.con+u.pro+u.con)

#Simulation of 1672 peolple not signing their ballot, run 10,000,000 times, with t.prob of voting in favor
u.sim <- rbinom(10000000,u.pro+u.con,t.prob)

#Percent of times in the simulation that a result as extreme or more was observed
(sum(u.sim<=u.pro)/10000000)*100
