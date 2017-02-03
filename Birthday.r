# Solves the birthday problem using simulation

number_sims <- 10000 #How many simulations to use as
max_group_size <- 50 #Graphing probability for groups of 1 to this number


genbirthday <- function(n) sample(1:365,n,rep=T)
# Function to generate any number of birthdays, returned as a vector of numbers
# between 1 and 365.

bdayprob <- function(groupsize,nsims)  {
# Function that takes the size of the group and the number of simulations to
# run to calculate the probability of that group having two or more members
# with the same birthday.
  nrepeats <- 0 # Number of samples with repeated birthdays
  for (i in 1:nsims) {
    bdaycounter <- rep(0,365) #fill the vector full of zeroes so the if statement below will work
    for (bday in genbirthday(groupsize)) {
      if (bdaycounter[bday] > 0) {
        nrepeats <- nrepeats + 1
        break
      }
      else {
        bdaycounter[bday] <- 1
      }
    }
  }
  return(nrepeats/nsims) #Divide by nsims to make it the simulated probability
}

bygroupsize <- function(maxsize,nsims) {
# Function that takes as argument the maximum group size you want to go until,
# and calculates the probability of at least two common birthdays for a group
# of one person up until the max, saving each to a vector and returning the
# vector.
  bdayprobs <- vector()
  for (i in 1:maxsize){
    bdayprobs[i] <- bdayprob(i,nsims)
  }
  return(bdayprobs)
}


plot(bygroupsize(max_group_size,number_sims),type="l",xlab="Number of People in the Room",ylab="Probability at Least Two Share a Birthday")
