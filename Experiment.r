# Flipping fair coin 100 x, recording HT String
# Use results to calculate the empirical probability of obtaining
#   a streak of at least 6 of something in a row.
# How does this probability [change] as you vary the number of
#   trials and length of streak

#Constants
NTRIALS <- 100 #number of trials
NSIMS <- 10000 #number of sims to run to calculate probability
STREAK <- 6 #minimum streak to register

# Function to generate n flips
flip_coin <- function(n) sample(c("H","T"), n, rep = T)

# Function to check for streaks of s in-a-row
check_streak <- function(s,sims,trials) {
  havestreak <- 0

  for (i in 1:sims){
    flipsim <- flip_coin(trials)
    lastf <- "N"
    count <- 0

    for (f in flipsim){
      if (f == lastf) {
        count <- count + 1
        if (count >= s){
          havestreak <- havestreak + 1
          break
        }
      } else {
        count <- 0
      }
      lastf <- f
    }
  }

  return(havestreak/sims) #returns probability
}


#print(check_streak(STREAK,NSIMS,NTRIALS))

# Compare different numbers of trials (streak constant at 6)
for (altt in c(100,200,500,1000)) {
 print(paste(altt,"trials:",check_streak(STREAK,NSIMS,altt)))
}

# compare different lengths of streak (tials constant at 100)
for (alts in c(1:15)) {
  print(paste("Streak of",alts,":",check_streak(alts,NSIMS,NTRIALS)))
}

plot(altgraph)

# Sample Oupu\t:
# [1] "100 trials: 0.5428"
# [1] "200 trials: 0.7943"
# [1] "500 trials: 0.9844"
# [1] "1000 trials: 0.9997"
# [1] "Streak of 1 : 1"
# [1] "Streak of 2 : 1"
# [1] "Streak of 3 : 0.9999"
# [1] "Streak of 4 : 0.973"
# [1] "Streak of 5 : 0.8012"
# [1] "Streak of 6 : 0.5402"
# [1] "Streak of 7 : 0.3118"
# [1] "Streak of 8 : 0.1684"
# [1] "Streak of 9 : 0.0825"
# [1] "Streak of 10 : 0.0422"
# [1] "Streak of 11 : 0.022"
# [1] "Streak of 12 : 0.0105"
# [1] "Streak of 13 : 0.0048"
# [1] "Streak of 14 : 0.0028"
# [1] "Streak of 15 : 8e-04"

# These results show that increasing the number of trials causes the
# of a set streak of numbers to increase, eventually approaching 1
# asymptotically. When trials are kept constant but the minimum requirement
# for a streak is increased, the probability descreases, approaching 0
# asymptotically.
