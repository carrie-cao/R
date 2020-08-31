# knight move problem, simulation
# Random number between 1 and 2, If x is 2, y is 1, and vice versa. Sample(c(1, -1)) can get either 1 or -1

#simulate 10 moves, 1000000 times
library( tidyr)
library("dplyr")

#Given the knight started at (0, 0), calculate the position after moves by cumulative sum of moves.
set.seed(123)
results_10Steps <- crossing(trial = 1:1000000,
                            turn = 1:10) %>%
  mutate(x_steps = sample(2, n(), replace = TRUE) *
           sample(c(1, -1), n(), replace = TRUE),
         y_steps = (3 - abs(x_steps)) *
           sample(c(1, -1), n(), replace = TRUE)) %>%
  group_by(trial) %>%
  mutate(x_localization = cumsum(x_steps),
         y_localization = cumsum(y_steps)) %>%
  ungroup()

#select the first plane, and the final position (10th moves)
head(results_10Steps)
tibble10s=results_10Steps[(results_10Steps$x_localization>=0) & (results_10Steps$y_localization>=0),]
#tibble10s=tibble10s[tibble10s$turn==10,]
tibble10s$distance=sqrt(tibble10s$x_localization^2+tibble10s$y_localization^2)
# calculate the mean and stand deviation
mean(tibble10s$distance)
sqrt(var(tibble10s$distance))

##select the  final position (10th moves)
tibble10sF=tibble10s[tibble10s$turn==10,]
mean(tibble10sF$distance)
sqrt(var(tibble10sF$distance))


# 100 moves
results_100Steps <- crossing(trial = 1:1000000,
                             turn = 1:100) %>%
  mutate(x_steps = sample(2, n(), replace = TRUE) *
           sample(c(1, -1), n(), replace = TRUE),
         y_steps = (3 - abs(x_steps)) *
           sample(c(1, -1), n(), replace = TRUE)) %>%
  group_by(trial) %>%
  mutate(x_localization = cumsum(x_steps),
         y_localization = cumsum(y_steps)) %>%
  ungroup()

#select the first plane
head(results_100Steps)
tibble100s=results_100Steps[(results_100Steps$x_localization>=0) & (results_100Steps$y_localization>=0),]

tibble100s$distance=sqrt(tibble100s$x_localization^2+tibble100s$y_localization^2)
# calculate the mean and stand deviation
mean(tibble100s$distance)
sqrt(var(tibble100s$distance))
