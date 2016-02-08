# RClass
####Homework #2
###Question 1
##Leemis' m statistic and Cho-Gains' d
vote.results <- seq(0,1, by = .01)
vote.results

election.vector <- sample(vote.results, 9)
election.vector
election.matrix

benford = function(...){
  i <- 1:9
  m <- max(x-log(1+1/i, base=10))
  d <- sqrt((x-log(1+1/i, base=10))^2)
}

###Question 2
print.benford = function()