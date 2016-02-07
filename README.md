# RClass
###Question 1
##Leemis' m statistic and Cho-Gains' d
benford = function(...){
  m <- max(i-log(1+1/i, base=10))
  d <- sqrt((i-log(1+1/i, base=10))^2)
}