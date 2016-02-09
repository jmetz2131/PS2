# RClass
####Homework #2
###Question 1
##Leemis' m statistic and Cho-Gains' d
vote.results <- seq(0,10000, by = 1) # Here, I used a sequence of 0 to 10000 to simulate the amount of votes a candidate would receive in a single district.

candidate.a <- sample(vote.results, 20, replace=T)
candidate.b <- sample(vote.results, 20, replace=T)
candidate.c <- sample(vote.results, 20, replace=T)
#For the three lines of code above, I made the vectors candidate.a, candidate.b, and candidate.c to represent how many votes three candidates would receive in a hypothetical election. 
#I then took a sample of vote.results 20 times to represent the vote count totals for 20 different counties to truly test if Benford's Law works out for a random sample.

county_results <- cbind(candidate.a, candidate.b, candidate.c)
county_results
#The matrix county_results includes the vote count totals for candidate.a, candidate.b, and candidate.c across twenty different hypothetical counties.

first_digit <- substr(county_results, start=1, stop=1)
x <- as.numeric(first_digit)
#I then took the county_results matrix and used the substr() function to take just the first digit
#to see the distribution of digits across the vote totals to see if this example satisfies Benford's Law

proportion_matrix = function(x){
  first_digit[first_digit==x]
}
function(x)


benford = function(x){
  i <- 1:9
  m <- max((x*100/sum(x))-log(1+1/i, base=10))
  d <- sqrt(sum((x*100/sum(x))-log(1+1/i, base=10))^2)
}
benford(x)

###Question 2
print.benford = function()