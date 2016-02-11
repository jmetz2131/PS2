# RClass
####Homework #2
###Question 1
##Setting up the test data
vote.results <- seq(1,10000, by = 1) # Here, I used a sequence of 0 to 10000 to simulate the amount of votes a candidate would receive in a single district.
candidate.a <- sample(vote.results, 20, replace=T)
candidate.b <- sample(vote.results, 20, replace=T)
candidate.c <- sample(vote.results, 20, replace=T)

#For the three lines of code above, I made the vectors candidate.a, candidate.b, and candidate.c to represent how many votes three candidates would receive in a hypothetical election. 
#I then took a sample of vote.results 20 times to represent the vote count totals for 20 different counties to truly test if Benford's Law works out for a random sample.

##Leemis' m statistic and Cho-Gains' d
benfords_law = function(results, m=TRUE, d=TRUE){
  first_digit <- substr(results, start=1, stop=1) # This takes the matrix or vector and uses the substr() function to take just the first digit to see the distribution of digits across the vote totals to see if this example satisfies Benford's Law
  x <- as.numeric(first_digit) #This changes the first digits to the numeric class from characters to make them workable for the rest of the function
  proportions<-table(x)/sum(table(x)) #This calculates the proportion of each first digit that occurs within the data from results. So if 5 numbers contained the first digit "3" out of 20 numbers all together, the proportion would be 5/20, or 0.25.
   
    for(i in 1:9){
    m <- max(proportions-log(1+1/i, base=10))
    d <- sqrt(sum(proportions-log(1+1/i, base=10))^2)
    }
    #These lines of code show the formulas for the Leemis' m statistic (assigned to the variable m) and the Cho-Gains' d (assigned to the variable d)
    #I used the proportions calculated from several lines above to take the place of "Xi" in the formulas and coded i to be a sequence from 1:9 to see the prevalence of each first digit in the results data.
    
    if(m == TRUE & d == TRUE) return(list("m"= m, "d"= d))
    if(m == TRUE & d == FALSE) return(list("m"= m))
    if(m == FALSE & d == TRUE) return(list("d"= d))
    if(m == FALSE & d == FALSE) return("Both tests are false.")
    #These conditional statements run based on whether m and d are met based on the calculations above and will return the full digit distribution if one of them is met or will return "Both tests are false" if the formulas aren't met.
}

##When the input is a matrix
county_results <- cbind(candidate.a, candidate.b, candidate.c) #The matrix county_results includes the vote count totals for candidate.a, candidate.b, and candidate.c across twenty different hypothetical counties.
benfords_law(county_results,m=T,d=T)
#This places the matrix county_results in my benfords_law function as the test data.

##When the input is a vector
test_vector<- sample(1:10000,60,replace=T)
benfords_law(test_vector,m=T,d=T)
#This places the vector test_vector in my benfords_law function as the test data.

###Question 2
print.benfords = function(x){
  calculations <- benfords_law(results) #This is to ensure the values found from the previous function carry over.
 
  leemis <- round(estimates$m,3)
  cho_gains <- round(estimates$d,3)
  #These two lines of code take the calculated statistics from the first function and truncate the results to three significant digits to simplify whether they meet the significance levels as defined by the critical values.
  
  m_critical_values <- c(0.851,0.967,1.212) #These values show where the level where the null hypothesis can be rejected for the m statistic at the .10, .05, and .01 levels, respectively.
  d_critical_values <- c(1.212,1.330,1.569) #These values show where the level where the null hypothesis can be rejected for the d statistic at the .10, .05, and .01 levels, respectively.
  
  leemis_pass <- sum(m>=m_critical_values)
  cho_gains_pass <- sum(d>=d_critical_values)
  #These two lines of code see whether the calculated values from the other function meet the critical values
  
  significance_level <- c("*", "**","***") # To depict the significance levels, the asterisks correspond to the .10, .05, and .01 levels, respectively.
 
  output_table <- data.frame(ResultingStatistic=c(m,d), Critical_Value=c(significance_level[leemis_pass],significance_level[cho_gains_pass]))
  rownames(output_table)<-c("Leemis' m statistic","Cho-Gains' d statistic") ##This designates the row names for the output_table
  print(c("* = The value is significant at the 0.10 level", "** = The value is significant at the 0.05 level", "*** = The value is significant at the 0.01 level")) #This will serve as the legend after the table is returned
}

##Using the sink function and creating a csv
getwd()
setwd("/Users/jacobmetz")
#These commands allowed me to find and set my working directory so I could create a csv

CSV_function = function(x){
  ?sink
  sink(file = "BenfordsTable.csv")
  print.table(print.benfords(x))
  sink()
}
CSV_function(x)
#Here I tried to follow the help file on the sink function as closely as possible. 
#To my understanding, the sink function took the table from the print.benfords function and "diverted" the output to the csv file.


  
  