#Roger H Hayden III
#11/08/2021
#Statistical Methods and Data Analysis
#Module 10 - Randomization Test

# The conduct of a randomization or permutation test for the equality of two 
# population means is as follows. Under the null hypothesis, there is no 
# difference in the populations. Thus, the assignment of values to one population
# or the other is regarded as one arbitrary permutation.The question is how 
# unusual is the test statistic under this permutation in relation to other
# permutations. For problem 9.2.20, consider the fixed and ARM rates. As the 
# data appear,compute the difference in mean rates between them. Then combined 
# the values from ARM and fixed into one vector. Sample without replacement from 
# that vector a set of 10 rates. The “sample” function will help. The rates 
# chosen represent one rate plan; the rates not chosen represent the other. For 
# each of these permutations, take the difference of means between the rates 
# chosen and the rates not chosen. Keep track for 1000 permutations of the test 
# statistic.  Use R to determine how many of the test statistics resulting from 
# permuted rates are more unusual than the initial difference between rates from 
# the way the data are given in the problem. The proportion of the mean 
# difference in rates more unusual (extreme) than the initial difference is 
# interpretable as a p-value. Report your p-value from the randomization test.


fixed=c(3.525, 3.625, 3.383, 3.625, 3.661, 3.791, 3.941, 3.781, 3.660, 3.733)
arm=c(2.923, 3.385, 3.154, 3.363, 3.226, 3.283, 3.427,  3.437, 3.746, 3.438)
combined=c(fixed, arm)
meandif<- mean(fixed)-mean(arm)
diff<-NULL

for (i in 1:1000){
  s1 <- sample(combined, 10, replace = F)
  s2 <- combined[!(combined %in% s1)]
  mean_s<-mean(s1)-mean(s2)
  diff<- c(diff, mean_s)
}


pvalue<-sum(diff >= meandif)/1000
pvalue

  