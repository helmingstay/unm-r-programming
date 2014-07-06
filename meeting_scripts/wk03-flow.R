######################
### Copyright (C) 14 Sept 2010 Christian Gunning
### 
### This is software libre: you can redistribute it and/or modify it
### under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 2 of the License, or
### (at your option) any later version.
### See http://www.gnu.org/licenses/ for more details

######################
### this is an excercise highlighting what's known as 
### "flow control", including for-loops and if-statements
### it compares explicit loops with vectorized functions
######################################
##
aa1 = 1:10
## create "empty container" to fill with loop-based multiplication
aa2 = rep(0, length=length(aa1) )

## use print statements to give feedback as source is run
print("## starting loop over aa1")
##
##  multiplication of aa1 using a loop
## for every element in aa1, do this
for ( index1 in 1:length(aa1) ) {
    ## cat is like print, but with more control
    ## objects and strings can be intermixed
    ## "\n" is the "enter" at the end of the line
    cat("#### index1 is set to ", index1, "\n")
    ## aa2 must already exist and be the proper size
    aa2[index1] = aa1[index1] *2
}

cat("## finished loop over aa1 \n\n")

## vectorized multiplication of aa1
## unlike the loop, we didn't have to "pre-define" aa3
aa3 = aa1 * 2
## test - are they equivalent?
cat('Are aa2 and aa3 indentical?', identical(aa2, aa3), ' \n\n')

####################
## Below is a example of a simple ecological model (discrete-time logistic equation)
## see http://en.wikipedia.org/wiki/Logistic_map for more details
## using a for-loop to index time
## try testing different values of pop0 and growth.rate
###################

## variables go at the top
##
growth.rate = 2.9   ## x100 = % growth rate per timestep
pop0 = 0.75          ## pop here is normalized by max pop (carrying capacity)
timesteps = 100
###
### the rest is code that uses the above variables
###
pop.t = rep(0, length=timesteps)    ## pre-create the result vector
pop.t[1] = pop0                     ## set the first timestep = pop0

for (timestep in 2:timesteps) {
    ## notice we start at timestep 2, since we need to access previous step
    lastpop = pop.t[ timestep - 1]
    pop.t[timestep] = growth.rate * lastpop * ( 1-lastpop )
    ## equivalent to "x_(n+1) = r * x_n * ( 1 - x_n)"
}
    
plot(pop.t, type='l')
