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
aa1 = 1:10
bb1 = 11:20
## create "empty container" to fill with loop-based multiplication
aa2 = numeric( length(aa1) )

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

## for outer product of aa1 and bb1
## create "empty container" to fill with loop-based multiplication
bb.out1 = matrix( numeric(), nrow=length(aa1), ncol=length(bb1) )
## for inner product of aa1 and bb1
bb.in1 = 0


## 
## inner product of aa1 and bb1 using loop
## print an error and stop if lengths are not equal
if ( length(aa1) != length(bb1) ) { stop("aa1 and bb1 must be of equal length for inner product") }
for ( index1 in 1:length(aa1) ) {
    #for ( index1 in 1:length(bb1) ) {
        bb.in1 = bb.in1 + aa1[index1] * bb1[index1]
    #}
}

## vector-based inner product
## %*% is classic "matrix multiplication"
## see ?"%*%" for more details
## note that non-alphabetical functions need quotes for ?-help to work
bb.in2 = aa1 %*% bb1

## test - are they equivalent?
cat('Are bb.in1 and bb.in2 indentical?', identical(bb.in1, bb.in2), ' \n\n')


## notice indents corresponding to loop-level - makes code more readable
## 
## outer product of aa1 and bb1
for ( index1 in 1:length(aa1) ) {
    for ( index2 in 1:length(bb1) ) {
        bb.out1[index1, index2] = aa1[index1] * bb1[index2]
    }
}

## vector-based outer product 
## t() transposes bb1 into a row vector
## no need to pre-define.  
bb.out2 = aa1 %*% t(bb1)

## test - are they equivalent?
cat('Are bb.out1 and bb.out2 indentical?', identical(bb.out1, bb.out2), '\n')

