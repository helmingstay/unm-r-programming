## challenge: 
## loops, string comparison, 
## setting random number generator state
## 
## output from pss('geb.R')
# godel: 6988636 wins!
# escher: 15419089 wins!
# bach: 40242 wins!
# user  system elapsed
# 632.114   0.626 633.637
test.strings <- c('godel', 'escher', 'bach')
for (.string in test.strings) {
    ii <- 0
    while(T) {
        set.seed(ii)
        proposal <- sample(letters, nchar(.string), replace=T) 
        proposal <- paste(proposal, collapse='')
        if ( proposal == .string ) {
            cat(sprintf('%s: %d wins!\n', .string, ii))
            break
        }
        ii <- ii+1
    }
}
