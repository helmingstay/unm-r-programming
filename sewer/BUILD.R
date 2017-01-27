library(rmarkdown)
library(knitr)
library(bookdown)

#.build <- c('pdf_document')
#.build <- c('pdf_document', 'word_document')
#render('writeup.Rmd', output_format=.build)
#render('supp.Rmd', output_format=.build)
render_book(input='writeup.Rmd',
    #output_format="bookdown::pdf_book",
    new_session=F
 )
