## specify base here
.filebase = 'writeup'
.filemaker <- function(ext, base=.filebase) {
   sprintf('%s.%s', base, ext) 
}

require(knitr)
## prep html writer options
.htmlOptions <- markdownHTMLOptions(defaults=TRUE)
## remove inline images from options vector
.htmlOptions <- .htmlOptions[!grepl('base64_images', .htmlOptions)]

## knit
knit(.filemaker('Rmd'))
##custom stylesheet as per http://gforge.se/2014/01/fast-track-publishing-using-knitr-part-ii/
md_txt <- markdownToHTML(.filemaker('md'),
    stylesheet='custom.css', 
    options=.htmlOptions
)

writeLines(con=.filemaker('html'),
    gsub("<h([0-9]+)>", 
         "<h\\1 style='margin: 10pt 0pt 0pt 0pt;'>", 
         gsub("<h1>",
              "<h1 style='margin: 24pt 0pt 0pt 0pt;'>",
              md_txt)) 
)

