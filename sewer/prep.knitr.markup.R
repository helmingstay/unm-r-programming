## http://gforge.se/2014/01/fast-track-publishing-using-knitr-part-iii/
# Notify that you want to use the counter,
# if you set the counter to 3 then it will use 
# that as starting number. You can also use strings
# if you for instance have a split figure with 
# a "1a" and "1b" setup
options(figure_counter = TRUE)
 
# If you want roman letters then set: 
# options(figure_counter_roman = TRUE)
 
# Evaluate the figure caption after the chunk, 
# sometimes you want to calculate stuff inside the
# chunk that you want to include in the caption and
# it is therefore useful to evaluate it afterwards.
opts_knit$set(eval.after='fig.cap')
 
# The actual hook
knit_hooks$set(plot = function(x, options) {
  fig_fn = paste0(opts_knit$get("base.url"), 
                  paste(x, collapse = "."))
 
  # Some stuff from the default definition
  fig.cap <- knitr:::.img.cap(options)
 
  # Style and additional options that should be included in the img tag
  style=c("display: block",
          sprintf("margin: %s;",
                   switch(options$fig.align, 
                          left = 'auto auto auto 0', 
                          center = 'auto',
                          right = 'auto 0 auto auto')))
  # Certain arguments may not belong in style, 
  # for instance the width and height are usually
  # outside if the do not have a unit specified
  addon_args = ""
 
  # This is perhaps a little overly complicated prepared 
  # with the loop but it allows for a more out.parameters if necessary
  if (any(grepl("^out.(height|width)", names(options)))){
      on <- names(options)[grep("^out.(height|width)", names(options))]
      for(out_name in on){
          dimName <- substr(out_name, 5, nchar(out_name))
          if (grepl("[0-9]+(em|px|%|pt|pc|in|cm|mm)", out_name))
              style=append(style, paste0(dimName, ": ", options[[out_name]]))
          else if (length(options$out.width) > 0)
              addon_args = paste0(addon_args, dimName, "='", options[[out_name]], "'")
      }
  }
 
  # Add counter if wanted
  fig_number_txt <- ""
  cntr <- getOption("figure_counter", FALSE)
  if (cntr != FALSE){
    if (is.logical(cntr))
      cntr <- 1
    # The figure_counter_str allows for custom 
    # figure text, you may for instance want it in
    # bold: <b>Figure %s:</b>
    # The %s is so that you have the option of setting the
    # counter manually to 1a, 1b, etc if needed
    fig_number_txt <- 
      sprintf(getOption("figure_counter_str", "Figure %s: "), 
              ifelse(getOption("figure_counter_roman", FALSE), 
                     as.character(as.roman(cntr)), as.character(cntr)))
 
    if (is.numeric(cntr))
      options(figure_counter = cntr + 1)
  }
 
  # Put it all together
  paste0("<figure><img src='", fig_fn, "'", 
         " ", addon_args,
         paste0(" style='", paste(style, collapse="; "), "'"),
         ">",
         "<figcaption>", fig_number_txt, fig.cap, "</figcaption></figure>")
})
