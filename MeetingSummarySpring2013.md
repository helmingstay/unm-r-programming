Summary of Spring 2013 Meetings<br>
<b>Click date to download script file</b>

<h1><a href='http://unm-r-programming.googlecode.com/files/R_group_8_Feb_2013.Rnw'>8 Feb 2013</a></h1>
<ul><li>A working LaTeX installation is required!  Get it here: <a href='http://www.latex-project.org/ftp.html'>http://www.latex-project.org/ftp.html</a>
</li><li>In Rstudio, go to the menu |Tools -> Options -> Sweave| and set "Weave Rnw files using:" to "knitr".<br>
</li><li><a href='http://unm-r-programming.googlecode.com/files/R_group_8_Feb_2013.pdf'>PDF of Rnw file</a>
</li><li>Discussion of color and plotting characters<br>
</li><li>Introduction to knitr, Rstudio<br>
</li><li>Reading in weather data, dealing with dates.</li></ul>

<h1>1 Feb 2013</h1>
<ul><li>No script file for this week<br>
</li><li>Josh found prcomp() for principle component analysis<br>
</li><li>Start looking at sewer data, dependence on air temp<br>
</li><li>Intro to ggplot<br>
</li><li>Discussion of ggplot and aes:<br>
<pre><code>## CORRECT<br>
## use a variable to control an aesthetic<br>
geom_point(aes(color=size))<br>
## If using a constant, don't use aes()<br>
geom_point(color='green')<br>
<br>
## WRONG<br>
## works but probably isn't what you want<br>
geom_point(aes(color='green'))<br>
## doesn't make sense<br>
geom_point(color=size)<br>
</code></pre></li></ul>

<h1><a href='http://unm-r-programming.googlecode.com/files/R_group_25_Jan_2013.R'>25 Jan 2013</a></h1>
<ul><li>Different plotting systems in R<br>
<ol><li>Base graphics: plot(x,y)<br>
</li><li>Lattice: xyplot(y~x, data)<br>
</li><li>ggplot2: ggplot(data, aes(x=x, y=y)) + geom_point()<br>
</li><li>Both lattice and ggplot are based on the grid package<br>
</li></ol></li><li>Intro to ggplot with bird mophology dataset<br>
</li><li>Tab-based autocompletion in Rstudio<br>
</li><li>Cool ggplot tricks:<br>
<ol><li>facet_wrap()<br>
</li><li>geom_smooth()</li></ol></li></ul>

<h1><a href='http://unm-r-programming.googlecode.com/files/R_group_18_Jan_2013.R'>18 Jan 2013</a></h1>
<ul><li>Introductions.<br>
</li><li>Mark Holstadt's dataset -- excel vs. R!<br>
</li><li>Intro to data types in R<br>
<ol><li>Vectors: atomic mode<br>
</li><li>Lists: arbitrary collection of objects<br>
</li><li>Data.frames: special lists, each list element has same length