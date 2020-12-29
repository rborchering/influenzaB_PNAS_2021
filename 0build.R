library(wrapr)
library(knitr)
## tinytex requires local install (fast, once per machine)
## see https://yihui.org/tinytex/ 
library(tinytex)
## %.>%
library(wrapr)
## convenience function: build path
mk.path <- function(x, dir='output/') paste0(dir,x)
## knit: Rnw to tex (returns file path)
## pdflatex: tex to pdf
(   knit('figs.Rnw', output=mk.path('figs.tex'))
    %.>% tinytex::pdflatex(
        file=., pdf_file=mk.path('figs.pdf')
    )
)
##

(   knit('supplemental.Rnw', output=mk.path('si.tex'))
    %.>% tinytex::pdflatex(
        ## need aux file
        file=., clean=F, pdf_file=mk.path('si.pdf')
    )
)

## extract labels for use in manuscipt .tex file
## paste above \begin{document}
#In bash:
## \grep '\\newlabel{' si.aux > output/si-labels.tex
