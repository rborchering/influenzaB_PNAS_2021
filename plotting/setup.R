## Color palettes, themes, margins, labels, etc.

## PNAS specs
## For details see https://blog.pnas.org/digitalart.pdf
## max height = 9in / 22.5cm
## knitr expects inches by default
## scale up by *1.56 here, down by 0.64 in final tex
#figwidth <- c(8.7, 11.4, 17.8)/2.54
figwidth <- c(13.6, 17.8, 27.7)/2.54
## Intended target dims
outwidth <- c('8.7cm', '11.4cm', '17.8cm')

## cowplot::plot_grid(label_size=)
lab.size = 14
## ggplot: geom_text size
lab.size.small=4
## for grid.text, match cowplot::plot_grid labels
gp.label <- gpar(fonsize=lab.size, fontface='bold')

## use consistent theme throughout
gg.font.base <-12
gg.theme <- (
    theme_bw(base_size=gg.font.base)
    + theme(
        legend.background=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank()
    )
    # + ...
)


## see plotting/phylodynamic_plot
gg.tree.tipsize <- lab.size.small
## 
gg.theme.phylo <- gg.theme + theme(
    legend.position = "None", 
    axis.line.x = element_line(size = 0.3), 
    axis.text.x.bottom = element_text(size = 10),
    panel.border = element_blank(),
    panel.grid.major.x=element_line(color="grey80", size=.3),
    panel.grid.major.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
)

gg.scale.noexpand <- list(
  scale_x_continuous(expand=c(0,0)),
  scale_y_continuous(expand=c(0,0)) 
)
## template x-axis setup
## breaks at every year
gg.scale.date <- function(lim, ...) {
    scale_x_date(limits=lim, expand=c(0,0), date_labels='%Y', date_breaks='1 year', ...)
}

gg.leg.title.blank <- theme(legend.title=element_blank())
gg.xrot <- theme(axis.text.x = element_text(angle=45, vjust=0.8, hjust=0.8))
gg.leg.outline <- theme(legend.background = element_rect(
    colour='grey40', fill='transparent', linetype='solid'
))
gg.leg.none <- theme(legend.position='none')

##  commonly-used labels
gg.lab.time = 'Week of\nseason'
## color palettes and keys
gg.col.type = c('red','blue')
gg.col.season = viridis(length(2010:2019), option='C', direction=-1, begin=0.1, end=0.9)
## exclude yellow
gg.col.state = brewer.pal(7, 'Set1')[-6]
gg.col.na <- 'grey80'

gg.colorbar <- guide_colourbar(
  ticks.colour = "black",
  frame.colour = "black",
)

## include both color & fill together
gg.scale.week = list(
    scale_color_viridis_c(gg.lab.time, direction=-1, end=0.9, option='D', guide = gg.colorbar),
    scale_fill_viridis_c(gg.lab.time, direction=-1, end=0.9, option='D', guide = gg.colorbar)
)
gg.panel.pack = theme(panel.spacing=unit(0.02,'lines'))



## helper functions
gg.leg.pos <- function(x) theme(legend.position=x)
## provide defaults
gg.plot.mar <- function(t=1, r=1, b=1, l=1) theme(plot.margin=margin(t,r,b,l, unit='lines'))

## labels
cap.percap.a <- 'Positive samples A (per 100,000)'
cap.percap.b <- 'Positive samples B (per 100,000)'

