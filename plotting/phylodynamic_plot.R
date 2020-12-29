# Function to get ggplot2 colors 
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

.xlim.range <- range(c(ha.df$Time, na.df$Time))
ha.gmrf <- (
    ggplot(ha.df)
    + aes(x = Time, y = Median, ymin = Lower, ymax = Upper)
    + geom_line(color = "dodgerblue4")
    + geom_ribbon(fill = "dodgerblue4", alpha = 0.4)
    #+ labs(x = "", y = expression('N'['e']))
    + labs(x = "", y = 'Genetic diversity')
    + scale_x_continuous(limits = .xlim.range, expand = c(0,0.001,0, 0))
    + scale_y_log10(limits = c(1, 2e2))
    + gg.theme
    + theme(legend.position = 'none')
)

## as above, but data for na
na.gmrf <- (ha.gmrf %+% na.df)

## shared scales
.scales.phylo <- list(
    coord_cartesian(xlim=.xlim.range, clip = 'off'),
    scale_x_continuous(expand=c(0.1,0,0,0)),
    ## see expand_scale
    scale_y_continuous(expand=c(0.02,0,0.01,0)) 
)

## horiz distance between vertical bar and cladelabel
.offset.text <- -19.95

ha.phylo <- (
  ggtree(ha.tree, size=0.3, mrsd = "2020-04-09") +
    geom_tippoint(aes(color = group), size = 0.5) +
    scale_color_manual(values = c("dodgerblue4", gg_color_hue(4)[2:4])) +
    ## see setup.R for shared theme
    theme_tree2() +
    gg.theme.phylo +
    .scales.phylo +
    geom_cladelabel(node = 622, 
                    label = "V1A.3", 
                    angle = 90, 
                    barsize = 1, 
                    align = TRUE,
                    hjust = 0.5,
                    fontsize = gg.tree.tipsize, 
                    offset = -40.2, 
                    offset.text = .offset.text,
                    color = gg_color_hue(4)[4]) +
    geom_cladelabel(node = 1053, 
                    label = "V1A.1", 
                    angle = 90, 
                    barsize = 1, 
                    align = TRUE,
                    hjust = 0.5,
                    fontsize = gg.tree.tipsize, 
                    offset = -40.2, 
                    offset.text = .offset.text,
                    color = gg_color_hue(4)[2]) +
    geom_cladelabel(node = grep("V1A.2", ha.tree$tip.label), 
                    label = "V1A.2", 
                    angle = 0, 
                    barsize = 1, 
                    align = TRUE, 
                    fontsize = gg.tree.tipsize,
                    hjust = 0.5,
                    offset = -40.5, 
                    offset.text = .offset.text,
                    color = gg_color_hue(4)[3]) +
    geom_cladelabel(node = 609, 
                    label = "V1A", 
                    angle = 90, 
                    barsize = 1, 
                    align = TRUE, 
                    fontsize = gg.tree.tipsize,
                    hjust = 0.5,
                    offset = -39.5, 
                    offset.text = .offset.text,
                    color = "dodgerblue4")
)

ha.phylo <- (ha.phylo %>% rotate(609) %>% rotate(1205))


na.phylo <- (
  ggtree(na.tree, size=0.3, mrsd = "2020-04-09") +
    geom_tippoint(aes(color = group), size = 0.5) +
    scale_color_manual(values = c("dodgerblue4", gg_color_hue(4)[2:4])) +
    theme_tree2() +
    gg.theme.phylo +
    .scales.phylo +
    geom_cladelabel(node = 982,
                    label = "V1A.3",
                    angle = 90,
                    barsize = 1,
                    align = TRUE,
                    hjust = 0.5,
                    fontsize = gg.tree.tipsize,
                    offset = -40.2,
                    offset.text = .offset.text,
                    color = gg_color_hue(4)[4]) +
    geom_tiplab(aes(subset = node == grep("V1A.3", na.tree$tip.label)[1]),
                color = gg_color_hue(4)[4],
                label = "V1A.3",
                size = gg.tree.tipsize, 
                hjust = -0.3,
                parse = T) +
    geom_tiplab(aes(subset = node == grep("V1A.1", na.tree$tip.label)[30]),
                color = gg_color_hue(4)[2],
                label = "V1A.1",
                size = gg.tree.tipsize, 
                hjust = -2.2,
                parse = T) +
    geom_tiplab(aes(subset = node == grep("V1A.1", na.tree$tip.label)[158]),
                color = gg_color_hue(4)[2],
                label = "V1A.1",
                size = gg.tree.tipsize,
                hjust = -1.1,
                parse = T) +
    geom_tiplab(aes(subset = node == grep("V1A.2", na.tree$tip.label) + 7),
                color = gg_color_hue(4)[3],
                label = "V1A.2",
                size = gg.tree.tipsize,
                hjust = -3.1,
                parse = T) +
    geom_cladelabel(node = 609,
                    label = "V1A",
                    angle = 90,
                    barsize = 1,
                    align = TRUE,
                    fontsize = gg.tree.tipsize,
                    hjust = 0.5,
                    offset = -39.45,
                    offset.text = .offset.text,
                    color = "dodgerblue4")
)

na.phylo <- (na.phylo %>% rotate(610) %>% rotate(974))
