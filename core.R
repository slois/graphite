# Title     : TODO
# Objective : TODO
# Created by: slois
# Created on: 25/9/20

library(ggplot2)
library(gridExtra)
library(patchwork)

myPlot <- function(df, x.d='x', y.d='y', grp.d='grp'){
  theme0 <- function(...) theme(legend.position = "none",
                                axis.ticks = element_blank(),
                                axis.text.x = element_blank(),
                                axis.text.y = element_blank(),
                                axis.title.x = element_blank(),
                                axis.title.y = element_blank(),
                                ...)

  base <- ggplot(data=df)

  main <- base + geom_point(aes_string(x=x.d, y=y.d, colour=grp.d)) +
    geom_rug(aes_string(x=x.d, y=y.d, colour=grp.d), length = grid::unit(2,units = "mm"), alpha=0.4, outside = F) +
    labs(x=x.d, y=y.d) +
    theme_bw() +
    theme()

  axis_x <- base +
    geom_density(aes_string(x=x.d, fill=grp.d, colour=grp.d), alpha=0.2) +
    theme_bw() +
    theme0()

  axis_y <- base +
    geom_density(aes_string(x=y.d, fill=grp.d, colour=grp.d), alpha=0.2) +
    coord_flip() +
    theme_bw() +
    theme0()

  return(axis_x + guide_area() + main + axis_y + plot_layout(ncol=2, nrow=2, widths=c(4,1), heights=c(1,4), guides="collect"))
}

coord_radar <- function (theta = "x", start = 0, direction = 1)
{
    theta <- match.arg(theta, c("x", "y"))
    r <- ifelse(theta == "x", "y", "x")
    ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
        direction = sign(direction),
        is_linear = function(coord) TRUE)
}


spider_plot <- function(x, identifier, title='', facets=FALSE){
  data <- x %>%
  melt(id=identifier) %>%
  dplyr::group_by(variable) %>%
  dplyr::mutate(norm_value=value/max(value)) %>%
  ungroup()

  p <- ggplot(data=data,
       aes_string(x='variable',
                  y='norm_value',
                  group=identifier,
                  fill=identifier,
                  color=identifier)) +
  geom_point(size=2, shape=19) +
  geom_polygon(size=1, alpha=0.2) +
  coord_radar() +
  theme(legend.position="top") +
    theme_bw() + #facet_wrap(~Model)
    labs(x='', y='', title=title, caption='Source: Graphite plots')

  if (facets){
    fmla <- as.formula(paste('~', identifier, sep=''))
    p <- p + facet_wrap(fmla)
  }

  return (p)
}
## Spider plot example
mtcars_adapted <- mtcars[1:3,] %>% dplyr::select(mpg:qsec) %>% add_rownames("Model")
spider_plot(mtcars_adapted,
            identifier="Model",
            title='Example of spider plot: comparison of car models',
            facets=F)

## Margin plot example
x <- c(rnbinom(750, 10, prob=0.1), rnbinom(375, 10, prob=0.05), rnbinom(375, 10, prob=0.15))
y <- c(rnorm(750, 1, 0.5), rnorm(750, 0, 0.1))
grp <- c(rep('VarA', 375), rep('VarB', 375), rep('VarC', 375), rep('VarD', 375))

df <- data.frame(ScoreA=x, ScoreB=y, Group=grp)
myPlot(df, x.d='ScoreA', y.d='ScoreB', grp.d='Group')

