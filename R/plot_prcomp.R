#' Extract label from ggplot
#' http://stackoverflow.com/a/13650878/1560062
#'
#' @param p ggplot
#' @return legend
#'
extract_legend <- function(p){
    tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    tmp$grobs[[leg]]
}

#' Plot first three principal components
#'
#' @param prcomp prcomp
#' @param group vector of the group/class labels
#' @param group_name this will be used for the legend
#' @param main plot title
#' @return ggplot
#'
plot_prcomp <- function(prcomp, group, group_name='group', main='') {
    stopifnot(identical(class(prcomp), 'prcomp'))
    
    x <- as.data.frame(prcomp$x)
    x[group_name] <- group
    
    add_options <- function(p) {
        p <- p + ggplot2::geom_point() + ggplot2::theme_bw()
        p + ggplot2::scale_colour_brewer(type='qual', palette='Set2')
    }
    
    group <- as.symbol(group_name)
    
    pc_3_1 <- add_options(ggplot2::ggplot(x, ggplot2::aes(x=PC3, y=PC1, col=group, shape=group)))
    pc_2_1 <- add_options(ggplot2::ggplot(x, ggplot2::aes(x=PC2, y=PC1, col=group, shape=group)))
    pc_2_3 <- add_options(ggplot2::ggplot(x, ggplot2::aes(x=PC2, y=PC3, col=group, shape=group)))
    
    gridExtra::grid.arrange(
        pc_3_1 + ggplot2::theme(legend.position = "none"),
        pc_2_1 + ggplot2::theme(legend.position = "none"),
        extract_legend(pc_3_1),
        pc_2_3 + ggplot2::theme(legend.position = "none"),
        ncol = 2,
        main = main
    )
}


