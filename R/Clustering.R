utils::globalVariables(
  c('xend','variable', 
    'Cl', 'xend', 'yend', 'variable', 'variable_num',
    'h', 'position', 'value', 'idx')) 

#' mosaic tools for clustering
#' 
#' @rdname clustering
#' @param model a model
#' @param data a data-like object
#' @param which which kind of fortification to compute
#' @param ... additional arguments passed on to \code{link{dendro_data}}
#' @export

fortify.hclust <- function(model, data, 
                           which=c("segments", "heatmap", "leaves", "labels", "data"), 
                           k=1, ...) {
  
  which <- match.arg(which)
  
  ord <- model$order
  
  if (which == "segments") {
    grps <- cutree(model, k=k)
    return( segment(dendro_data(model, ...)) %>%
              mutate(order=ord[round(xend)], 
                     group=grps[ord[round(xend)]])
            
    )
  }
  
  if (which %in% c("leaves", "labels")) { 
    ord <- model$order
    return( label(dendro_data(model, ...)) %>% 
              mutate( order = ord )
    )
  }
  
  # if (which == "heatmap0") {
  #   res <- fortify(model, data, which="data") %>% 
  #     melt(id.vars = c("idx","position")) %>% 
  #     mutate(variable = as.character(variable))
  #   uv <- unique(res$variable)
  #   res$variable_num <-  sapply( 
  #     res$variable, 
  #     function(x) which(x == uv)
  #   )
  #   return(res)
  # }
  
  if (which == "heatmap") {
    res <- fortify(model, data, which="data") %>% 
      tidyr::gather(variable, value, -idx, -position) %>%
      mutate(variable = as.character(variable))
    uv <- unique(res$variable)
    res$variable_num <-  sapply( 
      res$variable, 
      function(x) which(x == uv)
    )
    return(res)
  }
  if (which == "data") {  
    if (missing(data)) { 
      stop('missing data')
    } else {
      return( 
        data %>% 
          mutate(idx = 1:nrow(data),
                 position = order(ord))
      )
    }
  }
  
}




#' @rdname clustering
#' @param object an object of class `"hclust"`
#' @param colorize whether to show clusters in different colors
#' @param k number of clusters
#' @param labels a logical indicating whether labels should be used
#' to identify leaves of the tree.
#' @param heatmap the ratio of size of heatmap to size of dendrogram.  
#'   Use `0` or `FALSE` to omit the heatmap.
#' @param enumerate a color used for numbers within heatmap.  Use 
#'   `"transparent"` to hide.
#' @examples
#' KidsFeet %>% select(-name, -birthmonth) %>% rescale() -> KidsFeet2
#'   M <- dist(KidsFeet2)
#'   Cl <- hclust(M)
#'   fortify(Cl, k=5) %>% head(3)
#'   fortify(Cl, which="heatmap", data=KidsFeet2) %>% head(3)
#'   fortify(Cl, which="data", data=KidsFeet2) %>% head(3)
#'   fortify(Cl, which="labels") %>% head(3)
#'   mplot(Cl, data=KidsFeet2, k=4, heatmap=2)
#'   mplot(Cl, data=KidsFeet2, k=4, heatmap=0.5, enumerate="transparent")
#'   mplot(Cl, data=KidsFeet2, k=4, heatmap=2, type="triangle")
#'   mplot(Cl, data=KidsFeet2, k=4, heatmap=0, type="triangle")
#' @export

mplot.hclust <- function(object, data, colorize = TRUE, k=1, 
                         labels = FALSE,
                         heatmap = 0, 
                         enumerate="white", ...) {
  ggenv <- parent.frame()
  ggenv$h=5
  p <- ggplot( environment = ggenv ) 
  if (colorize && k > 1) { 
    p <- p + 
      geom_segment(data=fortify(object, which="segments", k=k, ...),
                   aes(x=x, y=y, xend=xend, yend=yend, colour=factor(group))) +
      guides(colour=guide_legend(title="group"))
      
  } else { 
    p <- p + geom_segment(data=fortify(object, which="segments", ...),
                          aes(x=x, y=y, xend=xend, yend=yend ))
  }
  
  if (labels) {
    lobject <- fortify(object, which="leaves")
    p <- p + 
      scale_x_continuous(breaks = lobject$x,
                         labels = lobject$label) 
  }
  
  if (heatmap) {
    HeatMapData <- fortify(object, data, which="heatmap") %>%
      mutate(h = rescale(variable_num, heatmap * max(object$height) * c(-1/12, -1)))
    TicksData <-  
      HeatMapData %>%  
      group_by(variable) %>% 
      summarise(pos=unique(variable_num), h=unique(h)) 
    p <- p + 
      geom_tile(data=HeatMapData,
                aes(x=position, 
                    y= h,
                    fill=value), 
                colour="white") +
      geom_text(data=HeatMapData,
                aes(x=position, y= h, label=idx), 
                colour=enumerate, size=3, angle=90) +
      scale_y_continuous(
        breaks = TicksData$h,
        labels = TicksData$variable
      )
#      geom_text(data = HeatMapData %>% 
#                  group_by(variable) %>% 
#                  summarise(pos=unique(variable_num), h=min(h)), 
#                aes(x = 0, y = h, label=variable),
#                hjust=1)  
  }
  p <- p + theme_minimal() + 
           labs(x="", y="") + 
           theme(axis.ticks.y=element_blank())
  if (labels) {
    p <- p + theme(axis.text.x = element_text(angle=90, hjust=1), 
        axis.ticks.x=element_blank())
  }
  p
}


