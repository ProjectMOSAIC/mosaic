#' mosaic tools for clustering
#' 
#' @rdname clustering
#' @param model a model
#' @param data a data-like object
#' @param which which kind of fortification to compute
#' @param k the number of clusters to show
#' @param ... additional arguments passed on to \code{link{dendro_data}}
#' @export

fortify.hclust <- function(model, data, 
                           which=c("segments", "heatmap", "leaves", "labels", "data"), 
                           k=1, ...) {
  if (! (require(ggdendro) ) ) { stop("package 'ggdendro' is required") }
  if (! (require(reshape2) ) ) { stop("package 'reshape2' is required") }
  
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
  
  if (which == "heatmap") {
    res <- fortify(model, data, which="data") %>% 
      melt(id.vars = c("idx","position")) %>% 
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
#' @param colorize whether to show clusters in different colors
#' @param k number of clusters
#' @param heatmap the ratio of size of heatmap to size of dendrogram.  
#'   Use \code{0} or \code{FALSE} to omit the heatmap.
#' @param enumerate a color used for numbers within heatmap.  Use 
#'   \code{"transparent"} to hide.
#' @examples
#' KidsFeet %>% select(-name, -birthmonth) %>% rescale() -> KidsFeet2
#' M <- dist(KidsFeet2)
#' Cl <- hclust(M)
#' mplot(Cl, data=KidsFeet2, k=4, heatmap=2)
#' @export

mplot.hclust <- function(object, data, colorize = TRUE, k=1, 
                         heatmap = 0, enumerate="white", ...) {
  ggenv <- list(h=5)
  p <- ggplot( environment = ggenv ) 
  if (colorize && k > 1) { 
    p <- p + geom_segment(data=fortify(Cl, which="segments", k=k),
                          aes(x=x, y=y, xend=xend, yend=yend, colour=factor(group)))
  } else { 
    p <- p + geom_segment(data=fortify(Cl, which="segments"),
                          aes(x=x, y=y, xend=xend, yend=yend ))
  }
  
  if (heatmap) {
    HeatMapData <- fortify(object, data, which="heatmap") %>%
      mutate(h = rescale(variable_num, heatmap * max(object$height) * c(-1/12, -1)))
    
    p <- p + 
      geom_tile(data=HeatMapData,
                aes(x=position, 
                    y= h,
                    fill=value), 
                colour="white") +
      geom_text(data=HeatMapData,
                aes(x=position, y= h, label=idx), 
                colour=enumerate, size=3, angle=90) +
      geom_text(data = HeatMapData %>% 
                  group_by(variable) %>% 
                  summarise(pos=unique(variable_num), h=min(h)), 
                aes(x = 0, y = h, label=variable),
                hjust=1)  
  }
  p <- p + theme_dendro()
  p
}


