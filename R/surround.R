.surround <-
function (x, pre = " ", post = " ", width = 8, ...) 
{
    x <- format(as.vector(x), ...)
    l <- length(x)
    format(paste(rep(pre, l), x, rep(post, l), sep = ""), width = width, 
        just = "centre")
}
