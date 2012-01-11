graphPaper = function(xticks=0:5, yticks=-5:5,
  xlim=c(min(xticks),max(xticks)),
  ylim=c(min(yticks),max(yticks)),
  xlab="x",
  ylab="y", maxxlabels=7,maxylabels=7,...) {
    axisYlabel=""
    axisXlabel=""
    if (nchar(ylab) > 3) axisYlabel=ylab
    if (nchar(xlab) > 3) axisXlabel=xlab
  
    plot( 0:1, type="n", xaxt="n",yaxt="n",bty="n",xlim=xlim,ylim=ylim,
         xlab=axisXlabel,ylab=axisYlabel,...)
    for (k in 1:length(xticks) )
      lines(xticks[k]*c(1,1), ylim, col="gray",lwd=1)

    for (k in 1:length(yticks) )
      lines(xlim,yticks[k]*c(1,1), col="gray",lwd=1)

    # Make sure there aren't too many labels on the axis
    
    if( length(xticks) > maxxlabels ) {
      skipfactor = ceiling( length(xticks)/maxxlabels)
      xticks = xticks[ c(1,-seq(-length(xticks), -2,by=skipfactor)) ]
    }

    if( length(yticks) > maxylabels ) {
      skipfactor = ceiling( length(yticks)/maxylabels)
      yticks = yticks[ c(1,-seq(-length(yticks),-2, by=skipfactor)) ]
    }
    xlabels = paste(xticks)
    ylabels = paste(yticks)
    # see if zero in is the scale
    ypos = min(ylim)
    if( 0 >= min(ylim) & 0 <= max(ylim) ) {
      ypos = 0
      xlabels[xticks==0] = "";
    }
    xpos = min(xlim)
    if( 0 >= min(xlim) & 0 <= max(xlim) ) {
      xpos = 0
      ylabels[yticks==0] = "";
    }
    axis(1, at=xticks,labels=xlabels, pos=ypos,lwd=2, hadj=1)
    axis(2, at=yticks,labels=ylabels, pos=xpos,las=2,lwd=2)
    if ( axisXlabel=="") text( max(xlim), ypos, xlab,pos=3)
    if ( axisYlabel=="") text( xpos, max(ylim), ylab,pos=3,srt=90)
    
  }

  
  
