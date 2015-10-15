#results/fxn_PlotMisingData.R


#plot sparse data
PlotSparse<-function(dat2c, ylabs){
  plot(x=dat2c$xval, y=dat2c$yi, ylab='', xlab='',las=1, bty="l", cex=dat2c$size)
  mtext(ylabs[i], side=2, line=2.5, cex=.8)
  abline(h=0, lty="dotted") #dotted line at RR=1 (no difference between groups)
}

#plot empty dataset
PlotEmpty<-function(){
  plot(1:2, 1:2, ylab='', xlab='',las=1, bty="l", type='n', xaxt='n', yaxt='n')
  mtext(ylabs[i], side=2, line=2.5, cex=.8)
}
