plotMould <-
function(uni, set, gap, thresholds, lmax, Hz){
  null <- rep(0, Hz)
  plot(set ~ thresholds, ylim = c(0,max(set)), col = 'grey30', type = 'l', bty = 'n', ylab = '', xlab = 'Speed thresholds, deg/s', xaxt = 'n', las = 2, main = '2. Velocity threshold estimation')
  mtext('Frequency of the local speed', 2, 3.5, cex = .65)
  mtext('maxima exceeding the threshold', 2, 2.5, cex = .65)
  lines(rep(0, max(lmax)), col = 'grey30')
  polygon(c(thresholds, rev(thresholds)), c(set, rev(null)), col = 'grey30', border = NA)
  lines(uni ~ thresholds, lty=2, lwd=2)
  lines(gap ~ thresholds, lwd=2, col=2)
  segments(thresholds[which.max(gap)], 0, thresholds[which.max(gap)], max(set), lwd=4)
  axis(1, at = seq(0, 1000, 30))
}
