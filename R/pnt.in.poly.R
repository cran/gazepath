## new pnt.in.poly function without SMDTools
pnt.in.poly <- function(pnts, polypnts){
  pip <- point.in.polygon(pnts[,1], pnts[,2], polypnts[,1], polypnts[,2])
  pip <- ifelse(pip > 0, 1, 0)
  return(data.frame(x = pnts[,1], y = pnts[,2], pip))
}