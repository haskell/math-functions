
# Read data for vector from the CSV summary for the polynomials
# evaluation
load.poly <- function(name) {
  csv <- read.csv(name)
  vec <- subset(csv, grepl('vector',   Name))
  unb <- subset(csv, grepl('unpacked', Name))
  #
  vec$Name <- as.numeric( gsub( '.*_(\\d)', '\\1', vec$Name) )
  unb$Name <- as.numeric( gsub( '.*_(\\d)', '\\1', unb$Name) )
  #
  data.frame(
      nelm   = vec$Name,
      tVec   = vec$Mean * 1e9,
      liwVec = (vec$Mean - vec$MeanLB) * 1e9,
      liwVec = (vec$MeanUB - vec$Mean) * 1e9,
      tUnb   = unb$Mean * 1e9,
      liwUnb = (unb$Mean - unb$MeanLB) * 1e9,
      liwUnb = (unb$MeanUB - unb$Mean) * 1e9
  )
}

# Plot summary for the polynomials evaluation
plot.poly <- function( q ) {
  #
  n  <- q$nelm
  tV <- q$tVec/n
  tU <- q$tUnb/n
  #
  lo = min(tV,tU)
  hi = max(tV,tU)
  d  = hi-lo
  #
  plot( n, tV, type='b', col='blue', log='x',
        ylim=c( lo-d*0.05, hi+d*0.05)
      )
  points( n, tU, type='b', col='red')

}