########################################################################################################################
########################################################################################################################
##################################
##################################        Ploton et al. 
##################################        
##################################        Spatial validation reveals poor predictive performance of large-scale
##################################        ecological mapping models
##################################        
##################################        Nature Communications           -           06/08/2020
##################################                                                    for WINDOWS operating system
########################################################################################################################
########################################################################################################################

############
############  External functions
############

### "bin.var" function from the Rcmdr R package 
# Objective : this function splits a numeric vector into bins
# Details : see https://github.com/cran/RcmdrMisc/blob/master/R/bin.var.R

binVariable <- function (x, bins=4, method=c("intervals", "proportions", "natural"), labels=FALSE){
  method <- match.arg(method)
  
  if(length(x) < bins) {
    stop("The number of bins exceeds the number of data values")
  }
  x <- if(method == "intervals") cut(x, bins, labels=labels)
  else if (method == "proportions") cut(x, quantile(x, probs=seq(0,1,1/bins), na.rm=TRUE),
                                        include.lowest = TRUE, labels=labels)
  else {
    xx <- na.omit(x)
    breaks <- c(-Inf, tapply(xx, KMeans(xx, bins)$cluster, max))
    cut(x, breaks, labels=labels)
  }
  as.factor(x)
}
bin.var <- function(...) binVariable(...)

### "addTrans" function 
# Objective : this function allows adding transparency to colors in a base plot (like the "alpha" parameter of ggplot2)
# it's purely aesthetic

addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

### "get_density" function 
# Objective : this function allows generating density values to generate a heat plot
# it's purely aesthetic

get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}