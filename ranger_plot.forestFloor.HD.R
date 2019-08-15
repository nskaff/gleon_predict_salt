# https://github.com/sorhawell/forestFloor/blob/master/R/fcol.R

plot.forestFloor.HD = function(x, plot_seq=c(1,3,4,5,8), cols) {
  
  #short for features and feature contribution in object
  X = x$X
  FCs = x$FCmatrix
  FCuse = FCs[, x$imp_ind[plot_seq]]
  #make catogorical features numeric, save jitter.template
  jitter.template = rep(FALSE,dim(X)[2]) #list of what features are catagorical
  as.numeric.factor <- function(x,rearrange=TRUE) {
    if(is.numeric(x)) return(x)
    if(rearrange) x = match(x,levels(droplevels(x))) else x = match(x,levels(x))
    return(x)
  }
  for(i in 1:dim(X)[2]) {
    if(is.factor(X[,i])) {
      jitter.template[i]=TRUE
      this.fac=as.numeric.factor(X[,i])
      X[,i] = this.fac
    }
    if(is.character(X[,i])) X[,i] = as.numeric(X[,i])
  } 

  ##get importance for plotting
  imp = x$importance     #fetch importance
  imp.ind = x$imp_ind    #fetch importance ranking/indices
 
  p = list()
  for (j in 1:length(plot_seq)) {
    i = plot_seq[j]
      #the XY coordinates to plot
      xplot = data.frame(
        physical.value = jitter(X[,imp.ind[i]],factor=jitter.template[imp.ind[i]]*2),
        partial.contribution  = FCs[,imp.ind[i]])
      xplot = xplot %>% mutate(cols = fcol.HD(ffra,1)) 
      
      p[[j]] = ggplot(xplot) + geom_point(aes(x = physical.value, y = partial.contribution, color = cols),
                                          size = 0.8, alpha = 0.6) +
        ylim(min(FCuse), max(FCuse)) +
        theme_bw(base_size = 8) +
        theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
        scale_colour_identity() +
        labs(title = names(x$X)[imp.ind[i]])
  }
  # Plot grid 
  # do.call(plot_grid, c(p, list(labels = c('A', 'B', 'C','D','E'), label_size = 10, nrow = 2, align = 'hv')))
  return(p)
}

fcol.HD = function(ff,cols) {
  colM = ff$X #else colM = ff$FCmatrix
  #reorder colM by importance
  colM = colM[,ff$imp_ind]
  #convert matrix to data.frame
  colM = data.frame(colM)
  
  sel.colM = data.frame(colM[,cols])    #use only selected columns
  sel.cols = 1:length(cols) #update cols to match new col.indices of colM

  #restrain outliers by limit(std.dev) and normalize.
  sel.colM = box.outliers(sel.colM,limit=3)
  
  ###################
  len.colM = box.outliers(sel.colM,limit=Inf) * 19 + 1
  colours = viridis_pal()(20) [len.colM$colM...cols.]
  return(colours)
}



