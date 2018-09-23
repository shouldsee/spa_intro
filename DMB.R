

############################################################
### A modifier logarithmic function  #######################
############################################################
f <- function(x){log10(1+10*x)}
f_i <- function(x) { (10^x - 1)/10}


########## shuffle your vector using a random permutation ###############
shuffle <- function(vct){
  sample(vct, length(vct))
}

############################################################
### A circular shift function ##############################
############################################################
shifter <- function(x, n = 1) {
  if (n == 0) x else c(tail(x, -n), head(x, n))
}

#####################################################################
### Apply both geometric growth and spatial dispersal ###############
#####################################################################
forward <- function ( s, method = 1){
  growed <- s$rate * s$pop;
  if (method == 1){
    ##### naive implementation #####
    tpop  <- c(
      head(growed,1),
      growed,
      tail(growed,1)
    )
    
    neighbored <-rbind(
      shifter(tpop,1),
      tpop,
      shifter(tpop,-1)
    )
    pop <- apply(
      sweep(neighbored, MARGIN=1, s$filter,FUN = '*')
      , MARGIN=2 , sum)
    pop <- pop[2:(length(pop)-1)]
    
  }else if(method==2){
    ##### FFT implementation
    pop <-  filter( c(growed,rev(growed)), s$filter , sides = 2 ,circular = 1,
                    # method='recursive'
                    method='convolution'
    )[1:s$L]
  }
  # pop <-pmax(0,pop)
  s$hist = rbind(tail(s$hist, s$nhis-1),pop)
  s$pop <- pop 
  return(s)
}

###############################################################
########### wrapper to apply "forwrad" iteratively ############
###############################################################
kickoff<- function (s0,tmax=1){
  
  # t0 = Sys.time() #### timing
  s <- s0
  s$tmax <- tmax
  for (t in 1:tmax){
    s <- forward(s, method = 2 );
  }
  # t1 = Sys.time() #### timing
  # paste(t1-t0)    #### timing
  rownames(s$hist) <- 1:dim(s$hist)[1]
  s$psum<- apply(s$hist,MARGIN=1,FUN=mean)
  return(s)
}

 

## ------------- Plotting helpers functions-------------------------------------------------------------------

#############################################
###  mfrow = c(1,2) with extra params #######
#############################################
init_row21 <- function(){
  par(mfrow=c(1,2)
      ,mar=c(5.1,4.1,1.6,.0)
      ,mai=c(0.6,0.6,0.9,0.)
      ,mgp=c(1.2,0.4,0.1)
      ,cex.axis = 0.8
      ,tcl=-0.5 
      # ,family="serif"
      ,omi=c(0.2,0.0,0.5,0.)
      ,oma=c(1,1,1,1)
      ,xpd=F
  )
  
}

#############################################
###  plotting N(s,t)                  #######
#############################################
simple_vis <-function(s1){
  init_row21()
  par(xpd = T)
  ts = 1:s1$tmax
  ys <- f(s1$psum)
  xlab = expression(
    italic('t')
  )
  ylab = expression(
    E[s](N[t](s))
  )
  
  ytk = 10^(-1:5)
  ytk_act = f(ytk)
  
  
  plot(ts, ys,type='l',ylim =range(ytk_act),
       ylab = ylab,
       main = 'Average Population',
       xlab = xlab,
       axes = F
  )
  axis(2, at=f(ytk), labels=ytk)
  axis(1)
  
  par(xpd = F)
  x = 50;y=ys[50];
  y_act = f_i(y)
  text(x,y,sprintf("(%.2f,%.2f)",x,y_act),adj = c(-.3,-.5))
  abline(v = x,h= y,lty=2, col='red')
  
  x = 1;y=ys[1];
  y_act = f_i(y)
  text(x,y,sprintf("(%.2f,%.2f)",x,y_act),adj = c(-.3,-.5))
  abline(v = x,h= y,lty=2, col='red')
  
  xs <- 1:s1$tmax
  ys <- 1:s1$L
  zs <- f(s1$hist)
  surf <- persp(1:s1$tmax,
                1:s1$L,
                f(s1$hist),
                xlab = 'time(t)',
                ylab = 'space(s)',
                main = 'Trend of N(s,t): the population \n (f-transformed)',
                # zlab = expressionas indicated in (4).as indicated in (4).
                ,cex.axis = .7
                ,cex.lab = .8
                ,ticktype =
                  "detailed"
                ,zlab = 'f( N(t,s) )'
                ,theta = -25
                ,phi = 20
                ,col = 'lightblue'
                ,border= NA
  )
  
  
  lnum = 20
  for(i in floor(seq( 1, length(ys), length=lnum+1)[1:lnum]+length(ys)/2/lnum)) lines(trans3d(xs, ys[i], zs[,i], pmat = surf), col = "black")
  lnum = 20
  for(i in floor(seq( 1, length(xs), length=lnum+1)[1:lnum]+length(xs)/2/lnum)) lines(trans3d(xs[i], ys, zs[i,], pmat = surf), col = "black")
}

#############################################
###  plotting nu(s,t)                  ######
#############################################

#############################################
###  Adding a line on lambda_gloabl    ######
#############################################

diff_add <- function(s1,yname = "lglobal",addtext = T,...){
  ts = 2:s1$tmax
  kwargs<-list(...)
  kwargs$x <- switch (
    yname,
    "lglobal"= diff(f(s1$psum)),
    "Ev" = apply(diff(f(s1$hist)),MARGIN=1,FUN=mean)
  )
  adj = if (is.null(kwargs$adj)){ c(1.5,-.5)} else{kwargs$adj}
  kwargs$adj = NULL
  # kwargs$x = ys
  # kwargs$x = ts
  do.call("lines", args = kwargs )
  
  par(xpd = F)
  x = tail(ts,1);y=tail(kwargs$x,1);
  y_act = 10^(y)
  if (addtext){text(x,y,parse(text = sprintf("lambda[end]==%.3f", y_act)), adj = adj,col = kwargs$col)}
  # text(x,y,parse(text = "lambda[end]\"=0.075\""),adj = c(1.5,-.5))
  abline(v = x,h= y,lty=2, col='red')
  
}



diff_vis_init <- function(s1 , main = 'Global effective growth rate'){
  ts = 2:s1$tmax
  ys <- diff(log10(s1$psum))
  xlab = expression(
    italic('t')
  )
  ylab = expression(
    lambda[global]
    # f(E[s](N[t](s))) - f( E[s](N[t-1](s)))
  )
  
  ytk = 10^(-1:5)
  ytk = seq(0.8,1.2, by = 0.1)
  ytk_act = log10(ytk)
  
  # plot(0,0
  #      ,ylim =range(ytk_act)
  #      ,ylab = ylab,
  #      main = 'Global effective growth rate',
  #      xlab = expression(italic('t')),
  #      axes = F
  # )
  plot(ts, -1+ts*0,
       ,type='l'
       ,ylim =range(ytk_act)
       ,ylab = ylab,
       main = main,
       xlab = expression(italic('t')),
       axes = F
       ,col = 'red'
  )
  axis(2, at=log10(ytk), labels=round(ytk,3))
  axis(1)
  
}

diff_vis<-function(s1){
  init_row21()
  diff_vis_init(s1)
  # diff_add(s1)
  diff_add(s1,yname = "lglobal",col=2)
  diff_add(s1,yname = "Ev",col=1)
  
  #### Add legend
  legend( 
    ,cex = 0.8 
    ,x="bottomright"
    # ,x="bottomleft" 
    ,legend=c(
      expression(lambda[global])
      ,expression(10^{E[s](nu(s,t))})
    )
    ,col=c("red","black"), lwd=1, lty=c(1,1), 
    pch=c(NA,NA) )
  
  xs <- 2:s1$tmax
  ys <-1:s1$L
  zs <- diff(f(s1$hist))
  
  bad = 0.9
  good = 1.1
  zlim = c(bad,good)
  zlim = c(1/1.5,1.5)
  
  xs <- 2:s1$tmax
  ys <-1:s1$L
  
  par(xpd = T)
  ttl = 'Local effective growth rate'
  surf <- persp(
    xs,
    ys,
    zs,
    xlab = 'time(t)',
    ylab = 'space(s)',
    zlab = '',
    ,ticktype ="detailed"
    ,cex.axis = .8
    ,cex.lab = 1
    # ,zlab = 'f( N(t,s) )'
    ,zlim = log10(zlim)
    ,theta = -35
    ,phi = 20
    # ,border="blue"
    ,col = 'lightblue'
    ,border= NA
  )
  lnum = 20
  for(i in floor(seq( 1, length(ys), length=lnum+1)[1:lnum]+length(ys)/2/lnum)) lines(trans3d(xs, ys[i], zs[,i], pmat = surf), col = "black")
  lnum = 20
  for(i in floor(seq( 1, length(xs), length=lnum+1)[1:lnum]+length(xs)/2/lnum)) lines(trans3d(xs[i], ys, zs[i,], pmat = surf), col = "black")
  
  title( ttl, line = 0)
  
  mxs <- outer(xs,ys, {function(x,y)x}) %f% as.vector
  mys <- outer(xs,ys, {function(x,y)y}) %f% as.vector
  zs = log10(matrix(rep(s1$rate,s1$tmax-1,byrow =T),s1$L,s1$tmax-1)%f%t)
  mzs <- zs %f% as.vector
  
  pts <- trans3d( mxs, mys, mzs
                  ,pmat=surf)
  points(pts
         ,pch=20
         ,col=2
         ,cex=0.3
  )
  
  legend( .05,-.52
          ,cex = 0.8 
          # ,x="bottomright"
          # ,x="bottomleft" 
          ,legend=c(
            expression(log10(lambda(s,t)))
            ," v(s,t) "
            # ,"zlim : []"
          ),
          col=c("red","black"), lwd=1, lty=c(0,1), 
          pch=c(20,NA) )
}