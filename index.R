## ----word1, echo=F, inclue = T-------------------------------------------

#### Define utility function
'%f%' <- function(x,f) { do.call(what = f,
                                 args = list(x)) }

#### Read data from file
f = file('rpc2017/a1/usr-share-dict-words','r')
words  <-  readLines(f)
close(f)

#### Convert to uppercase and count unique number
words <- toupper(words)
words <- unique(words)
sprintf('A: %d', length(words))%f%cat;

## ----word2, echo=F-------------------------------------------------------
buf <- c()
match_idx <- grep('\'',words)
buf <- c(buf,sprintf('A: %d ', length(match_idx)))

######### Print hits
hlen = 10
df = data.frame( words[match_idx])
caption = sprintf('Showing best %d hits below:',hlen)
knitr::kable( 
  head(df,hlen),
  booktabs = TRUE,
  caption = caption
)
words <- words[-match_idx]
buf <- c(buf,sprintf('\n%d words left after filtering', length(words)))
buf%f%cat

## ----word3, echo=F-------------------------------------------------------
buf <- c()
match_idx <- grep('[^ -~]',words)
buf <- c(buf,sprintf('A: %d ', length(match_idx)))

######### Print hits
hlen = 10
df = data.frame( words[match_idx])
caption = sprintf('Showing best %d hits below:',hlen)
knitr::kable( 
  head(df,hlen),
  booktabs = TRUE,
  caption = caption
)



words <- words[-match_idx]
words_data <- words
buf <- c(buf,sprintf('\n%d words left after filtering, saved to "words_data"', length(words)))
buf%f%cat

## ----word4, echo=F-------------------------------------------------------
words_data <- sort(words_data)
putative <- words_data[
  endsWith(words_data, 'OG')
  ]
target <- vapply( putative
                  , FUN = {function (x) paste0(x,'UE')}
                  ,FUN.VALUE='char'
                  ,USE.NAMES=T)
XOGUE <- words_data[match(target,words_data)]
XOG   <- putative 
result <- data.frame(XOG,
                     XOGUE,
                     row.names = NULL) 
colnames(result)<- c("XOG","XOGUE") 
result <- result[complete.cases(result),]
rownames(result) <- 1:nrow(result)
sprintf('%d pairs of words were found to conform "XOG"->"XOGUE" projection', nrow(result))%f%cat
# print(result,  row.names = T)

######### Print hits
hlen = 10
df = result
caption = sprintf('Showing best %d hits below:',hlen)
knitr::kable( 
  head(df,hlen),
  booktabs = TRUE,
  caption = caption,
  align = 'l'
)



## ----word5,echo=F--------------------------------------------------------
fname = 'rpc2017/a1/scrabble.txt'
f = file(fname,'r')
buf = f%f%readLines
buf = strsplit(buf,' ')
f%f%close
buf = t(array(unlist(buf),dim = c(length(buf[[1]]),length(buf) )))
buf <- buf[order(buf[,1]),]

l = length(unique(buf[,1]))
if (l != 26){
  msg = paste0('[WARNING]: scrabble scores incomplete, expected 26, actual ',l)
  warning(msg)
}
# data.frame( char = buf[,1], score = buf[,4]) %f%print
scores <- buf[,ncol(buf)] %f% as.integer
names(scores) <- buf[,1]
hlen = 10
df = data.frame(scores = sort(scores,decreasing = T))

caption = sprintf('Showing best %d hits below:',hlen)
knitr::kable(
  head(df,hlen),
  booktabs = TRUE,
  caption = caption,
  align = 'l'

)


## ----word6,echo = F------------------------------------------------------
x = words_data
scoreit = {function (x){
                      sum(
                          unlist(
                            lapply( strsplit(x,''),
                            FUN = {function(i) scores[i]}
                            )
                          )
                      )
                    }
          }
y <- sapply( x,
                   FUN = scoreit,
                  # FUN.VALUE='integer',
                  USE.NAMES=T)
dy <- density( y, bw = 0.6)
# par(mar=c(3,3,3,3))
plot( dy,
      main='Distribution of scrabble scores')
my = mean(y)
abline( v = my, col=2 )
text(my, median(dy$y),sprintf('mean=%.3f',my), col = 2)

#### DEBUG stats
if (debug){dy%f%print}

cat(
  "Highest score is achieved by:",
  names(y)[idx<-which.max(y)],
  ", Score:",
  y[idx]
)

## ----word7---------------------------------------------------------------
##################################################################
#### compile reverse complement and define functions #############
##################################################################
dict = rev(LETTERS)
names(dict) <- LETTERS 

rev_comp = function(x){
  paste0(
    rev(dict[unlist(strsplit(x,''))])
  ,collapse = "")
}


##################################################################
#### Carry out search 
##################################################################

idx = match(
  sapply(words_data, FUN=rev_comp),
  words_data
)%f%na.omit

##################################################################
#### linking  WORDS to their reverse complements #################
#### Sort and format 
##################################################################
rv_0 = words_data[idx]
rv_1 = sapply( rv_0, FUN = rev_comp )

res <- data.frame(
  rv_0,
  rv_1)
res <-  apply(res, MARGIN = 1, FUN = sort)%f%t
dup_idx <- duplicated(res[,1])
res <- res[!dup_idx,]
res <- res[ order(nchar(res[,1]), decreasing = T),]

colnames(res) = c('smaller_word','larger_word')
rownames(res) <- 1:nrow(res)
sprintf('We found %d entries, out of which %d are reverse-complement to themselves , Showing longest 10 entries', nrow(res), sum(res[,1]==res[,2]),'\n') %f%cat

######### Print hits
hlen = 10
df = res
caption = sprintf('Showing best %d hits below:',hlen)
knitr::kable(
  head(df,hlen),
  booktabs = TRUE,
  caption = caption,
  align = 'l'

)

## ----word8---------------------------------------------------------------
count = function(lst){
  sapply(lst, {function(x){
    if (x[1]==-1){
      0
    }else{
      length(x)
    }
    }} )
}


##### Define generic query function
search_perm <- function (query_orig_str = 'LLFAUYPNI',
                         min_len = 4,
                        database =  words_data,
                        special  = c('A')
                        ){
  
  #### Preprocess the given string (vectorizing etc.)
  query_orig_str <- query_orig_str
  query_orig = strsplit(query_orig_str,'')[[1]]

  #### Obtain the unqiue set of chars
  query_uni = query_orig%f%unique

  #### Count occurrence of chars in the query
  qcount = sapply( query_uni, 
          {function(subq)
            {count(
              gregexpr(subq, query_orig_str)
              )}})
  
  #### Counting occurence of chars for each word in database
  raw = sapply( query_uni, 
          {function(subq)
            {count(
              gregexpr(subq, database)
              )}})
  
  if(debug){
    raw0 <- raw
    raw <- raw0
  }
  
  ### add index to initial dataset
  rownames(raw) = 1:nrow(raw)
  
  
  #### Exclude results that use more letters than specified
  exidx = apply(
    sweep(raw,qcount,MARGIN=2,FUN={function(x,y) x>y}),
    MARGIN=1,
    FUN = any )
  raw <- raw[!exidx,]
  
  #### Exclude result that does not contain special chars
  exidx <- apply(
    sweep(raw==0, query_uni %in% special,
         MARGIN = 2,
         FUN = '&'),
        MARGIN = 1,
        FUN = any)
  raw <- raw[!exidx,]
  
  ###### Compile into a data.frame
  words_res = database[rownames(raw)%f%as.integer]
  words_score = apply( raw,MARGIN = 1, sum)
  
  output = data.frame(
    words_res,
    words_len = nchar(words_res),
    words_score
    )
  
  ##################################################################
  #### Filtering out words that require additional characters ##############
  
  #### Option 1: Do not allow extra chars
  inidx <-  output$words_len==output$words_score & output$words_score>=min_len
  
  #### Option 2: Do allow extra chars
  # inidx <-  output$words_score>=min_len
  
  output <- output[inidx,]
  
  odx = order(output$words_score,decreasing = T)
  output <- output[odx,]
}


### Actual evaluating the demanded query
query_orig_str = 'LLFAUYPNI'
min_len = 4
database =  words_data
special = c('A')

output <- search_perm(
    query_orig_str = query_orig_str,
    min_len = min_len,
    database =  database,
    special = special
)

#### Check correctness
if (nrow(output) != 39)
{
  warning("[WARNING]:Number of result is different from the cached one")
}


sprintf('We found %d hits according to query \"%s\" in \"words_data\"", \n (Option: min_len=%d, special=%s). ', nrow(output), query_orig_str, min_len ,paste0(special,collapse = ','))%f%cat
# sprintf('Showing best %d hits below:',hlen) %f% cat

######### Print hits
hlen = 20
df = output
caption = sprintf('Showing best %d hits below:',hlen)
knitr::kable(
  head(df,hlen),
  booktabs = TRUE,
  caption = caption,
  align = 'l'
)


## ----exam1---------------------------------------------------------------
#### Read data 
setwd('rpc2017/a1/grading/')


##### read correct answers into "ref"
fname = "crib.dat"
f = file( fname,'r')
ref  <-  readLines(f)
close(f)
qnum <- length(ref)
qlevel = unique(ref)%f%sort
qdict <- 1:length(qlevel)
names(qdict) <- qlevel

sprintf("[DEBUG]:There are %d questions from file:%s",qnum,fname)%f%cat

####### Loading grading scheme and transform it to a vector
grade_crit <- read.table(file = 'grade.txt',header = T)
grade_dict <- rep(0,101)
for (i in 1:nrow(grade_crit)){
  ROW <- grade_crit[i,]
  MIN = ROW[[1]]
  MAX = ROW[[2]]
  grade = ROW[[3]]
  grade_dict[ MIN:MAX + 1] <- grade  
}
grade_dict <- factor(grade_dict, labels = grade_crit$grade%f%levels) 
graduate <- function(x){grade_dict[x+1]}

##### Test 
if ( any(!graduate(c(39,40,49,50,59,60,69,70)) == c('F','D','D','C','C','B','B','A')) ){
  warning("[WARNING]:Cannot safely define \"graduate()\"")
}

##### Read results for each student
buf = list()
resp = list()
for (fname in list.files(getwd())){
  if ( startsWith(fname,'student') ){
    
  ##### Extract student number from filename and read data
  idx = regmatches( fname,gregexpr("[0-9]+", fname))%f%as.integer
  f = file(fname,'r')
  ttbl <- read.table(f, header = T,stringsAsFactors = F)
  close(f)
  ##### score answers and cache answers to "resp"
  score = sum(ref[ttbl$qn] == ttbl$response)
  buf[[idx]] = list( student_idx = idx, 
                     score = score, 
                     qnum = nrow(ttbl)
                     )
  ttbl$response <- qdict[ttbl$response] 
  resp[[idx]] = ttbl
  }
}

#### Compile data frame
df <- data.frame(matrix(unlist(buf), nrow=length(buf), byrow=T))
colnames(df) <- names(buf[[1]])
df$percent <- floor(df$score / df$qnum * 100)
df$grade <- df$percent%f%graduate
df$rank <- rank(-df$percent)
df <- df[order(-df$score),]
if (sd(df$qnum)!=0){
  warning("Question answered is not homogeneous")
}

df <- subset(df,select = -c(qnum)) #### Drop columns for better printout
hlen <- nrow(df)
caption = sprintf('Result of %d students in this exam, sorted by percent score in descending order', nrow(df) )
knitr::kable( 
  head(df,hlen),
  booktabs = TRUE,
  caption = caption,
  row.names = F
  # align = 'l'
)
sprintf("Stats were calculated for the rounded percent score for %d students in the exam", nrow(df))%f%cat
summary(df$percent)%f%print
res_exam <- df


## ----exam_naive,fig.cap=" \\label{fig:exam_naive} Distribution of $I_{cheat}$"----
idx_xy <- t(combn(1:nrow(res_exam),2))
res_exam <- res_exam[order(res_exam$student_idx),]

# df <- res_exam
cindex <- function(sa,sb,df = resp){
  sumA <- intersect(df[[sa]]$qn,df[[sb]]$qn) %f% length
  sumB <- intersect(data.frame(t(df[[sa]]))
            ,data.frame(t(df[[sb]]))
              )%f% ncol
  c(sumA,sumB)
  }  

overlap <- apply(idx_xy, MARGIN = 1, FUN = {function(x) 
  cindex( x[1],x[2])
}
)%f%t
I_cheat <- overlap[,2]/overlap[,1]
hist(I_cheat,main = NULL);


## ----exam_diag, fig.cap= cap---------------------------------------------

cap <- "\\label{fig:exam_diag} Diagnostic plots for finding the outlier/cheater"

##### Calculate naive cheat index
idx_xy <- t(combn(1:nrow(res_exam),2))
res_exam <- res_exam[order(res_exam$student_idx),]

cindex <- function(sa,sb,df = resp){
  sumA <- intersect(df[[sa]]$qn,df[[sb]]$qn) %f% length
  sumB <- intersect(data.frame(t(df[[sa]]))
            ,data.frame(t(df[[sb]]))
              )%f% ncol
  c(sumA,sumB)
  }  


#### take two students and find overlap in choic of questions
overlap <- apply(idx_xy, MARGIN = 1, FUN = {function(x) 
  cindex( x[1],x[2])
}
)%f%t
question_overlap <- overlap[,1]
answer_overlap <- overlap[,2]
x <- question_overlap
y <- answer_overlap

#### regression
mdl <- lm(y~x)
mdl$stdres <- rstandard(mdl)

###### Thresholding outliers
conf <-  0.975 #### 1-tail p-value
cap <- paste0(cap,
sprintf("\\newline (Using %.3f %% confidence (2 tailed) as threshold)", (2 * conf - 1 )*100)
              )
zlim <- qnorm(conf)
outliers <- which( abs(mdl$stdres) > zlim)

##### Print plots
par(mfrow=c(1,2))
plot(x,y,
     xlab = "Number of overlapped questions",
     ylab = "Number of overlapped answers",
     xlim = c(-1,35),
     ylim = c(-1,35)
    )

text( x[outliers], y[outliers], outliers,adj = c(-.3,-.3))
abline(mdl,col = 2)

plot(mdl$stdres,ylab='standardised residual',
     ylim = c(-6,6))
abline(h = zlim, col=2)
abline(h =-zlim, col=2)
abline(h = 0,lty=2)
text( outliers, mdl$stdres[outliers], outliers,adj = c(-.3,-.3))

#### compile data frame
cheats<-data.frame(
  stud_A = idx_xy[,1],
  grad   = df$grade[idx_xy[,1]],
  stud_B = idx_xy[,2],
  grad   = df$grade[idx_xy[,2]],
  ques_same = question_overlap,
  ans_same = answer_overlap,
  zscore = mdl$stdres,
  Confidence_to_reject_NULL = 2 * pnorm(abs(mdl$stdres)) - 1
)

cheats <- cheats[order(-(cheats$zscore)),]  

######### Print hits
hlen = 10
df <- cheats
df <- subset(df,select = -c(zscore)) #### Drop columns for better printout
caption = sprintf('Showing best %d hits below:',hlen)
knitr::kable(
  head(df,hlen),
  booktabs = TRUE,
  caption = paste0('\\label{tab:exam_diag}',caption)
)



## ----DMB_log, fig.cap= cap,eval=T----------------------------------------
source('DMB.R')
#################################################
### simple visulisation of N(s,t) ###############
#################################################
cap <- " \\label{fig:DMB_log} Simple visualisation without taking differential " 
L=20
pop = rep(5,L)
# pop <- rnorm(L,mean = 1, sd = 4 )
d<- 0.1
rate <- rep( c(0.9,1.1),c( L/2, L/2)) # %f% shuffle
# rate <- rep( c(0.9,1.1),c( L/2, L/2)) %f% shuffle

s0 <- list(    pop = pmax(0,pop),
               rate = pmax(0,rate),
               filter= c(d, 1-2*d, d),
               nhis = 500
               )
s0$his <- do.call(rbind, rep(list(pop), s0$nhis))
s0$L <- length(s0$pop)
s1<-s0
s1 <- kickoff(s1,100)
# f <- function(x){log10(1+10*x)}
simple_vis(s1)

## ----DMB_logdiff,fig.cap = cap,eval = T----------------------------------
###################################################################
### visulisation of N(s,t) after taking differential w.r.t. time 
###################################################################
cap <- " \\label{fig:DMB_logdiff} Visualisation after taking differential w.r.t. time" 
s1<-s0
s1 <- kickoff(s1,500)
diff_vis(s1)


## ----reflect_sep,fig.cap=cap---------------------------------------------
cap <- " \\label{fig:reflect_sep} Left: $\\lambda_{end}$ stays invariant under reflection operation; \\newline Right: dotted-line indicates edge of {s}"

source('DMB.R')
init_row21()


L=20
pop = rep(5,L)
d<- 0.1
bad = 0.9
good = 1.1
rate <- rep(bad,L)
s0 <- list(    pop = pmax(0,pop),
               rate = pmax(0,rate),
               filter= c(d, 1-2*d, d),
               nhis = 500
               )
s0$his <- do.call(rbind, rep(list(pop), s0$nhis))
s0$L <- length(s0$pop)

rate[1:8] <- good
rate_ref<-rate
s0$rate<-rate
s1 <- kickoff(s0,400)
diff_vis_init(s1)
par(xpd = F)
diff_add(s1,adj = c(1.5,-.5))


L=40
pop = rep(5,L)
d<- 0.1
bad = 0.9
good = 1.1
rate <- rep(bad,L)
s0 <- list(    pop = pmax(0,pop),
               rate = pmax(0,rate),
               filter= c(d, 1-2*d, d),
               nhis = 400
               )
s0$his <- do.call(rbind, rep(list(pop), s0$nhis))
s0$L <- length(s0$pop)
rate[1:16 + 12] <- good
s0$rate<-rate
s1 <- kickoff(s0,400)
diff_add(s1,adj = c(1.5, 1.2),col = 2)

legend(
  # x = 245, y = -.11
          ,cex = 0.8 
    ,x="bottomright"
    # ,x="bottomleft" 
          ,legend=c(
            # sprintf('{%s}'),expression(s))
             # substitute(t[0]==t0)
            # parse(text ="'{s}'=='[0,20]''{s_good}'=='[0,8]'" )
            '{s}=[1,20], {s_good}=[1,8]'
            ,'{s}=[1,40], {s_good}=[13,28]'
            # expression(s="["0,10"]")
            # ,expression(10^{E[s](nu(s,t))})
                   )
          ,col=c(1,2), lwd=1, lty=c(1,1), 
          pch=c(NA,NA) )
plot(s0$rate,type = 'l',col=2
     ,xlab = 's'
     ,ylab = expression(lambda(s))
     ,main = 'Aligned spatial configuration')
points(c(rep(0,20),rate_ref),col = 1)
abline(v = c(20.5,40.5),col = 1,lty=2)
abline(v = c(0.5,40.5),col = 2,lty=3)


## ----data__lend_K,eval = F-----------------------------------------------
## ###################################################################
## ### Compiling data for equally-sized clusters at different separations
## ###################################################################
## Ks = 0:8
## bad = 0.8
## good = 1.2
## 
## 
## 
## 
## ##### Initialise grid
## L=200
## pop = rep(5,L)
## d<- 0.1
## rate <- rep(bad,L)
## s0 <- list(    pop = pmax(0,pop),
##                rate = pmax(0,rate),
##                filter= c(d, 1-2*d, d),
##                nhis = 20
##                )
## s0$his <- do.call(rbind, rep(list(pop), s0$nhis))
## s0$L <- length(s0$pop)
## 
## 
## L_goods = c(2,4,8,16,32)
##             # ,32)
## 
## #################################################
## ### Initialise OUTPUT matrix and collect data ###
## #################################################
## OUTPUT_M = matrix(0,length(L_goods), length(Ks),
##                   dimnames = list(L_goods,Ks))
## 
## for (L_i in 1:length(L_goods)){
##   L_good = L_goods[L_i]
##   n_good = L_good/2
## ?matrix
## OUTPUT = matrix(0,length(Ks),s0$nhis-1,
##                 dimnames = list(Ks,NULL))
##   for (i in 1:length(Ks)){
##     K = Ks[i]
##     offset = floor(K/2)
##     rate[1:L] = bad
##     rate[ c((L/2-offset-n_good+1):(L/2-offset),
##             (L/2-offset+K+1):(L/2-offset+K+n_good))] <- good
##     s0$rate<-rate
##     s1 <- s0
##     max_iter = 7
##     num_steps = 200
##     for (num in 1:max_iter){
##     s1 <- kickoff(s1,num_steps)
##     zs = diff(log10(s1$psum))
##     if (sd(zs)<1E-8){
##       break
##     }else if(num==max_iter){
##       warning(
##         sprintf("[WARNING]: lambda_global does not converge after %d steps",num*num_steps)
##       )
##     }
## 
##     }
##     OUTPUT[i,] = zs
##   }
## 
##   MEAN = apply(OUTPUT,MARGIN=1,mean)
##   OUTPUT_M[L_i,] = MEAN
## }
## ### save output
## save( L_goods,Ks,OUTPUT_M, file = "lend_K_dp1.dat")

## ----data__lend_K1,eval = F----------------------------------------------
## ###################################################################
## ### Compiling data for unequally-sized clusters at K = 1 ##########
## ###################################################################
## # Ks = 0:8
## bad = 0.8
## good = 1.2
## 
## ##### Initialise grid
## L=400
## pop = rep(5,L)
## d<- 0.1
## rate <- rep(bad,L)
## s0 <- list(    pop = pmax(0,pop),
##                rate = pmax(0,rate),
##                filter= c(d, 1-2*d, d),
##                nhis = 20
##                )
## s0$his <- do.call(rbind, rep(list(pop), s0$nhis))
## s0$L <- length(s0$pop)
## 
## # n_goods = c(2,4,8,16,32)
## n_goods = 0:12
##             # ,32)
## 
## 
## OUTPUT_M = matrix(0,length(n_goods), length(n_goods),
##                   dimnames = list(n_goods,n_goods))
## 
## for (n_i in 1:length(n_goods)){
##   n_goodi = n_goods[n_i]
##   OUTPUT = matrix(0,length(n_goods),s0$nhis-1,
##                   dimnames = list(n_goods,NULL))
##   for (n_j in 1:length(n_goods)){
##     n_goodj = n_goods[n_j]
##     rate[ 1:L ] = bad
##     rate[c((L/2-n_goodi):(L/2-1),
##            (L/2+1):(L/2+n_goodj)
##            )] <- good
##     s0$rate<-rate
##     s1 <- s0
##     max_iter = 5
##     num_steps = 200
##     for (num in 1:max_iter){
##       s1 <- kickoff(s1,num_steps)
##       zs = diff(log10(s1$psum))
##       if (sd(zs)<1E-5){
##         break
##       }else if(num==max_iter){
##         warning(
##         sprintf("[WARNING]: lambda_global does not converge after %d steps",num*num_steps)
##       )
##     }
##     }
##     # zs
##     OUTPUT[n_j,] = zs
##   }
##   MEAN = apply(OUTPUT,MARGIN=1,mean)
##   OUTPUT_M[n_i,] = MEAN
## }
## 
## ### save output
## save( OUTPUT_M, file = "lend_K1_dp1.dat")

## ----vis__lend_K_dp1, fig.cap = cap--------------------------------------
###################################################################
### Plotting data for clusters at separations   ###################
###################################################################
cap <- "\\label{fig:vis__K} Left: Equally sized clusters at different separation. Right: Unequally sized clusters at $K = 1$ "
init_row21()

load( file = "lend_K_dp1.dat")
p1 <- matplot(x = colnames(OUTPUT_M), t(10^OUTPUT_M),log='y',type= 'b',pch=2,
              xlab = expression(K),
              ylab = expression(lambda[end])
              ,cex = 0.8)
legend(x = 'bottomright',cex = 0.8
       , legend = sapply( rownames(OUTPUT_M),function(x){ 
             as.expression(
             bquote(L[good] ==.(x))
             )}
           )
       # , legend =  paste0(expression(L[good]),"=",rownames(OUTPUT_M))
       , col = 1:dim(OUTPUT_M)[2]
       , lty = 1:dim(OUTPUT_M)[2]
       , lwd = 1)

load( file = "lend_K1_dp1.dat")
OUTPUT_M <- OUTPUT_M[1:9,1:9]
p2 <- matplot(x = rownames(OUTPUT_M), 10^OUTPUT_M,log='y',type= 'b',pch=2,
              xlab = expression(L[right]),
              ylab = expression(lambda[end])
              ,cex = 0.8)
legend(x = 'bottomright',cex = 0.8
       , legend = sapply( rownames(OUTPUT_M),function(x){
           as.expression(
            bquote(L[left] ==.(x))
           )}
         )
        , col = 1:dim(OUTPUT_M)[1]
       , lty = 1:dim(OUTPUT_M)[1]
       , lwd = 1)

## ----data__max_cluster,eval = F------------------------------------------
## max_cluster <- function(rate, good = NULL){
##   if (is.null(good)){
##   good = max(rate)
##   }
##   N = length(rate)
##   max_vct = vector(mode='integer',length=N)
##   rbools = rate==good
##   edge = 1
##   MAX  = integer(1)
##   for (i in 1:N){
##     rbool = rbools[i]
##     MAX = (MAX * rbool) + rbool
##     if (!rbool){
##       edge = 0
##       }else if(i==N){
##       edge = 1
##     }
##   max_vct[i] = MAX * (edge + 1)
##   }
##   max(max_vct)
## }
## 
## 
## ##### Gathering Data
## for (L_good in c(30,35)){
## 
## set.seed(0)
## L=80
## # L_good = 30
## pop = rep(5,L)
## d<- 0.1
## good = 1.2
## rate <- rep(bad,L)
## s0 <- list(    pop = pmax(0,pop),
##                rate = pmax(0,rate),
##                filter= c(d, 1-2*d, d),
##                nhis = 20
##                )
## s0$his <- do.call(rbind, rep(list(pop), s0$nhis))
## s0$L <- length(s0$pop)
## rate[1:L_good] <- good
## N = 10000
## OUTPUT = list()
## OUTPUT$lend = matrix(0,N,s0$nhis-1)
## for (i in 1:N){
##     # rate <- shuffle(rate0)
##     s0$rate <- shuffle(rate)
##     s1 <- s0
##     max_iter = 5
##     num_steps = 200
##     for (num in 1:max_iter){
##       s1 <- kickoff(s1,num_steps)
##       zs = diff(log10(s1$psum))
##       if (sd(zs)<1E-5){
##         break
##       }else if(num==max_iter){
##         warning(
##           sprintf("[WARNING]: lambda_global does not converge after %d steps \n",num*num_steps)
##         )
##       }
##     }
##   OUTPUT$lend[i,] = zs
##   OUTPUT$max_cluster[i] <- max_cluster(s0$rate)
## if (i %% 100==0){
##   sprintf("[PROGRESS]:%d of %d \n",i,N)%f%cat
## }
## }
## fname = sprintf("L%d_Good%d_N%d.dat",L,L_good,N)
## save(OUTPUT,file = fname)
## }
## 
## ''

## ----vis__max_cluster,fig.cap=cap----------------------------------------
source("plotter.R") #### Utils for ggplot2 
###################################################################
### Plotting lambda_{end} for randomly sampled lambda(s)    #######
###################################################################

cap <- " \\label{fig:vis__max_cluster} $\\lambda_{end}$ for randomly sampled spatial configurations.  Left: Plotted with raw count; Right: Count is normalised for each $max(cluster\\_size)$ so that highest bar measures 1.0"
fname = "L80_Good30_N10000.dat"
load(fname)
N = gsub(".*N(\\d+).*","\\1", fname)


##### Modify data frame
OUTPUT$lend_mean = apply(OUTPUT$lend,MARGIN=1,mean)
OUTPUT$stdev = apply(OUTPUT$lend,MARGIN=1,mean)
bw = 0.0002
OUTPUT$lend_mean <- 10^OUTPUT$lend_mean
bw = 0.0005
OUTPUT$lend = NULL
df<- data.frame(OUTPUT)
df$max_cluster <- factor(round(df$max_cluster))


plot1 <- ggplot(df) + geom_histogram( aes(x = lend_mean, y = ..count..
                                 ,fill =  max_cluster
                                 # ,colour = max_cluster
                                 ),binwidth = bw
                             ) + 
  xlab(expression(lambda[end])) + labs(fill='max(cluster_size)') 
plot2 <- ggplot(df) + geom_histogram( aes(x = lend_mean, y = ..ncount..
                                 ,fill =  max_cluster
                                 # ,colour = max_cluster
                                 ),binwidth = bw
                             ) + 
  xlab(expression(lambda[end])) + ylab("Normalised count")

grid_arrange_shared_legend(plot1, plot2,ncol=2, top = sprintf("N = %s",N)) 

## ----aov__max_cluster----------------------------------------------------
##########################################
############# ANCOVA analysis  ###########
##########################################
df$lambda_end <- df$lend_mean
df.lm <- lm( lambda_end~as.numeric(max_cluster),data  = df)
res = anova( df.lm)
print(res)

## ----data__N1,eval = F---------------------------------------------------
## ###############################################################
## ### Compile data of lambda_global from randomly sampled N(s)###
## ###############################################################
## 
## pop = rep(5,L)
## d<- 0.1
## bad = 0.9
## good = 1.1
## rate <- rep(bad,L)
## s0 <- list(    pop = pmax(0,pop),
##                rate = pmax(0,rate),
##                filter= c(d, 1-2*d, d),
##                nhis = 300
##                )
## s0$his <- do.call(rbind, rep(list(pop), s0$nhis))
## s0$L <- length(s0$pop)
## 
## rate[1:L/2] <- good
## s0$rate<-rate
## N = 1000
## set.seed(0)
## par(xpd = F)
## s1 <- kickoff(s0,500)
## # diff_vis_init(s1)
## OUTPUT <- vector("list",N)
## for (i in 1:N){
##   s0$pop = pmax(0,runif(L,0,100))
##   s1 <- kickoff(s0,300)
##   OUTPUT[[i]] = s1
## }
## fname = sprintf('traj_%d.dat',N)
## save(OUTPUT,file = fname)
## cat(
##   sprintf("[DONE]:saved to %s",fname)
## )

## ----vis__N1,fig.cap=cap-------------------------------------------------
###################################################
### plot lambda_global for randomly sampled N(s)###
###################################################
cap <- " \\label{fig:vis__N1} $\\lambda_{global}(t)$ for $N_1(s)\\sim unif(0,100)$" 
load(file = "traj_1000.dat")
l_global<-sapply(OUTPUT,function(x){diff(log10(x$psum[1:101]))})
mat<- l_global
N = dim(mat)[2]
ts<-matrix(rep(1:dim(mat)[1],dim(mat)[2]),dim(mat)[1],dim(mat)[2])
idx<-matrix(rep(1:dim(mat)[2],dim(mat)[1]),dim(mat)[1],dim(mat)[2], byrow =  T)
df = data.frame( l_global = as.vector(l_global),
                 t = as.vector(ts),
                 idx = as.factor(idx))
require(ggplot2)
require(gridExtra)
d = ggplot(df,aes(x=t,y=10^l_global))
p1 <- d +
  geom_line(aes(group=idx,colour=idx) , show.legend = F) + 
  ylab(expression(lambda[global]))
# +
#   scale_y_log10(breaks=c(1,2,5,10,25))
p2 <- d + stat_summary(fun.data ="mean_sdl",  geom = "smooth") + 
  ylab(expression(lambda[global]))

grid.arrange(p1,p2,ncol = 2,top = sprintf("N = %d", N))

