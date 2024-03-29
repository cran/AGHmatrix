#########################################################################
# 									
# Package: AGHmatrix 							
# 									
# File: Amatrix.R
# Contains: Amatrix 							
# 									
# Written by Rodrigo Rampazo Amadeu
# 									
# First version: Feb-2014 
# Last update: 20-Mar-2019 						
# License: GPL-3
# 								
#########################################################################

#' Construction of Relationship Matrix A
#'
#' Creates an additive relationship matrix A from a pedigree data in a 3-column way format based on ploidy level (an even number) and, if ploidy equals 4, based on proportion of parental gametes that are IBD (Identical by Descent) due to double reduction. Returns a dominance relationship matrix if dominance true (ploidy 2 only). Autopolyploid matrices based on Kerr (2012), used when `ploidy` argument is higher than `2` and `dominance=FALSE`.
#' Diploid additive numerator relationship matrix built as in Henderson (1976), used when `ploidy=2` and `dominance=FALSE`. Diploid dominance numerator relationship matrix built as in Cockerham (1954), used when `ploidy=2` and `dominance=FALSE`. For details of recursive method see Mrode (2005).
#'
#' @param data pedigree data name (3-column way format). Unknown value should be equal 0.
#' @param ploidy an even number (default=2).
#' @param w proportion of parental gametas IBD due to double reduction (default=0), only if ploidy=4. 
#' @param verify verifies pedigree file for conflictuos entries (default=TRUE).
#' @param dominance if true, returns the dominance relationship matrix
#' @param slater if true, returns the additive autotetraploid relationship matrix as Slater (2013)
#' @param ASV if TRUE, transform matrix into average semivariance (ASV) equivalent (K = K / (trace(K) / (nrow(K)-1))). Details formula 2 of Fieldmann et al. (2022). Default = FALSE.
#' @param ... arguments to be passed to datatreat()
#' 
#' @return Matrix with the Relationship between the individuals.
#'
#' @examples
#' data(ped.mrode)
#' #Computing additive relationship matrix considering diploidy (Henderson 1976):
#' Amatrix(ped.mrode, ploidy=2)
#' #Computing non-additive relationship matrix considering diploidy (Cockerham 1954):
#' Amatrix(ped.mrode, ploidy=2, dominance=TRUE)
#' #Computing additive relationship matrix considering autotetraploidy (Kerr 2012):
#' Amatrix(ped.mrode, ploidy=4)
#' #Computing additive relationship matrix considering autooctaploidy (Kerr 2012):
#' Amatrix(ped.mrode, ploidy=8)
#' #Computing additive relationship matrix considering autotetraploidy and double-
#' #reduction of 0.1 (Kerr 2012):
#' Amatrix(ped.mrode, ploidy=4, w=0.1)
#' #Computing additive relationship matrix considering 
#' #autotetraploidy and double-reduction of 0.1 (Slater 2014):
#' Amatrix(ped.mrode, ploidy=4, w=0.1, slater = TRUE)
#' #Computing additive relationship matrix considering autohexaploidy and double-
#' #reduction of 0.1 (Kerr 2012):
#' Amatrix(ped.mrode, ploidy=6, w=0.1)
#'
#' @author Rodrigo R Amadeu, \email{rramadeu@@gmail.com}
#' @references \emph{Cockerham, CC. 1954. An extension of the concept of partitioning hereditary variance for analysis of covariances among relatives when epistasis is present. Genetics 39, 859–882}
#' @references \emph{Feldmann MJ, et al. 2022. Average semivariance directly yields accurate estimates of the genomic variance in complex trait analyses. G3 (Bethesda), 12(6).}
#' @references \emph{Henderson, CR. 1976. A simple method for computing the inverse of a numerator relationship matrix used in prediction of breeding values. Biometrics 32, 69-83}
#' @references \emph{Kerr, RJ, et al. 2012. Use of the numerator relationship matrix in genetic analysis of autopolyploid species. Theoretical and Applied Genetics 124 1271-1282}
#' @references \emph{Mrode, RA. 2014. Chapter 2: Genetic Covariance Between Relatives and Chapter 9: Non-additive Animal Models in Mrode, RA. 2014. Linear models for the prediction of animal breeding values. Cabi, 3rd edition.}
#' @references \emph{Slater, AT, et al. 2013. Improving the analysis of low heritability complex traits for enhanced genetic gain in potato. Theoretical and Applied Genetics 127, 809-820}
#'
#' @export

Amatrix <- function(data = NULL,
                    ploidy=2,
                    w=0,
                    verify=TRUE,
                    dominance=FALSE,
                    slater=FALSE,
                    ASV=FALSE,
                    ...){
  if(ploidy%%2!=0)
    stop(deparse("Ploidy should be an even number"))
  
  if(ploidy!=2 & dominance)
    stop(deparse("Dominance relationship matrix is implemented only for ploidy=2"))
  
  if( is.null(data))
    stop(deparse("Please define the variable data"))
  
  unk=0
  cat("Verifying conflicting data... \n")
  flag<-verifyped(data)
  if(flag) 
    stop(deparse("Please double-check your data and try again"))
  
  cat("Organizing data... \n")
  orig.order <- as.character(data[,1])
  data.after.treat <- try(datatreat(data=data,unk=unk,...),silent=TRUE)
  
  # checking if order was fixed
  flag = FALSE
  flag = inherits(data.after.treat,"try-error")
  if(!flag)
    flag = (length(unique(data.after.treat$ind.data))!=nrow(data))

if(flag){
      cat("To organize the data in a fast way wasn't possible... \n")
      cat("Trying to organize in a slow (naive) way... \n")
      data.sorted <- sortped(data)
      data.after.treat <- try(datatreat(data=data.sorted,unk=unk,...))

      # checking if order was fixed
      flag = FALSE
      flag = inherits(data.after.treat,"try-error")
      if(!flag)
        flag = (length(unique(data.after.treat$ind.data))!=nrow(data))
  
      if(flag){
          cat("It wasn't possible to organize your data chronologically. We recommend you to do it by hand or use the flag 'naive_sort=TRUE'. If the problem persists, please contact this package mainteiner \n")
          return()
      }
  }
  
  data <- data.after.treat
  
  s <- data$sire
  d <- data$dire
  
  if( is.null(s) || is.null(d) )
    stop(deparse("Please define the variable s (sire) and/or d (dire)"))
  
  if( length(s) != length(d) )
    stop(deparse("Please verify the variable s (sire) and/or d (dire), they don't have the same length"))
  
  if( !is.numeric(s) || !is.numeric(d) )
    stop(deparse("Pleasy verify your data, it has to be 2 numeric vectors"))
  
  if( length(data$sire) > 1000 )
    cat("Processing a large pedigree data... It may take a couple of minutes... \n")
  
  
  n <- length(s)
  A <- matrix(NA,ncol=n,nrow=n)
  
  Time = proc.time()
  
  #### For ploidy 2 ####
  if(ploidy == 2){
    w <- NA
    cat("Constructing matrix A using ploidy = 2 \n")
    A[1,1] <- 1
    for( i in 2:n){
      
      ## Both are unknown
      if( s[i] == 0 && d[i] == 0 ){
        A[i,i] <- 1
        for( j in 1:(i-1))
          A[j,i] <- A[i,j] <- 0
      }
      
      ## Sire is unknown
      if( s[i] == 0 && d[i] != 0 ){
        A[i,i] <- 1
        for( j in 1:(i-1))
          A[j,i] <- A[i,j] <- 0.5*(A[j,d[i]])
      }
      
      ## Dire is unknown
      if( d[i] == 0 && s[i] != 0 ){
        A[i,i] <- 1
        for( j in 1:(i-1))
          A[j,i] <- A[i,j] <- 0.5*(A[j,s[i]])
      }
      
      ## Both are known
      if( d[i] != 0 && s[i] != 0 ){
        A[i,i] <- 1+0.5*(A[d[i],s[i]])
        for( j in 1:(i-1))
          A[j,i] <- A[i,j] <- 0.5*(A[j,s[i]]+A[j,d[i]])
      }
    }
  }
  
  if(dominance){
    cat("Constructing dominance relationship matrix \n")
    D <- matrix(NA,ncol=n,nrow=n)
    for(i in 1:n){
      for(j in 1:n){
        u1 <- ifelse(length(A[s[i],s[j]])>0,A[s[i],s[j]],0)
        u2 <- ifelse(length(A[d[i],d[j]])>0,A[d[i],d[j]],0)
        u3 <- ifelse(length(A[s[i],d[j]])>0,A[s[i],d[j]],0)
        u4 <- ifelse(length(A[s[j],d[i]])>0,A[s[j],d[i]],0)
        D[i,j] <- D[j,i] <- 0.25*(u1*u2+u3*u4)
      }
    }
    diag(D)<-1
    A<-D
    D<-NULL
  }
  
  #### For ploidy 4 ####
  if(slater==TRUE){
    listA <- list()
    
    cat(paste("Constructing matrix A using ploidy = 4 and proportion of double reduction =",w,";as in Slater et al. (2014) \n"))
    start.time <- Sys.time()
    
    A[1,1] <- (1+w)/4
    for( i in 2:n){
      
      ## Both are unknown
      if( s[i] == 0 && d[i] == 0 ){
        A[i,i] <- (1+w)/4
        for( j in 1:(i-1))
          A[j,i] <- A[i,j] <- 0
      }

      ## Sire is unknown
      if( s[i] == 0 && d[i] != 0 ){
        A[i,i] <- (5 + 7*w + 4*A[d[i],d[i]]*(1-w) ) / 24
        for( j in 1:(i-1))
          A[j,i] <- A[i,j] <- 0.5*(A[j,d[i]])
      }
      
      ## Dire is unknown
      if( d[i] == 0 && s[i] != 0 ){
        A[i,i] <- (5 + 8*w + 4*A[s[i],s[i]]*(1-w) ) / 24 ##On Slater in 7w, deriving from hand based on Kerr is 8w
        for( j in 1:(i-1))
          A[j,i] <- A[i,j] <- 0.5*(A[j,s[i]])
      }
      
      ## Both are known
      if( d[i] != 0 && s[i] != 0 ){
        A[i,i] <- (1 + 2*w + (1-w)*(A[s[i],s[i]]) + (1-w)*(A[d[i],d[i]]) + 3*A[s[i],d[i]] ) / 6
        for( j in 1:(i-1))
          A[j,i] <- A[i,j] <- 0.5*(A[j,s[i]]+A[j,d[i]])
      }
    }
    
    A <- 4*A
  }
  
  if(slater==FALSE && ploidy>2){ ## It does not use double-reduction proportion, need to double-check formula on kerr 2012 for higher ploidies...
    listA <- list()
    cat(paste("Constructing matrix A using ploidy =",ploidy,"and proportion of double reduction =",w,";as in Kerr et al. (2012) \n"))
    start.time <- Sys.time()
    v = ploidy/2
    A[1,1] <- (1)/(2*v)
    for( i in 2:n){
      ## Both are unknown
      if( s[i] == 0 && d[i] == 0 ){
        A[i,i] <- (1)/(2*v)
        for( j in 1:(i-1))
          A[j,i] <- A[i,j] <- 0
      }
      
      ## Sire is unknown
      if( s[i] == 0 && d[i] != 0 ){
        A[i,i] <- (1 + (v-1)*w + ((v-1)*(1-w)*(v*A[d[i],d[i]] + 1/2 - 1))/(2*v-1))/(2*v)
        for( j in 1:(i-1))
          A[j,i] <- A[i,j] <- 0.5*(A[j,d[i]])
      }
      
      ## Dire is unknown
      if( d[i] == 0 && s[i] != 0 ){
        A[i,i] <- (1 + (v-1)*w + ((v-1)*(1-w)*(v*A[s[i],s[i]] + 1/2 - 1))/(2*v-1))/(2*v)
        for( j in 1:(i-1))
          A[j,i] <- A[i,j] <- 0.5*(A[j,s[i]])
      }
      
      ## Both are known
      if( d[i] != 0 && s[i] != 0 ){
        A[i,i] <- (1  + (v-1)*w + ((v-1)*(1-w)*(v*A[d[i],d[i]] + v*A[s[i],s[i]] - 1)/(2*v-1)))/(2*v) + A[d[i],s[i]]/2
        for( j in 1:(i-1))
          A[j,i] <- A[i,j] <- 0.5*(A[j,s[i]]+A[j,d[i]])
      }
    }
    
    A <- 2*v*A
  }
  
  NA.errors <- which(is.na(A))
  if( length(NA.errors) > 0 )
    cat("Please verify your original data with the function 'verifyped', there are some data missing/conflicting data \n")
    
  Time = as.matrix(proc.time()-Time)
  cat("Completed! Time =", Time[3]/60," minutes \n")
#      "Visualization options: (matrix, w) \n ")
  rownames(A) <- colnames(A) <- data$ind.data

  A <- A[orig.order,orig.order]
  
  
  if (ASV) {
    A = get_ASV(A)
  }
  
  return(A)
}
