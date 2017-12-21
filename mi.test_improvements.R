MMI.test.modified <- function(data, I, J, add.constant = .5){
  n <- nrow(data)
  if(I != 1L){
    # reoder data columns and place single categorical variable on the first place
    setcolorder(data, c(names(data)[ncol(data)], names(data)[-ncol(data)]))
    J <- I
  }
  if (!all(data[, 2:(J + 1)] == 1L | data[, 2:(J + 1)] == 0L)){
    stop("not all values of the multiple responses variable are equal to 1L or 0L.")
  }
    
  singleVariableName <- copy(names(data)[1])
  setnames(data, 1, "singleVariable")
  setkeyv(data,names(data))
  n_margins <- data[,list(count = .N), by = singleVariable]
  r <- nrow(n_margins)
  if (r == 1){
    print("The single categorical variable takes only one value")
    return(NULL)
  }
  
  A.i <- n_margins$count/n
  N <- data[, list(freq = .N), by = key(data)] #Get all non-zeros combos of single variable and Y's
  
  # Find counts of each each category of the multiple variable across categories of the single variable
  Totals <- N[, lapply(.SD, function(x) sum(x*freq)), by = singleVariable, .SDcols = 1:J + 1]
  
  # Remove from Totals column with single categories values and convert it to the matrix
  Totals <- as.matrix(Totals[, singleVariable := NULL])
  Pi.not.j <- colSums(Totals)/n
  
  ### We should exclude categories that are take only one value (0 or 1) for all respondents.
  ### Otherwise conjugacy table between such category and single categorical variable would be have size r x 1
  ### Those tables are invalid for MMI test and Pearson's chi-squared statistic are not calculated for them  
  excluded.categories <- which(Pi.not.j == 0 | Pi.not.j == 1)
  if (length(excluded.categories) > 0){
    ### update the number of categories of the multiple-response variable
    J = J - length(excluded.categories)
    
    if (J == 0){
      print("Each category of the multiple categorical variable takes only one value (0 or 1)")
      return(NULL)
    }
    
    ### remove categories that should be excluded
    N[, names(excluded.categories) := NULL]
    setkey(N, singleVariable)
    ## remove excluded categories from marginal table
    Totals <- Totals[, -excluded.categories]
    ## remove unaccepted probabilities
    Pi.not.j <- Pi.not.j[-excluded.categories]
    ### and go further
  }
  
  if (J == 1){
    ### this is case of ordinary r x 2 table, we should apply usual Pearson's chi-squared test
    tab <- table(data)
    tab[tab == 0] <- add.constant
    suppressWarnings(chisq.test.results <- chisq.test(tab, correct = FALSE))
    return(list(X.sq.S.ij  = chisq.test.results$statistic,
                X.sq.S.rs2 = chisq.test.results$statistic, 
                df.rs2 = chisq.test.results$parameter,
                p.value.rs2 = chisq.test.results$p.value))
  }
 
  N <- N[n_margins]
  Tau <- N[, freq/count]
  Tau.indexes <- c(0, N[, list(cases = .N), by = singleVariable][,cases])
  Tau.indexes <- cumsum(Tau.indexes)
  N <- as.matrix(N[, c("singleVariable", "freq", "count") := NULL])
  
  H <- kronecker(t(diag(r) - A.i), diag(ncol(N)))
  GVG <- bdiag(lapply(1:r, function(i){
                             Tau.part <- Tau[(Tau.indexes[i] +1):Tau.indexes[i+1]]
                               if (length(Tau.part) > 1){
                                  G.part <- N[(Tau.indexes[i] +1):Tau.indexes[i+1],]
                                  crossprod(G.part, (diag(Tau.part) - tcrossprod(Tau.part))) %*% G.part/A.i[i]
                                }
                                else{
                                  Matrix(0, 2, 2)
                                }
                            }
                      )
               )
  
  Di <- Diagonal(x = t(outer(A.i, Pi.not.j*(1-Pi.not.j), FUN = "/")))
  Di.HGVGH <- Di%*%H%*%tcrossprod(GVG, H)
  Di.HGVGH.eigen<-Re(eigen(Di.HGVGH, only.values = TRUE)$values)
  sum.Di.HGVGH.eigen.sq<-sum(Di.HGVGH.eigen^2)
  
  chisq.values <- function(x){
    nn <- n_margins$count
    adjusted.cells <- x == 0
    adjusted.cells.of.both.columns <- adjusted.cells | x == nn
    x[adjusted.cells] <- add.constant
    nn[adjusted.cells.of.both.columns] <- nn[adjusted.cells.of.both.columns] + add.constant
    E <- nn*sum(x)/sum(nn)
    return(statistics = sum((x - E)^2 * (1/E + 1/(nn - E))))
  }
  
  observed.Chi.sq <- apply(Totals, 2, chisq.values)
  
  X.sq.S.rs2 <- (r-1)*J*sum(observed.Chi.sq)/sum.Di.HGVGH.eigen.sq 
  df.rs2 <- (r-1)^2*J^2/sum.Di.HGVGH.eigen.sq       
  X.sq.S.p.value.rs2 <- pchisq(q = X.sq.S.rs2, df = df.rs2, lower.tail = FALSE)
  
  # if the data columns were reodered, return the original columns order
  if (I != 1L){
    setcolorder(data, c(names(data)[-1], "singleVariable"))
  }
  # return original name of the single categorical variable
  setnames(data, "singleVariable", singleVariableName)
  
  
  return(list(X.sq.S.ij = observed.Chi.sq,
              X.sq.S.rs2 = X.sq.S.rs2, 
              df.rs2 = df.rs2,
              p.value.rs2 = X.sq.S.p.value.rs2))
}

SPMI.test.modified <- function(data, I, J, add.constant = .5){
  if (! all(data == 1L | data == 0L)){
    stop("not all values of the input data are equal to 1L or 0L.")
  }
  n <- nrow(data)
  setkeyv(data, names(data))
  N <- data[,list(freq = .N), by = key(data)] #Get all non-zeros combos of W's and Y's
  N.freq <- N[, freq] # store the frequencies in a separate vector
  N <- as.matrix(N[, freq := NULL])
  excluded.categories <- apply(N, 2, function(x) length(unique(x))) == 1
  if (sum(excluded.categories) > 0){
    I <- I - sum(head(excluded.categories, I))
    J <- J - sum(tail(excluded.categories, J))
    if (I == 0 | J == 0){
      print("Each category at least one of the multiple categorical variables takes only single value (0 or 1)")
      return(NULL)
    }
    if (I == 1 | J == 1){
      ### for such case we should apply MMI.test
      return(MMI.test.modified(data[, which(! excluded.categories), with = FALSE], I, J, add.constant))
    }
    N <- N[,!excluded.categories]
  }
  
  Tau <- N.freq/n
  GH <- apply(N, 1, function(x) x[1:I] %x% x[(1:J) + I])
  Totals <- apply(N, 2, function(x) sum(x*N.freq)) # totals for W and Y categories
  T.row <- Totals[1:I]   # totals for W categories
  T.col <- Totals[1:J + I] # totals for Y categories
  Pi.row <- T.row/n #pi_i.
  Pi.col <- T.col/n #pi_.j 
  
  # G.ij is t(N[,1:I]) and H.ji is t(N[,(1+I):(I+J)])
  F <- GH - kronecker(Pi.row, t(N[,1:J + I])) - kronecker(t(N[,1:I]), Pi.col)
  
  Mult.cov <- diag(Tau) - tcrossprod(Tau, Tau)
  sigma <- F%*%tcrossprod(Mult.cov, F)
  D.inv <- diag(1/as.vector(kronecker(Pi.row, Pi.col)*kronecker(1-Pi.row, 1-Pi.col)))
  Di.sigma <- D.inv%*%sigma
  Di.sigma.eigen <- Re(eigen(Di.sigma, only.values = TRUE)$values) 
  sum.Di.sigma.eigen.sq <- sum(Di.sigma.eigen^2)
  
  chisq.values <- function(observed.cells){
    m.rows <- matrix(T.row, nrow = I, ncol = J, byrow = FALSE) 
    m.cols <- matrix(T.col, nrow = I, ncol = J, byrow = TRUE)
    totals <- matrix(n, I, J)
    
    apply.adjst.1 <- observed.cells == 0
    apply.adjst.2 <- observed.cells == m.rows 
    apply.adjst.3 <- observed.cells == m.cols
    apply.adjst.4 <- m.rows + m.cols - observed.cells == totals
    

    if (sum(apply.adjst.1) > 0){
      observed.cells[apply.adjst.1] <- add.constant
      m.rows[apply.adjst.1] <- m.rows[apply.adjst.1] + add.constant
      m.cols[apply.adjst.1] <- m.cols[apply.adjst.1] + add.constant
      totals[apply.adjst.1] <- totals[apply.adjst.1] + add.constant
    }
    if (sum(apply.adjst.2) > 0){
      m.rows[apply.adjst.2] <- m.rows[apply.adjst.2] + add.constant
      totals[apply.adjst.2] <- totals[apply.adjst.2] + add.constant
    }
    if (sum(apply.adjst.3) > 0){
      m.cols[apply.adjst.3] <- m.cols[apply.adjst.3] + add.constant
      totals[apply.adjst.3] <- totals[apply.adjst.3] + add.constant
    }
    if (sum(apply.adjst.4) > 0){
      totals[apply.adjst.4] <- totals[apply.adjst.4] + add.constant
    }
    
    E <- m.rows*m.cols/totals
    statistics <- (observed.cells - E)^2 * ( 1/E + 1/(m.rows - E) + 1/(m.cols - E) +
                                       1/(totals - m.rows - m.cols + E) )
    dimnames(statistics) <- list(names(T.row), names(T.col))

    return(statistics)
  }
  
  observed.Chi.sq <- chisq.values(matrix(GH%*%N.freq, nrow = I, byrow = TRUE))
  X.sq.S.rs2<-I*J*sum(observed.Chi.sq)/sum.Di.sigma.eigen.sq 
  df.rs2<-I^2*J^2/sum.Di.sigma.eigen.sq 
  X.sq.S.p.value.rs2 <- pchisq(q = X.sq.S.rs2, df = df.rs2, lower.tail = FALSE)
  
  return(list(X.sq.S.ij = observed.Chi.sq,
                X.sq.S.rs2 = X.sq.S.rs2, 
                df.rs2 = df.rs2,
                p.value.rs2 = X.sq.S.p.value.rs2))
}

multiple.chiRS2.test <- function(data, I, J, add.constant = .5){
  assertInt(I, lower = 1)
  assertInt(J, lower = 1)
  assertNumber(add.constant, lower = 0)
  assertDataTable(data, any.missing = FALSE, ncols = I+J)
  if (I == 1 | J == 1){
    MMI.test.modified(data, I, J, add.constant)
  }
  else{
    SPMI.test.modified(data, I, J, add.constant)
  }
}