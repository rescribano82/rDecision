#' Laplace criteria
#' @param m matrix with strategies in rows and states in columns. Benefits in 
#' positive and costs in negative
#' @return A list with best Laplace criteria decision and means vector
#' @author Ruben Escribano-Garcia \email{escribano.engineer@gmail.com}
#' @examples
#' #
#' m <- data.frame(c(200, 260, 272), 
#'                 c(300, 320, 280), 
#'                 c(340, 320, 300))
#' dimnames(m) <- list(LETTERS[1:nrow(m)], 
#'                     paste0('State_', as.roman(1:ncol(m))))
#' dec.laplace(m)
#' @export
dec.laplace <- function(m) {
   x <- apply(m, 1, mean)
   d <- data.frame(which.max(x), rownames(m)[which.max(x)], max(x))
   dimnames(d) <- list('', c('strategyNumber', 'strategyName', 'value'))
   return(list(decision=d, means=x))
}


#' Optimistic criteria
#' @param m matrix with strategies in rows and states in columns. Benefits in 
#' positive and costs in negative
#' @return A list with best optimistic criteria decision and maximums vector
#' @author Ruben Escribano-Garcia \email{escribano.engineer@gmail.com}
#' @examples
#' #
#' m <- data.frame(c(200, 260, 272), 
#'                 c(300, 320, 280), 
#'                 c(340, 320, 300))
#' dimnames(m) <- list(LETTERS[1:nrow(m)], 
#'                     paste0('State_', as.roman(1:ncol(m))))
#' dec.optimistic(m)
#' @export
dec.optimistic <- function(m) {
   x <- apply(m, 1, max)
   d <- data.frame(which.max(x), rownames(m)[which.max(x)], max(x))
   dimnames(d) <- list('', c('strategyNumber', 'strategyName', 'value'))
   return(list(decision=d, maximums=x))
}


#' Pesimistic criteria
#' @param m matrix with strategies in rows and states in columns. Benefits in 
#' positive and costs in negative
#' @return A list with best pesimistic criteria decision and minimums vector
#' @author Ruben Escribano-Garcia \email{escribano.engineer@gmail.com}
#' @examples
#' #
#' m <- data.frame(c(200, 260, 272), 
#'                 c(300, 320, 280), 
#'                 c(340, 320, 300))
#' dimnames(m) <- list(LETTERS[1:nrow(m)], 
#'                     paste0('State_', as.roman(1:ncol(m))))
#' dec.pesimistic(m)
#' @export
dec.pesimistic <- function(m) {
   x <- apply(m, 1, min)
   d <- data.frame(which.max(x), rownames(m)[which.max(x)], max(x))
   dimnames(d) <- list('', c('strategyNumber', 'strategyName', 'value'))
   return(list(decision=d, minimums=x))
}


#' Hurwicz criteria
#' @param m matrix with strategies in rows and states in columns. Benefits in 
#' positive and costs in negative
#' @param a index of optimistic (between 0 and 1). Default: 0.5
#' @return A list with best Hurwicz criteria decision and calculation matrix
#' @author Ruben Escribano-Garcia \email{escribano.engineer@gmail.com}
#' @examples
#' #
#' m <- data.frame(c(200, 260, 272), 
#'                 c(300, 320, 280), 
#'                 c(340, 320, 300))
#' dimnames(m) <- list(LETTERS[1:nrow(m)], 
#'                     paste0('State_', as.roman(1:ncol(m))))
#' dec.hurwicz(m, a=0.6)
#' @export
dec.hurwicz <- function(m, a=0.5) {
   x1 <- apply(m, 1, max)*a
   x2 <- apply(m, 1, min)*(1-a)
   
   x  <- x1*a + x2*(1-a)
   d  <- data.frame(which.max(x), rownames(m)[which.max(x)], max(x))
   dimnames(d) <- list('', c('strategyNumber', 'strategyName', 'value'))
   
   calcs <- cbind(max=x1, min=x2, weightedMean=x)
   return(list(decision=d, calculations=calcs))
}


#' Savage criteria
#' @param m matrix with strategies in rows and states in columns. Benefits in 
#' positive and costs in negative
#' @return A list with best Savage criteria decision and regrets matrix
#' @author Ruben Escribano-Garcia \email{escribano.engineer@gmail.com}
#' @examples
#' #
#' m <- data.frame(c(200, 260, 272), 
#'                 c(300, 320, 280), 
#'                 c(340, 320, 300))
#' dimnames(m) <- list(LETTERS[1:nrow(m)], 
#'                     paste0('State_', as.roman(1:ncol(m))))
#' dec.savage(m)
#' @export
dec.savage <- function(m) {
   reg <- apply(m, 2, function(x) abs(x-max(x)) )                            # Regret's matrix
   x   <- as.numeric(apply(reg, 1, max))
   d   <- data.frame(which.min(x), rownames(m)[which.min(x)], min(x))
   dimnames(d) <- list('', c('strategyNumber', 'strategyName', 'value'))
   return(list(decision=d, regrets=cbind(reg, weightedMean=x)))
}


#' Summary of all criterias
#' @param m matrix with strategies in rows and states in columns. Benefits in 
#' positive and costs in negative
#' @param a index of optimistic for Hurwicz criteria (between 0 and 1). Default: 0.5
#' @return A list matrix with best decisions for all criterias
#' @author Ruben Escribano-Garcia \email{escribano.engineer@gmail.com}
#' @examples
#' #
#' m <- data.frame(c(200, 260, 272), 
#'                 c(300, 320, 280), 
#'                 c(340, 320, 300))
#' dimnames(m) <- list(LETTERS[1:nrow(m)], 
#'                     paste0('State_', as.roman(1:ncol(m))))
#' decs.summary(m, a=0.6)
#' @export
decs.summary <- function(m, a=0.5) {
   decs <- rbind(Laplace    = dec.laplace(m)[[1]], 
                 Optimistic = dec.optimistic(m)[[1]], 
                 Pesimistic = dec.pesimistic(m)[[1]], 
                 Hurwicz    = dec.hurwicz(m, a)[[1]], 
                 Savage     = dec.savage(m)[[1]])
   return(decs)
}

