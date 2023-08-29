
##------------- FUNCTIONS FOR MATRIX KINSHIP MODEL -----------------------------
## 
## 
##  Author: Benjamin Schlüter
##  Date: August 2023
##
##
## -----------------------------------------------------------------------------



## Load packages ---------------------------------------------------------------

packages <- c("tidyverse", "Matrix")
invisible( lapply(packages, library, character.only = TRUE))



## Functions -------------------------------------------------------------------

## Construct U matrix from a lifetable
get_U <- function(s, y, c, lifeTable) {
    
    ## Extract survival probs 
    px <- lifeTable %>% 
        filter(sex == s,
               year == y,
               ctry == c) %>% 
        pull(px)
    ## Dim of A
    omega <- length(px)
    ## Creation of U
    U <- matrix(0, 
                nrow = omega, 
                ncol = omega)
    ## Survival prob on subdiagonal
    U[row(U)-col(U)==1] <- head(px,-1) # ASSUMPTION: Everybody dies at last age
    
    return(U)
}

## Construct \hat{M} matrix
## Could add an argument for when to record 
## cause-specific mortality without assuming 
## uniformity
get_M_hat <- function(s, y, c, lifeTable, alpha) {
    
    ## Account for cause-specific mortality starting from 1990
    ## Assume before that, that each cause has the same share of death
    ## --> Useful to be able to compute death from all cause but
    ## carefull not to interpret these years as real cause-specific
    ## death
    ## Extract death probs
        qx <- lifeTable %>%
            filter(sex == s,
                   year == y,
                   ctry == c) %>%
            pull(qx)
        
        ## Dim of A
        omega <- length(qx)
        
        ## GBD data only available from 1990
        if (y < 1990) {
            ## Assume each cause has the same share of death
            ## when data on CoD is not available
            P <- matrix((1/alpha),
                        nrow = alpha,
                        ncol = omega)
        } else {
            
            ## Create prop death matrix, dim(H)=(alpha*omega)
            P <- lifeTable %>%
                filter(sex == s,
                       year == y,
                       ctry == c) %>%
                dplyr::select(starts_with("cause")) %>%
                as.matrix %>%
                t()
        }
        ## Multiply prop of death cause i by qx
        M <- P %*% diag(qx)
        
        ## Store columns of M as a list of vectors
        M.cols <- lapply(1:ncol(M), function(j) return(M[,j]))
        
        ## Create M_hat using the vectors as elements of the block diagonal
        M_hat <- bdiag(M.cols)
        
        
    
    return(M_hat)
}

## Construct U_tilde from U and M
get_U_tilde <- function(lifeTable, y, c, alpha, cum = F) {
    
    
    ## U for both sexes
    U <- lapply(c("Female", "Male"), get_U, y, c, lifeTable)
    
    ## M_hat for both sexes
    M_hat <- lapply(c("Female", "Male"), get_M_hat, y, c, lifeTable, alpha)
    
    ## Dimensions
    omega <- dim(M_hat[[1]])[2]
    alphaomega <- dim(M_hat[[1]])[1]
    
    ## Construct block matrix, block by block
    ## Upper-left block: survival probs.
    block_UL <- bdiag(U)
    ## Lower-left block: death probs. by cause
    block_LL <- bdiag(M_hat)
    ## Upper-right block: death can't become alive 
    zeros <- matrix(0,nrow = omega, ncol = alphaomega)
    block_UR <- bdiag(list(zeros, zeros))
    ## Lower-right block: 
    if (cum) {
        I <- diag(alphaomega)
        block_LR <- bdiag(list(I, I))
    } else {
        zeros <- matrix(0, nrow = alphaomega, ncol = alphaomega)
        block_LR <- bdiag(list(zeros, zeros))
    }
    ## Combine
    block_U <- cbind(block_UL, block_UR)
    block_L <- cbind(block_LL, block_LR)
    
    U_tilde <- rbind(block_U, block_L)
    
    return(as.matrix(U_tilde))
}

## Construct F matrix from fx
get_F <- function(s, y, c, ages, asfr) {
    
    omega <- length(ages)
    ## Extract asfr
    fx <- asfr %>% 
        filter(sex == s,
               year == y,
               ctry == c) %>% 
        pull(fx)
    ## Creation of F
    F. <- matrix(0, 
                 nrow = omega, 
                 ncol = omega)
    ## ASFR on 1st row
    F.[1, ] <- fx 
    
    return(F.)
}

## Dist. of the ages of the parents of offspring 
## NOTE: Using norm() for 1-norm does not lead to 
##      take the sum of the absolute values of a vector.
##      -> We compute it manually
get_pi <- function(ages, asfr, lifeTable, y, c) {
    
    
    PI <- sapply(c("Female", "Male"), function(s) {
        
        ## Fertility matrix
        ## !! Using female fx for both sexes !!
        F. <- get_F("Female", y, c, ages, asfr)
        ## Population vector
        z <- lifeTable %>% 
            filter(sex == s,
                   year == y,
                   ctry == c) %>% 
            pull(pop)
        ## Population age structure
        z <- z/sum(z)
        ## Compute distribution of ages at offspring
        num <- t(F.[1, ]) * z
        denom <- sum( abs(num) )
        pi <- num / denom
        
        return(pi)
    },
    simplify = T,
    USE.NAMES = T)
    
    return(c(PI))
}

