## makeCacheMatrix() and cacheSolve() are functions used to store an inverted matrix
## in the cache to be able to call the inverted matrix without recomputing it several 
## times so that computing time can be saved.

## makeCacheMatrix() creates a special matrix object that can cache it and its inverse.
## A list of 4 funtions is returned.

makeCacheMatrix <- function(X = matrix()){
     
     InvMat <- NULL                #creates an empty local object (InvMat) intended 
                                   #to receive the inverted matrix in the cache
     
     set <<- function(Y){          #function that assigns the original 
          X <<- Y                  #matrix into the cache
          InvMat <<- NULL          #creates an empty cache object for the 
     }                             #inverted matrix
     
     get <- function() X           #function to return the original matrix
     
     setinv <- function(inverse) InvMat <<- inverse    #function to store inverse
                                   
     getinv <- function() InvMat   #function to return inverted matrix
     
     list(set = set,               #list of function associate with original 
          get = get,               #matrix and its inververted matrix in cache
          setinv = setinv, 
          getinv = getinv)
}

## Computes and returns the inverse of the special matrix created by makeCacheMatrix.
## If the inverted matrix has already been calculated, then the cachesolve() function  
## should retrieve the inverted matrix from the cache.

cacheSolve <- function(X, ...){
     
     InvMat <- X$getinv()          #gets the value for the inverted matrix stored 
                                   #in the cache's special matrix object
     
     if(!is.null(InvMat)) {                  #tests if inverted matrix was already
          message("Getting cached data...")  #computed and stored
          return(InvMat)                     #in cache
     }
     
     Mat <- X$get()                #gets the original matrix for inverting
     
     InvMat <- solve(Mat, ...)     #computes matrix invertion
     
     X$setinv(InvMat)              #stores the inverted matrix into cache's 
                                   #special matrix
     
     InvMat                        #returns inverted matrix
     
}

