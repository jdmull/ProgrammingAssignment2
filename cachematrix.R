## cashematrix module - Create matrix inverses, cache results
 
library(MASS)  ## library for function that calculates a general matrix inverse

## Create a matrix object that can cache its inverse
makeCacheMatrix <- function(thisMatrix = matrix()) {
    cachedInverse<-NULL

    ## property accessors, allowing get & set on origional matrix and inverse matrix
    set <- function(matrixToAssign){
        thisMatrix <<- matrixToAssign
        cachedInverse <<-NULL
    }
    get <- function(){ thisMatrix }
    setInverse <- function(inverse){ cachedInverse <<- inverse }
    getInverse <- function(){ cachedInverse }
  
    list(set = set, get = get, setInverse = setInverse, getInverse=getInverse)
}


## compute inverse of the matrix returned by makeCaseMatrix
cacheSolve <- function(x) {    ## Return a matrix that is the inverse of 'x'
  inverse <-x$getInverse()
  if (is.null(inverse)){  ## If there was no cached inverse, calculate & cache it
    xMatrix<-x$get()
    inverse<-ginv(xMatrix)  ## calculation of inverse matrix
    x$setInverse(inverse)   ## saving result
  }
  inverse;
}
