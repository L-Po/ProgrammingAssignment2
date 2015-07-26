## These two functions were created to avoid repeating
## computation if they were done before. They check whether
## it is the case or not. If it is, they display the cached result
## and if it is not, they perform the computation.
## In our case it concerns inverting matrices. 


## This function returns a list of the following actions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverted matrix
## 4. get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function checks whether the computation of the 
## inverted matrix has been already done. If it is the case
## and if the matrix has not been modified, it returns the 
## cached result with the additional message. Otherwise,
## it calculates the inverted matrix itself.

cacheSolve <- function(x, ...) {
		m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
