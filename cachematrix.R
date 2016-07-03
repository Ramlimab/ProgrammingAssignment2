## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix that caches its inverse
## the set function creates the matrix by inputting a valid matrix
## the get function returns the matrix
## the setinv function will set the value of the inverse
## the getinv function returns the inverse value

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## this function returns the inverse of a matrix
## but checks first whether the inverse has already been stored

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

testmatrix<-matrix(c(2,3,4,1,2,-3,5,6,7),3,3)

##initialize matrix, set to testmatrix and return its value
a<-makeCacheMatrix()
a$set(testmatrix)
a$get()

## this returns NULL
a$getinv()

##Solving the matrix
cacheSolve(a)

##now it returns the inverse
a$getinv()

##now it retrives from the cache and returns the inverse
cacheSolve(a)


