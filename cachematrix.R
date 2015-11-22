#First of all the MASS package needs to be installed and loaded in order
#to be able to inverse matrixes which are not squre matrix since the 'solve()' 
#function is only able to inverse square matrixes and the 'ginv()' function in
#previously mentioned package able to do handle this issue via Moore-Penrose
#Generalized Inverse method. The assigment states to use solve() so just change
#in line #47 the 'ginv' to 'solve' if it bothers.

install.packages("MASS")
library(MASS)

#The first function, `makeCacheMatrix` creates a special "matrix", which is
#really a list containing a function to

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse matrix
#4.  get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
} 

#The following function makes inverse of the matrix created with the
#above function. However, it first checks to see if the matrix has an
#inverse already. If so, it get`s the inverse matrix from the
#cache and skips the computation. Otherwise, it makes the inverse matrix
#and sets the values in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- ginv(data, ...)
    x$setinverse(inv)
    inv
}