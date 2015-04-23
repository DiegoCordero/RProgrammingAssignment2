## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix creates a special "matrix" object that can cache its inverse, so it basically creates the functions that assign/retrieve the inverse of the matrix either if it had been calculated before or not.

## Write a short comment describing this function

##The function receives as parameter the matrix for which the inverse is going to be calculated.
##At first it defines m as the variable that will be returned with the inverse. It is null within the function environment.
##The set function sets the value of the matrix, and the get function returns the matrix.
##setinv sets the inverse of the matrix to the m variable in the global environment, while getinv returns the inverse.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getinv()		#assigns the inverse (whether it exists already or not) to the variable that will be returned
        if(!is.null(m)) {	#it checks if the inverse calculated or not, and returns the cached value if it was calculated already, indicating that the value comes from the cache
                message("getting cached data")
                return(m)
        }
        data <- x$get()		#if it gets to this point it's because there was not a cached value, so it will calculate the inverse. This steps stores the matrix in the variable named 'data'
        m <- solve(data, ...) #calculates the inverse over the matrix stored in 'data'
        x$setinv(m)		#stores the inverse in the global enviroment, using the setinv() function defined in makeCacheMatrix()
        m				#returns the inverse of the matrix after it was calculated for the first time
}
