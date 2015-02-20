## The function 'makeCacheMatrix' creates an object that contains a matrix and its inverse.
## On object creation, the inversed matrix is not yet available currently NULL. 
## The inversed matrix will become available and cached only after the first call of the function 'cacheSolve'
makeCacheMatrix <- function(mtx = matrix()) {
    mtx_inversed_cache <- NULL
    set_matrix <- function(y) {
        mtx <<- y
        mtx_inversed_cache <<- NULL
    }
    
    get_matrix <- function() mtx
    set_cache <- function(mtx_inversed) mtx_inversed_cache <<- mtx_inversed
    get_cache <- function() mtx_inversed_cache
    list(set_matrix = set_matrix, get_matrix = get_matrix, set_cache = set_cache, get_cache = get_cache)
}


## The function 'cacheSolve' will inverse the matrix x passed as argument.
## It will first check if the object x has already the inversed matrix already calculated and cached.
## If there's no value already cached in the object, the function will calculate the inverse of x and store it on cache.

cacheSolve <- function(mtx, ...) {
    mtx_inversed <- mtx$get_cache()
    if(!is.null(mtx_inversed)) {
        message("Getting inversed matrix from cache")
        return(mtx_inversed)
    }
    data <- mtx$get_matrix()
    mtx_inversed <- solve(data, ...)
    mtx$set_cache(mtx_inversed)
    mtx_inversed
}
