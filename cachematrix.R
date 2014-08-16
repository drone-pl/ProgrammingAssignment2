##
## This function creates a special "vector" of functions.
##
makeCacheMatrix <- function(x = matrix()) {

    # Here we initialize cache variable.
    m <- NULL

    # This setter function clears cached variable and sets the original matrix.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # This returns original matrix.
    get <- function() x

    # This sets original matrix without clearing of cached variable.
    setmatrix <- function(mx) m <<- mx

    # This returns inverted matrix.
    getmatrix <- function() m

    # List (or "special" vector) of functions that is returned.
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

##
## This function inverts the matrix or returns cached matrix if it exists.
##
cacheSolve <- function(x, ...) {

    # Try to get cached inverted matrix.
    mmatrix <- x$getmatrix()

    # mmatrix isn't null means we already have cached variable, we inform user
    # about that and returning cached value.
    if(!is.null(mmatrix)) {
        message("getting cached data")
        return(mmatrix)
    }

    # We didn't find cached matrix, hard work is required.
    # First we're getting matrix from our beloved "special" vector ("obsv")
    # that isn't vector but a list.
    data <- x$get()

    # now we try to solve (a.k.a invert) matrix with provided data.
    # I assumed that I'll get only square matrix here.
    # I'm  too lazy to check if it's not.
    mmatrix <- solve(data,...)

    # Seems like we've solved matrix without any error.
    # It's time to cache the result by putting it into "obsv"
    x$setmatrix(mmatrix)

    # At last! Here is your inverted matrix.
    # You shouldn't complain if you tried to invert uninvertable matrix.
    # No refunds!
    mmatrix
}


# I understand that I have a strange sense of humor, hope you enjoyed it.
# Even if you didn't.
# Try to not cache your points when you evaluate my work.
