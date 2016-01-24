makeVector <- function(z = numeric()) {
    m <- NULL
    set <- function(y) {
        z <<- y
        m <<- NULL
    }
    get <- function() z
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(z, ...) {
    m <- z$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- z$get()
    m <- mean(data, ...)
    z$setmean(m)
    m
}