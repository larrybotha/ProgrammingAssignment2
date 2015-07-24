# create a matrix object, and initialise it right away,
# before returning its public methods
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  m <- NULL

  init_ <- function(x) {
    set_(x)
  }

  get_ <- function() m

  set_ <- function(val) m <<- val

  getInverse_ <- function() i

  setInverse_ <- function(inv) i <<- inv

  init_(x)

  list(
    get = get_,
    set = set_,
    getInverse = getInverse_,
    setInverse = setInverse_
  )
}

# get the inverse of our matrix object above, caching it
# if it hasn't yet been cached
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()

  if (is.null(inv)) {
    inv <- x$setInverse(solve(x$get()))
  }

  inv
}
