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

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()

  if (is.null(inv)) {
    inv <- x$setInverse(solve(x$get()))
  }

  inv
}
