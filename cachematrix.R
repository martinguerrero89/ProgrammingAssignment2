## Set of two functions that calculates the inverse of a square matrix or directly returns the result if it was previously calculated


###THE FUNCTION makeCacheMatrix will set the functions "set","get","setinversion", "getinversion" for a squeare matrix(m)

makeCacheMatrix <- function(m= matrix()){
        inverse<- NULL
        set <- function(y){             # this function "sets" the matrix (y) we are going to work with, even though we don't use this function in the example we can use it to call it later...
                m<<-y                      # The argument "y" is set to "m" and is assigned to the parent enviorment of the function at a higher level
                inverse<<- NULL
        }
        get <- function() m        #HERE "m" is called from the parent function and is assigned to the function "get"
        setinversion <- function(inv) inverse <<- inv   # The argument "inv" of the function is set to "inverse" and is cached to the parent enviorment of the function
        getinversion <- function() inverse              #Call the argument "inverse" from the parent enviorment.
        list(set = set, get = get,          #Lists the functions "set", "get", "setinversion" and "getinversion" that are going to be used in the next function
                setinversion = setinversion,
                        getinversion = getinversion)
}


### THIS FUNCTION returns a matrix that is the inverse of "m" (previously used in the function makeCacheMatrix(m))
cacheSolve <- function(x, ...) {
        inverse <- x$getinversion()                             #Assign the "getinversion" function from the function x ("makeCacheMatrix" in this case) to the object "inverse"
        if(!is.null(inverse)) {                                 # If "Inverse" was previously calculated get the value of it and send the message "getting cached inversed matrix"
                message("getting cached inversed matrix")
                return(inverse)
        }                                                       
        data <- x$get()                 #If the inverse of the matrix was not calculated get the matrix data from the function "get" in x (makeCacheMatrix) and solve it with the function "Solve"
        inverse <- solve(data, ...)
        x$setinversion(inverse)         # Once "inverse" is calculeted call the function "setinversion" from x (makeCacheMatrix) that is going to assing this value to the enviroment of the function
        inverse
}

### THIS IS AN EXAMPLE

#a <- makeCacheMatrix( matrix(rnorm(4), nrow = 2, ncol = 2) );
#a$get();
#       [,1]        [,2]
#[1,] 0.2390467 -0.01198477
#[2,] 0.4257483 -0.04955568
# cacheSolve(a)
#       [,1]     [,2]
#[1,]  7.34852  -1.7772
#[2,] 63.13342 -35.4478
#cacheSolve(a);
#"getting cached inversed matrix"
#       [,1]     [,2]
#[1,]  7.34852  -1.7772
#[2,] 63.13342 -35.4478
> 