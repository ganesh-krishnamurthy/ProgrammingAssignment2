###################################################################
## cachematrix.R: This program was written as part of the submission #
## requirements of the Coursera - R Programming course. The program #
## below shows of the object oriented (class, constructors, set/get #
## functionality, object of a class) abilities of R. It is achieved #
## through lexical scoping and the "<<-" super-assignment operator. #
## In this example, if the inverse of a matrix is already available, #
## then instead of calculating the inverse again, the cached results #
## from a previous call are returned.                             #
## Creation date: 05/14/2015.                                     #
## Author: Ganesh Krishnamurthy                                   #
###################################################################


##############################################################
## This function constructs a matrix and creates a set of functions #
## to access this matrix element. The return value is a function list #
## in which the individual functions can be used to access/retrieve #
## the matrix element, i.e., similar to public functions and private #
## variables in object oriented programming. #
#############################################################

makeCacheMatrix <- function(x = matrix())
{
    # Declare a variable for the inverse of the matrix
    # and set it to null
    inv_x<-NULL  
    
    # Declare a "set" function to take an assignment for the x matrix
    set<-function(y)
    {
          x<<-y
          # Set the inverse to null whenever the x matrix is set
          inv_x<<-NULL
    }
    
    # A "get" function to retrieve the x matrix
    get<-function()
    {
          return(x)
    }
    
    # A "set" function to assign value to the inverse_x matrix
    setInverse<-function(inv)
    {
          inv_x<<-inv
    }
    
    # A "get" function for the inverse_x matrix
    getInverse<-function()
    {
          return(inv_x)
    }
    
    # Return value for this construction - a list of set/get functions
    # Note: The actual elements x and inv_x are kept private
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


###############################################################
## This function performs a Matrix inverse. It first checks to see if #
## the answer already exists and is so, just returns the value from cache. #
## If the (answer) inverse element doesn't exist, it then performs a #
## matrix inverse using the solve() function. #
###############################################################

cacheSolve <- function(x, ...) 
{
    # Step 1: Retrieve the inverse element
    inv<-x$getInverse()
    
    # Step 2: Check if the retrieved inverse element is NULL. If not
    # NULL, then don't need to solve() again. It returns the cached
    # answer
    if(!is.null(inv))
    {
        message("Returning cached inverse")
        return(inv)
    }
    
    # Step 3: If the retrieved inverse element is null, then calculate it
    # First, get the raw X matrix
    mat<-x$get()
    inv<-solve(mat,...)
    
    # Step 4: Update the cached inverse element
    x$setInverse(inv)
}