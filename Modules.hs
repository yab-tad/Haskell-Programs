import Data.List

-- number of unique elements in a list
numUnq :: Eq a => [a] -> Int
numUnq = length . nub  --given input [1,1,3,2,4,2,1,4] it returns 4.

{-
    If you just need a couple of functions from a module, you can selectively import just 
    those functions. If we wanted to import only the nub and sort functions from Data.List, 
    we'd do this:
                
                import Data.List (nub, sort)


    You can also choose to import all of the functions of a module except a few select ones. 
    That's often useful when several modules export functions with the same name and you want 
    to get rid of the offending ones. Say we already have our own function that's called nub 
    and we want to import all the functions from Data.List except the nub function:
                
                import Data.List hiding (nub)

    Another way to counter name clashes is to use |qualified imports|. For example, when 
    we import Data.Map and try to use the filter function, it gets confused as to which 
    filter to use since the function's name from the Data.Map is the same as the Prelude 
    module. A solution for this looks like qualifying the import and calling the function 
    by appending the function we want with the imported module.
    This looks like the following:

                import qualified Data.Map
        
        Calling the function filter from the imported module now would look like: Data.Map.filter


    Assigning shorthand notations for the modules we import can further help create a 
    smoothe process. It specially comes very handy when a cascading import is at play:

                import qualified Data.Map as M

        Now, things become a lot simpler and clearer: M.filter

-}



