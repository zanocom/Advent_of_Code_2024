############################################
# ADVENT OF CODE 2024
# Day 2
# 2024/12/14

# https://adventofcode.com/2024/day/2
############################################

# a report only counts as safe if both of the following are true:
# The levels are either all increasing or all decreasing.
# Any two adjacent levels differ by at least one and at most three.
############################################



library( stringr )
library( dplyr )

    
    readLines( 'input_day_2.txt' , 3 )
    
    input_data <- 
      read.table(  'input_day_2.txt' , 
                   sep = '\t' ,
                   header=F ,
                   row.names = NULL )
    
    sample_data <- 
      read.table(  'input_day_2_sample.txt' , 
                   sep = '\t' ,
                   header=F ,
                   row.names = NULL )



    input_data_text <- lapply( input_data[,1] ,  str_split , "\\s+" ) 
    input_data_numeric <- lapply( input_data_text ,  function(x) as.numeric( x [[1]] )  ) 
    

    is_safe <- function(x) {
      # takes input a numeric vectors
      
      check_safe <- FALSE
      # get row from input dataframe
      # split text by space
      # convert test to numbers
      #x <-  sample_data[2,] 
      #x_numeric <- as.numeric( x [[1]] )
      
      # compute difference of elements
      x_diff <- diff( x )
      
      n <- length( x_diff )
      x_diff_sum <-  abs( sum( sign(x_diff )) )
      
      # if elements are all increasing or decreasing then sum of sings
      # is equal to elements count
      
      if(  x_diff_sum == n &
           min( abs(x_diff ) ) >= 1 &
           max( abs(x_diff ) ) <= 3
           ){
              check_safe <- TRUE 
            }
      
      return( check_safe )
    }


    
    # How many reports are safe?
    sum( unlist( lapply( input_data_numeric , is_safe  ) ))
   
   
  
  ###########################################################
  # PART 2
  # Update your analysis by handling situations where the Problem Dampener 
  # can remove a single level from unsafe reports. 
  # How many reports are now safe?
  
  
  # For each numeric vector iterate and remove a single number
  # the check whether is safe
  
  
 dampened_safe <- function(x)  {
    
   #input_data_numeric[[100]]
   
    # get each element from input data list
    #i <- 100
    x_numeric <-  x 
    check_safe <- FALSE
    
        for( k in 1:length(x_numeric) ) {
          # iterate over numeric vector
          # remove each element
         
           
               if(  !check_safe  ){
              # we update the check_safe flag 
              # until we find  a safe combination 
                check_safe <-  is_safe ( x_numeric[-k] )
              }
      
          }
            return( check_safe )
    }
    
    
    
  
  
    sum( unlist( lapply( input_data_numeric , dampened_safe  ) ))

    
  

############################################
############################################