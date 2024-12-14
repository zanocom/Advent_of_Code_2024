############################################
# ADVENT OF CODE 2024
# Day 3
# 2024/12/14

# https://adventofcode.com/2024/day/3
############################################

# a report only counts as safe if both of the following are true:
# The levels are either all increasing or all decreasing.
# Any two adjacent levels differ by at least one and at most three.
############################################



library( stringr )
library( dplyr )


readLines( 'input_day_3.txt' , 3 )

input_data <- readLines("input_day_3.txt") |>
  paste0(collapse = "")

  # read.table(  'input_day_3.txt' , 
  #              sep = '\t' ,
  #              header=F ,
  #              row.names = NULL 
  #              )

sample_data <- readLines("input_day_3_sample.txt") |>
  paste0(collapse = "")
  
  # 
  # read.table(  'input_day_3_sample.txt' , 
  #              sep = '\t' ,
  #              header=F ,
  #              row.names = NULL )

str( paste0(input_data$V1) )

# valid instructions are of type mul(number,number)

 fn_detect_and_multiply <- function(x) {
    x <- input_data
    selected_elements <- unlist(  str_extract_all( x , "mul\\([0-9]{1,3},[0-9]{1,3}\\)" ))

    # get number to be multiplied
    # each couple is a list
    list_of_numbers <- 
    selected_elements %>% 
      str_remove_all( "mul\\(" ) %>% 
      str_remove_all( "\\)") %>% 
      str_split( "," ) 
    
    list_of_numbers    <- lapply( list_of_numbers  , function(x)  as.numeric(x) )
    multiplied_numbers <- lapply( list_of_numbers  , function(x)  prod(x) )
    row_result         <- sum( unlist( multiplied_numbers ))
    
    return( row_result  )
    }
    
# What do you get if you add up all of the results of the multiplications?

  sum( sapply( sample_data$V1 , fn_detect_and_multiply  , USE.NAMES = F))

  sum( sapply( input_data$V1 , fn_detect_and_multiply  , USE.NAMES = F))
  
###################################################
  # PART 2
  # do and dont respectively enables and disables multiplications
  # sum only enabled multiplications
  
  
  sample_data <- readLines("input_day_3_sample_2.txt") |>
    paste0(collapse = "")
  
  
  x <- sample_data
  
  fn_enabled_text  <- function(x) {
        #x <- input_data
        #x <- sample_data
        index_do <- str_locate_all( x , "do" )[[1]][,1]
        index_dont <- str_locate_all( x , "don't" )[[1]][,1]  
        
        # do overlap with dont, so we remove from do indexx already in dont
        
        index_do <-  index_do [! index_do %in% index_dont]
        # add first position becausedo is by default
        index_do <- c( 0 , index_do  ) 
        
      
        
         enabled_intervals <- data.frame( id_do = numeric(length(index_do)) ,
                                         id_dont = numeric(length(index_do)) 
                                          )
        
        
        
        for( i in 1:length(index_do) ) {
          enabled_intervals$id_do[i] <- index_do[i]
          enabled_intervals$id_dont[i] <- min( index_dont[ index_dont > index_do[i] ] )
        }
        
        enabled_intervals$id_dont[ is.infinite( enabled_intervals$id_dont ) ] <- nchar( x )
        # until now we have duplicated values for id_dont
        # we need distinct intervals
        # enabled intervals are the ones with distinct id_donts
        
        enabled_intervals <- 
        enabled_intervals %>% 
          group_by( id_dont ) %>% 
          summarise( id_do = min( id_do)) %>% 
          select ( id_do , id_dont )
        
         
        
        selected_text <- character(0)
        
                  for( i in 1:nrow(enabled_intervals)) {
                    selected_text <- c(
                                      selected_text , 
                                      str_sub( x
                                               , start =  enabled_intervals$id_do[i] 
                                               , end = enabled_intervals$id_dont[i] 
                                               ) 
                                        )
                  }
        
        
        
        return( selected_text )
        }
  

     
   sum( sapply( fn_enabled_text(input_data) , fn_detect_and_multiply  , USE.NAMES = F))
   
  


############################################
############################################