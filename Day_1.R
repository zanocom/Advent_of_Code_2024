############################################
# ADVENT OF CODE 2024
# Day 1
# 2024/12/09

# https://adventofcode.com/2024/day/1
############################################

  library( stringr )
 library( dplyr )
  input_data <- readLines( 'input.txt' , 3 )

  input_data <- 
  read.table(  'input.txt' , 
               sep = '\t' ,
               header=F ,
               row.names = NULL )
 
  
  
  # remove spaces
  input_data <- unlist(sapply(  input_data  ,   str_split , "\\s+"  ))
  
  # separate even and odd locations_ids
  df_location_id <- 
    data.frame( id_1 = sort( input_data[ seq(1 , 2000 , by = 2) ] ) , 
                id_2= sort( input_data[ seq(2 , 2000 , by = 2)  ] ) )
  
  
  df_location_id <- 
    df_location_id %>% 
    mutate_all( as.numeric )
  
  
  df_location_id <- 
    df_location_id %>% 
     mutate( dist= abs( id_1- id_2  ) )  
  
  sum( df_location_id$dist )
  
  
############################################
  # This time, you'll need to figure out exactly 
  # how often each number from the left list appears in the right list. 
  # Calculate a total similarity score by adding up each number in the left list 
  # after multiplying it by the number of times that number appears in the right list.

  location_matches <- function(x)  sum(  df_location_id$id_2 %in% x ) 
  
  
  df_location_id$match_nr <-  sapply( df_location_id$id_1  , location_matches )
  
  
  df_location_id %>% 
    mutate( sim_score= id_1 * match_nr  ) %>% 
    summarise( sum(sim_score ))
  
  
############################################