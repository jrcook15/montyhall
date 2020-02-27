#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title 
#' Select a door.
#' 
#' @description
#' `select_door()` selects one of the three door options available to the player.
#' 
#' @details
#'  Like the Monty Hall game show the contestant is asked to pick a door.
#'  
#' @param ... no arguments are used by the function.
#' 
#' @return  The function returns a length 3 character vector
#'   indicating the door the contestant selected.
#'   
#' @examples
#'  select_door
#' 
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' The goat door.
#' 
#' @description
#' Monty will now open a door with a goat behind it.
#' 
#' @details
#' Ever the showman, Monty will show the lucky contestant one of the two goats.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the door with one of goats.  If the contestant's first door hides the car,
#'   Monty can show either of the other doors, however, if the first door selected was a goat,
#'   Monty can only show the contestant the other goat door.
#' 
#' @examples
#' open_goat_door
#' 
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' The other door opportunity.
#' 
#' @description
#' Monty always gives each contestant the ability to switch doors.
#' 
#' @details
#' If the contestant wishes to keep their first selection stay will = T, if they choose
#' to switch doors the selection will be stay = F.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the door the door number that is not the original selection, nor the 
#'   goat door that was shown to them in the previous segement.
#' 
#' @examples
#' change_door
#' 
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Did you win?
#' 
#' @description
#' Monty now shows you what is behind the door you selected, showing you if
#' you won a beautiful new car, or a goat!
#' 
#' @details
#' If the contestant selected the car door, they will see "WIN", if they 
#' selected the goat door they will see, "LOSE"
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a string of win or lose.
#' 
#' @examples
#' determine_winner 
#' 
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play both opportunities to see which strategy wins.
#' 
#' @description
#' In this section you will play both the stay and switch strategy to identify which 
#' method gives you a greater chance at winning.
#' 
#' @details
#' This section will capture all wins and losses playing both stay and switch strategies.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return returns win lose values.
#' 
#' @examples
#' play_game
#' 
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' This is the table that shows you the results of staying and switching.
#' 
#' @description
#' The table will catalog all wins and losses for 100 games to give the viewer 
#' an understanding of the best strategy for playing Monty Hall.
#' 
#' @details
#' This simulates 100 games playing a stay strategy, or playing the same 100 games
#' with a switch strategy.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return wins and losses in a table.
#' 
#' @examples
#' play_n_games
#' 
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
