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
#'   Randomly select a door.
#' @description
#'   `select_door()` randomly selects one of the three doors.
#' @details
#'   This is the first step of the game where the contestant selects a door.
#' @param ... no arguments are used by the function. 
#' @return The function returns a number between 1 and 3, indicating the contestant door choice.
#' @examples
#'   select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Select a goat door from the two remaining doors.
#' @description
#'   `open_goat_door()` selects one of the two remaining doors that does not have the car.
#' @details
#'   This is the second step of the game where the host selects a door with a goat behind it.
#' @param game A character vector of length 3 representing the Monty Hall game setup. One door has "car" and two doors have "goat".
#' @param a.pick An integer representing the contestant's initial door choice (1, 2, or 3). 
#' @return The function returns a number between 1 and 3, indicating the host door choice. The door number will be a door that is not the contestant choice and not the car.
#' @examples
#'   open_goat_door( this.game, my.initial.pick )
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
#'   Choose whether to stay or switch the contestant door choice and return the final door choice.
#' @description
#'   `change_door()` returns the final door choice based on whether the contestant switches or stays with the original door.
#' @details
#'   This is the third step of the game where the contestant chooses to switch or stay with their door.
#' @param stay A logical value indicating whether the contestant stays with their initial door choice (`TRUE` by default) or switches (`FALSE`).
#' @param opened.door An integer representing the door that was opened to reveal a goat (1, 2, or 3).
#' @param a.pick An integer representing the contestant's initial door choice (1, 2, or 3).
#' @return The function returns a number between 1 and 3, indicating the final door choice. With the stay strategy, the final choice will be the same as the initial choice. With the switch strategy, the final choice will be the door that was not the initial pick and not the door opened by the host.
#' @examples
#'   change_door( stay=T, opened.door, a.pick )
#'   change_door( stay=F, opened.door, a.pick )
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
#'   Determine whether the contestant has won based on the final door choice.
#' @description
#'   `determine_winner()` returns "WIN" if the final door has the car or "LOSE" if the final door has a goat.
#' @details
#'   This labels the game as a win or loss.
#' @param final.pick An integer representing the contestant's final door choice (1, 2, or 3).
#' @param game A character vector of length 3 representing the Monty Hall game setup. One door has "car" and the other two have "goat". 
#' @return The function returns "WIN" if the final door has the car or "LOSE" if the final door has a goat.
#' @examples
#'   determine_winner( final.pick, game )
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
#'   Combines all of the functions to play a full game.
#' @description
#'   `play_game()` runs a simulation of the game and creates a dataframe to show if the contestant wins or loses based on the strategy chosen.
#' @details
#'   This brings all of the steps together into a simulation of the game.
#' @param ... no arguments are used by the function. 
#' @return The function returns a dataframe that shows if the contestant will "WIN" or "LOSE" based on the two strategy choices.
#' @examples
#'   play_game( )
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
#'   Play multiple games.
#' @description
#'   `play_n_games()` runs multiple simulations of the game and create a dataframe to show how many times the contestant wins or loses based on the strategy chosen.
#' @details
#'   This runs multiple simulations of the game and returns the results in a single dataframe.
#' @param n An integer specifying the number of games to play. The default is 100.
#' @return The function returns a dataframe that shows how many times the contestant will "WIN" or "LOSE" based on the two strategy choices.
#' @examples
#'   play_n_games( n=100 )
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