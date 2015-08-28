type game
type player = int

datatype request =
    Propose of {Leader : player, MissionSize : int}
  | Vote of list player
  | Mission of list player
  | Win
  | Lose

datatype role = Resistance | Spy

val show_request : show request
val show_game : show game

type response = {Game : game, Request : request}

val new : int -> transaction {Roles : list role, Response : response}
val numPlayers : game -> int
val submitProposal : list player -> game -> response
val submitVote : list player -> list bool -> game -> response
val submitMission : list bool -> game -> response
