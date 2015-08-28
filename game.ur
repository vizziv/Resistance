type round = int
type score = int
type attempt = int
type player = int
type game =
    {NumPlayers : int,
     Round : round,
     Score : score,
     Attempt : attempt,
     Leader : player}

datatype request =
    Propose of {Leader : player, MissionSize : int}
  | Vote of list player
  | Mission of list player
  | Win
  | Lose

datatype role = Resistance | Spy

val show_request =
    let
        fun showRequest request =
            case request of
                Propose {Leader = player, MissionSize = size} =>
                "Player " ^ show player ^ ": propose "
                ^ show size ^ " players for a mission."
              | Vote players =>
                "All players: vote on " ^ Lib.showList players ^ "."
              | Mission players =>
                "Players " ^ Lib.showList players ^ ": go on the mission."
              | Win => "Resistance victory!"
              | Lose => "Spy victory!"
    in
        mkShow showRequest
    end

val show_game =
    let
        fun showGame {NumPlayers = numPlayers,
                      Round = round,
                      Score = score,
                      Attempt = attempt,
                      Leader = leader} =
            List.foldr strcat
                       ""
                       ("{NumPlayers = " :: show numPlayers ::
                        ", Round = " :: show round ::
                        ", Score = " :: show score ::
                        ", Attempt = " :: show attempt ::
                        ", Leader = " :: show leader ::
                        "}" :: [])
    in
        mkShow showGame
    end

type response = {Game : game, Request : request}

datatype newness = NewRound | NewAttempt

fun numPlayers game = game.NumPlayers

fun justRequest request game = {Game = game, Request = request}

val win = justRequest Win
val lose = justRequest Lose
fun mission players = justRequest (Mission players)

fun propose newness game =
    let
        val game =
            Lib.set game
                    {Leader = mod (game.Leader + 1) game.NumPlayers,
                     Round = case newness of
                                 NewRound => game.Round + 1
                               | NewAttempt => game.Round,
                     Attempt = case newness of
                                   NewRound => 0
                                 | NewAttempt => game.Attempt + 1}
        val missionSize =
            case List.nth (case game.NumPlayers of
                               5 => 2::3::2::3::3::[]
                             | 6 => 2::3::4::3::4::[]
                             | 7 => 2::3::3::4::4::[]
                             | _ => 3::4::4::5::5::[])
                          game.Round of
                Some n => n
              | None => error <xml>Unreachable.</xml>
    in
        {Game = game,
         Request = Propose {Leader = game.Leader, MissionSize = missionSize}}
    end

val newRound = propose NewRound

fun newAttempt game =
    (if game.Attempt >= 4
     then lose
     else propose NewAttempt) game

fun mkRoles roles numPlayers numSpies =
    if numPlayers = 0
    then return roles
    else r <- rand;
         if r `mod` numPlayers < numSpies
         then mkRoles (Spy :: roles) (numPlayers - 1) (numSpies - 1)
         else mkRoles (Resistance :: roles) (numPlayers - 1) numSpies

fun new numPlayers =
    roles <- mkRoles [] numPlayers (case numPlayers of
                                        5 => 2
                                      | 6 => 2
                                      | 10 => 4
                                      | _ => 3);
    return {Roles = roles,
            Response = newRound {NumPlayers = numPlayers,
                                 Round = -1, (* Bumped to 0 by newRound. *)
                                 Score = 0,
                                 Attempt = 0,
                                 Leader = numPlayers - 1}}

fun submitProposal players = justRequest (Vote (List.sort ge players))

fun submitVote players approves =
    let
        val pass = List.length (List.filter Lib.id approves)
                               > List.length (List.filter not approves)
    in
        if pass
        then mission players
        else newAttempt
    end

fun submitMission successes game =
    let
        val failures = (List.length (List.filter not successes))
        val success = failures = 0 || (failures = 1
                                       && game.Round = 4
                                       && game.NumPlayers >= 7)
        val game =
            Lib.set game
                    {Score = game.Score + (if success then 1 else 0)}
    in
        (if game.Score >= 3
         then win
         else if game.Round - game.Score >= 3
         then lose
         else newRound) game
    end
