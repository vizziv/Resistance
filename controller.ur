(*
 * TODO:
 * - handle bad function calls better (e.g. one player votes twice)
 * - extract common factor from vote and mission
 * - report bug where unification error happens instead of type error
 *)

type id = int
type group = id

type response = Game.response
type player = Game.player
type role = Game.role

type user = {Id : id, Player : player}

datatype reveal = Resistance | Spy of list player

type message =
     {Response : response,
      Init : option {Player : player, Reveal : reveal}}

sequence ids

table players :
      {User : serialized user,
       Channel : channel message,
       Id : id,
       Role : serialized (option Game.role),
       Player : player}
          PRIMARY KEY User

table groups : {Id : id, Size : int} PRIMARY KEY Id
table games : {Id : id, Response : serialized response} PRIMARY KEY Id
table votes : {Id : id, Player : player, Approve : bool}
table actions : {Id : id, Player : player, Success : bool}

fun load id = Monad.mp deserialize (Lib.sqlLookup1 [#Id] [#Response] games id)

fun store id response =
    dml (UPDATE games SET Response = {[serialize response]} WHERE Id = {[id]})

val sizeGroup = Lib.sqlLookup1 [#Id] [#Size] groups

val createGroup =
    id <- nextval ids;
    dml (Lib.insertRow groups {Id = id, Size = 0});
    return id

fun joinGroup id =
    chan <- channel;
    player <- sizeGroup id;
    let
        val user = {Id = id, Player = player}
    in
        dml (UPDATE groups SET Size = Size + 1 WHERE Id = {[id]});
        dml (Lib.insertRow players {User = serialize user,
                                    Channel = chan,
                                    Id = id,
                                    Role = serialize None,
                                    Player = player});
        return {User = user, Channel = chan}
    end

val allGroups =
    query1' (SELECT groups.Id FROM groups) (fn row ids => row.Id :: ids) []

fun broadcast id response =
    store id response;
    debug ("broadcast " ^ show response.Request);
    queryI1 (SELECT players.Channel
             FROM players
             WHERE players.Id = {[id]})
            (fn {Channel = chan, ...} => send chan {Response = response,
                                                    Init = None})

fun start' id _ _ _ : transaction unit =
    numPlayers <- sizeGroup id;
    dml (DELETE FROM groups WHERE Id = {[id]});
    {Roles = roles, Response = response} <- Game.new numPlayers;
    List.foldlMi (fn player role _ =>
                     dml (UPDATE players
                          SET Role = {[serialize (Some role)]}
                          WHERE Id = {[id]} AND Player = {[player]}))
                 ()
                 roles;
    dml (Lib.insertRow games {Id = id, Response = serialize response});
    let
        val spies =
            Lib.mapiPartial (fn i role =>
                                case role of
                                    Game.Spy => Some i
                                  | Game.Resistance => None)
                            roles
    in
        queryI1 (Lib.sqlWhereEq [#Id] [_] players id)
                (fn {Channel = chan, Player = player, Role = roleSerial} =>
                    send chan
                         {Response = response,
                          Init = Some {Player = player,
                                       Reveal = case deserialize roleSerial of
                                                    Some Game.Spy => Spy spies
                                                  | _ => Resistance}})
    end

fun propose' id player _ players =
    {Game = game, Request = request} <- load id;
    case request of
        Game.Propose {Leader = leader, ...} =>
        if leader = player
        then broadcast id (Game.submitProposal players game)
        else return ()
      | _ => return ()

fun vote' id player _ approve =
    {Game = game, Request = request} <- load id;
    case request of
        Game.Vote players =>
        dml (Lib.insertRow votes {Id = id,
                                  Player = player,
                                  Approve = approve});
        count <- oneRowE1 (SELECT COUNT( * )
                           FROM votes
                           WHERE votes.Id = {[id]});
        if count >= Game.numPlayers game
        then approves <- query1' (Lib.sqlWhereEq [#Id] [_] votes id)
                                 (fn {Approve = a} as => a :: as)
                                 [];
             dml (DELETE FROM votes WHERE Id = {[id]});
             broadcast id (Game.submitVote players approves game)
        else return ()
      | _ => return ()

fun mission' id player roleq success =
    {Game = game, Request = request} <- load id;
    case request of
        Game.Mission players =>
        if List.mem player players
        then dml (Lib.insertRow actions {Id = id,
                                         Player = player,
                                         Success = case roleq of
                                                       Some Game.Spy => success
                                                     | _ => True});
             count <- oneRowE1 (SELECT COUNT( * )
                                FROM actions
                                WHERE actions.Id = {[id]});
             if count >= List.length players
             then successes <- query1' (Lib.sqlWhereEq [#Id] [_] actions id)
                                       (fn {Success = s} ss => s :: ss)
                                       [];
                  dml (DELETE FROM actions WHERE Id = {[id]});
                  broadcast id (Game.submitMission successes game)
             else return ()
        else return ()
      | _ => return ()

fun withUser [t] name (action : id -> player -> option role
                                -> t -> transaction unit)
             (x : t) user =
    let
        val {Id = id, Player = player} = user
    in
        debug ("withUser: " ^ name ^ " " ^ show id ^ "/" ^ show player);
        {Role = roleSerial} <- Lib.sqlLookup [#User] [_] players (serialize user);
        action id player (deserialize roleSerial) x
    end

val start = withUser "start" start' ()
val propose = withUser "propose" propose'
val vote = withUser "vote" vote'
val mission = withUser "mission" mission'
