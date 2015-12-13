(*
 * TODO:
 * - handle bad function calls better (e.g. one player votes twice)
 * - extract common factor from vote and mission
 * - suggest records be valid paths so things like [rpc foo.Bar] work.
 * - report this:

$ urweb resistance -dbms sqlite
/Users/ziv/Dropbox/Dev/Resistance/clientRequest.ur:41:24: (to 41:26) Unification failure
Expression:  hs
  Have con:
$(map (fn t :: (Type * Type) => t.2 -> transaction {}) M.interface)
  Need con:
$(map (fn t' :: Type => t' -> <UNIF:U63::Type+1>) <UNIF:U58::{Type}>)
Kind unification failure
Have:  (Type * Type)
Need:  Type
Incompatible kinds
Kind 1:  (Type * Type)
Kind 2:  Type

 *)

type id = int
type group = id

type response = Game.response
type player = Game.player
type role = Game.role

type user = {Id : id, Player : player}

datatype reveal = Resistance | Spy of list player

datatype report =
    Init of {Player : player, Reveal : reveal}
  | Votes of list {Player : player, Approve : bool}
  | Actions of {Success : int, Fail : int}

val show_report =
    let
        fun showVote {Player = player, Approve = approve} =
            show player ^ ": " ^ if approve then "approve" else "reject"
        fun showReport report =
            case report of
                Init init =>
                "Player " ^ show init.Player ^ ", you're "
                 ^ (case init.Reveal of
                        Resistance => "on the resistance."
                      | Spy spies =>
                        "a spy. The spy team is " ^ Lib.showList spies ^ ".")
              | Votes votes =>
                "Votes are " ^
                Lib.stringList (List.mp showVote
                                        (List.sort (fn v w =>
                                                       v.Player > w.Player)
                                                   votes))
                ^ "."
              | Actions actions =>
                "Mission has " ^ Lib.plural actions.Success "success" ^ " and "
                ^ Lib.plural actions.Fail "failure" ^ "."
    in
        mkShow showReport
    end

type message = {Response : response, Report : option report}

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
                  CONSTRAINT User UNIQUE (Id, Player)
table actions : {Id : id, Player : player, Success : bool}
                  CONSTRAINT User UNIQUE (Id, Player)

fun load id = Monad.mp deserialize (Lib.sqlLookup1 [#Id] [#Response] games id)

fun store id response =
    dml (UPDATE games SET Response = {[serialize response]} WHERE Id = {[id]})

val sizeGroup = Lib.sqlLookup1 [#Id] [#Size] groups

val createGroup =
    id <- nextval ids;
    Lib.insertRow groups {Id = id, Size = 0};
    return id

fun joinGroup id =
    chan <- channel;
    player <- sizeGroup id;
    let
        val user = {Id = id, Player = player}
    in
        dml (UPDATE groups SET Size = Size + 1 WHERE Id = {[id]});
        Lib.insertRow players {User = serialize user,
                               Channel = chan,
                               Id = id,
                               Role = serialize None,
                               Player = player};
        return {User = user, Channel = chan}
    end

val allGroups =
    query1' (SELECT groups.Id FROM groups) (fn row ids => row.Id :: ids) []

fun broadcast id response report =
    store id response;
    debug ("broadcast " ^ show response.Request);
    queryI1 (SELECT players.Channel
             FROM players
             WHERE players.Id = {[id]})
            (fn {Channel = chan, ...} => send chan {Response = response,
                                                    Report = report})

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
    Lib.insertRow games {Id = id, Response = serialize response};
    let
        val spies =
            Lib.mapiPartial (fn i role =>
                                case role of
                                    Game.Spy => Some i
                                  | Game.Resistance => None)
                            roles
    in
        queryI1 (Lib.sqlWhereEq [#Id] [_] players id)
                (fn {Channel = chan, Player = player, Role = rolez} =>
                    send chan
                         {Response = response,
                          Report = Some (Init {Player = player,
                                               Reveal =
                                               case deserialize rolez of
                                                   Some Game.Spy => Spy spies
                                                 | _ => Resistance})})
    end

fun propose' id player _ players =
    {Game = game, Request = request} <- load id;
    case request of
        Game.Propose {Leader = leader, ...} =>
        if leader = player
        then broadcast id (Game.submitProposal players game) None
        else return ()
      | _ => return ()

fun vote' id player _ approve =
    {Game = game, Request = request} <- load id;
    case request of
        Game.Vote players =>
        Lib.insertRow votes {Id = id,
                             Player = player,
                             Approve = approve};
        count <- oneRowE1 (SELECT COUNT( * )
                           FROM votes
                           WHERE votes.Id = {[id]});
        if count >= Game.numPlayers game
        then vs <- queryL1 (Lib.sqlWhereEq [#Id]
                                           [_]
                                           votes
                                           id);
             dml (DELETE FROM votes WHERE Id = {[id]});
             broadcast id
                       (Game.submitVote players
                                        (List.mp (fn v => v.Approve) vs)
                                        game)
                       (Some (Votes vs))
        else return ()
      | _ => return ()

val countActions = List.foldl (fn success {Success = s, Fail = f} =>
                                  if success
                                  then {Success = s + 1, Fail = f}
                                  else {Success = s, Fail = f + 1})
                              {Success = 0, Fail = 0}

fun mission' id player roleq success =
    {Game = game, Request = request} <- load id;
    case request of
        Game.Mission players =>
        if List.mem player players
        then Lib.insertRow actions {Id = id,
                                    Player = player,
                                    Success = case roleq of
                                                  Some Game.Spy => success
                                                | _ => True};
             count <- oneRowE1 (SELECT COUNT( * )
                                FROM actions
                                WHERE actions.Id = {[id]});
             if count >= List.length players
             then successes <- query1' (Lib.sqlWhereEq [#Id] [_] actions id)
                                       (fn {Success = s} ss => s :: ss)
                                       [];
                  dml (DELETE FROM actions WHERE Id = {[id]});
                  broadcast id
                            (Game.submitMission successes game)
                            (Some (Actions (countActions successes)))
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
        rolez <- Lib.sqlLookup1 [#User] [#Role] players (serialize user);
        action id player (deserialize rolez) x
    end

val start = withUser "start" start' ()
val propose = withUser "propose" propose'
val vote = withUser "vote" vote'
val mission = withUser "mission" mission'
