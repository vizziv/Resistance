type group
type user

val show_group : show group

datatype reveal = Resistance | Spy of list Game.player

datatype report =
    Init of {Player : Game.player, Reveal : reveal}
  | Votes of list {Player : Game.player, Approve : bool}
  | Actions of {Success : int, Fail : int}

val show_report : show report

type message = {Response : Game.response, Report : option report}

val createGroup : transaction group
val allGroups : transaction (list group)
val joinGroup : group -> transaction {User : user, Channel : channel message}

val start : user -> transaction unit
val propose : list Game.player -> user -> transaction unit
val vote : bool -> user -> transaction unit
val mission : bool -> user -> transaction unit
