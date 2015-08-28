type group
val show_group : show group

datatype init = Resistance | Spy of list Game.player

type message = {Response : Game.response, Init : option init}

val createGroup : transaction group
val allGroups : transaction (list group)
val joinGroup : group -> transaction {Player : Game.player,
                                      Channel : (channel message)}

val start : transaction unit
val propose : list Game.player -> transaction unit
val vote : bool -> transaction unit
val mission : bool -> transaction unit
