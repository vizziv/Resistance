(*
 * TODO:
 * - avoid needing safeGet.
 *)

fun buttons' rows =
    srcShouldShow <- source True;
    let
        val sgl =
            shouldShow <- signal srcShouldShow;
            if shouldShow
            then return (List.mapX (fn {Value = value,
                                        Onclick = onclick} => <xml>
              <button value={value}
                      onclick={fn _ => set srcShouldShow False; onclick}/>
            </xml>) rows)
            else return <xml></xml>
    in
        return <xml><dyn signal={sgl}/></xml>
    end

fun button1' row = buttons' (row :: [])

fun buttons rows = <xml><active code={buttons' rows}/></xml>
fun button1 rows = <xml><active code={button1' rows}/></xml>

val formStart = button1 {Value = "Start", Onclick = rpc Controller.start}

val formVote =
    buttons ({Value = "Approve",
              Onclick = rpc (Controller.vote True)}
          :: {Value = "Reject",
              Onclick = rpc (Controller.vote False)} :: [])

val formMission =
    buttons ({Value = "Success",
              Onclick = rpc (Controller.mission True)}
          :: {Value = "Fail",
              Onclick = rpc (Controller.mission False)} :: [])

fun formPropose' numPlayers missionSize =
    srcs <- List.tabulateM (fn _ => source 0.0) missionSize;
    let
        val sgl =
            players <- List.mapM (compose (Monad.mp round) signal) srcs;
            if Lib.distinct players
            then return (button1 {Value = "Propose",
                                  Onclick = rpc (Controller.propose players)})
            else return <xml></xml>
    in
        return <xml>
          {List.mapX (fn src => <xml>
            <cnumber source={src}
                     min={0.0}
                     max={float (numPlayers - 1)}
                     step={1.0}/>
          </xml>) srcs}
          <dyn signal={sgl}/>
        </xml>
    end

fun formPropose numPlayers missionSize = <xml>
  <active code={formPropose' numPlayers missionSize}/>
</xml>

fun render player responseq =
    case responseq of
        None => <xml></xml>
      | Some response =>
        case response.Request of
            Game.Propose propose =>
            if player = propose.Leader
            then formPropose (Game.numPlayers response.Game)
                             propose.MissionSize
            else <xml></xml>
          | Game.Mission players =>
            if List.mem player players
            then formMission
            else <xml></xml>
          | Game.Vote _ => formVote
          | _ => <xml></xml>

fun play showStart group =
    {Player = player, Channel = chan} <- Controller.joinGroup group;
    srcShowStart <- source showStart;
    srcResponse <- source None;
    buffer <- Buffer.create;
    let
        fun showInit init =
            "Player " ^ show player ^ ", you're "
            ^ case init of
                  Controller.Resistance => "on the resistance."
                | Controller.Spy spies =>
                  "a spy. The spy team is " ^ Lib.showList spies ^ "."
        fun listen () =
            {Response = response, Init = init} <- recv chan;
            (case init of
                 Some init =>
                 set srcShowStart False;
                 Buffer.write buffer (showInit init)
               | None => return ());
            set srcResponse (Some response);
            Buffer.write buffer (show response.Game);
            Buffer.write buffer (show response.Request);
            listen ()
        val fm =
            showStart <- signal srcShowStart;
            responseq <- signal srcResponse;
            return (if showStart
                    then formStart
                    else render player responseq)
    in
        return <xml>
          <body onload={listen ()}>
            <h1>Resistance</h1>
            <h2>Game #{[group]}</h2>
            <div>
              <dyn signal={fm}/>
            </div>
            <div>
              <dyn signal={Buffer.render buffer}/>
            </div>
          </body>
        </xml>
    end

fun playCreate _ =
    group <- Controller.createGroup;
    play True group

fun playJoin group _ =
    play False group

val menu =
    groups <- Controller.allGroups;
    return <xml>
      <body>
        <h1>Resistance</h1>
        <h2>Menu</h2>
        <ul>
          {List.mapX (fn group => <xml>
            <li>
              <form>
                <hidden{#Dummy}/>
                <submit action={playJoin group} value={"Join game #" ^ (show group)}/>
              </form>
            </li>
          </xml>) groups}
          <li>
            <form>
              <hidden{#Dummy}/>
              <submit action={playCreate} value={"Create a new game"}/>
            </form>
          </li>
        </ul>
      </body>
    </xml>
