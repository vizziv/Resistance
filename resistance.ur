fun formStart user =
    Ui.button {Value = "Start", Onclick = rpc (Controller.start user)}

fun formVote user =
    Ui.buttons ({Value = "Approve",
                 Onclick = rpc (Controller.vote True user)}
             :: {Value = "Reject",
                 Onclick = rpc (Controller.vote False user)} :: [])

fun formMission user =
    Ui.buttons ({Value = "Success",
                 Onclick = rpc (Controller.mission True user)}
             :: {Value = "Fail",
                 Onclick = rpc (Controller.mission False user)} :: [])

fun formPropose' numPlayers missionSize user =
    srcs <- List.tabulateM (fn _ => source 0.0) missionSize;
    let
        val sgl =
            players <- List.mapM (compose (Monad.mp round) signal) srcs;
            if Lib.distinct players
               && Lib.minimum numPlayers players >= 0
               && Lib.maximum 0 players < numPlayers
            then return (Ui.button
                             {Value = "Propose",
                              Onclick = rpc (Controller.propose players user)})
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

fun formPropose numPlayers missionSize user = <xml>
  <active code={formPropose' numPlayers missionSize user}/>
</xml>

fun render playerq responseq user =
    case responseq of
        None => <xml></xml>
      | Some response =>
        case response.Request of
            Game.Propose propose =>
            if playerq = Some propose.Leader
            then formPropose (Game.numPlayers response.Game)
                             propose.MissionSize
                             user
            else <xml></xml>
          | Game.Mission players =>
            if (case playerq of
                    None => False
                  | Some player => List.mem player players)
            then formMission user
            else <xml></xml>
          | Game.Vote _ => formVote user
          | _ => <xml></xml>

fun play showStart group =
    {User = user, Channel = chan} <- Controller.joinGroup group;
    srcShowStart <- source showStart;
    srcResponse <- source None;
    srcPlayer <- source None;
    buffer <- Buffer.create;
    let
        fun listen () =
            {Response = response, Report = reportq} <- recv chan;
            (case reportq of
                 Some report =>
                 (case report of
                      Controller.Init init =>
                      set srcShowStart False;
                      set srcPlayer (Some init.Player)
                    | _ => return ());
                 Buffer.write buffer (show report)
               | None => return ());
            set srcResponse (Some response);
            Buffer.write buffer (show response.Game);
            Buffer.write buffer (show response.Request);
            listen ()
        val sgnl =
            showStart <- signal srcShowStart;
            responseq <- signal srcResponse;
            playerq <- signal srcPlayer;
            return (if showStart
                    then formStart user
                    else render playerq responseq user)
    in
        return <xml>
          <body onload={listen ()}>
            <h1>Resistance</h1>
            <h2>Game #{[group]}</h2>
            <div>
              <dyn signal={sgnl}/>
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
                <submit action={playJoin group} value={"Join game #" ^ show group}/>
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

(*
fun fib (n : int) = if n <= 1 then n else fib (n-1) + fib (n-2)

fun getFib n : transaction int = return (fib n)

val test =
    xr <- source 0.0;
    yr <- source 0;
    return <xml>
      <body>
        <cnumber source={xr}/>
        <button onclick={fn _ => x <- get xr; y <- rpc (getFib (round x)); set yr y}/>
        <dyn signal={y <- signal yr; return <xml>{[y]}</xml>}/>
      </body>
    </xml>
*)

val test = ClientRequest.test
