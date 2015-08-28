(*
 * TODO:
 * - avoid needing safeGet.
 *)

fun rpcClose src action = set src None; rpc action

fun formStart src = <xml>
  <button value="Start" onclick={fn _ => rpc Controller.start}/>
</xml>

fun formVote src = <xml>
  <button value="Approve" onclick={fn _ => rpcClose src
                                                    (Controller.vote True)}/>
  <button value="Reject" onclick={fn _ => rpcClose src
                                                   (Controller.vote False)}/>
</xml>

fun formMission src = <xml>
  <button value="Pass" onclick={fn _ => rpcClose src
                                                 (Controller.mission True)}/>
  <button value="Fail" onclick={fn _ => rpcClose src
                                                 (Controller.mission False)}/>
</xml>

fun formPropose' numPlayers missionSize src =
    srcs <- List.tabulateM (fn _ => source 0.0) missionSize;
    let
        val sgl =
            players <- List.mapM (compose (Monad.mp round) signal) srcs;
            if Lib.distinct players
            then return <xml>
              <button value="Propose"
                      onclick={fn _ => rpc (Controller.propose players)}/>
            </xml>
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

fun formPropose numPlayers missionSize src = <xml>
  <active code={formPropose' numPlayers missionSize src}/>
</xml>

fun render player responseq src =
    case responseq of
        None => <xml></xml>
      | Some response =>
        case response.Request of
            Game.Propose propose =>
            if player = propose.Leader
            then formPropose (Game.numPlayers response.Game)
                             propose.MissionSize
                             src
            else <xml></xml>
          | Game.Mission players =>
            if List.mem player players
            then formMission src
            else <xml></xml>
          | Game.Vote _ => formVote src
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
    in
        return <xml>
          <body onload={listen ()}>
            <h1>Resistance</h1>
            <h2>Game #{[group]}</h2>
            <div>
              <dyn signal={showStart <- signal srcShowStart;
                           if showStart
                           then return (formStart srcShowStart)
                           else responseq <- signal srcResponse;
                                return (render player responseq srcResponse)}/>
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
                <submit action={playJoin group} value={"Join game #" ^ (show group)}/>
              </form>
            </li>
          </xml>) groups}
          <li>
            <form>
              <submit action={playCreate} value={"Create a new game"}/>
            </form>
          </li>
        </ul>
      </body>
    </xml>
