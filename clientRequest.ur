functor ClientRequest(M : sig
    con interface :: {(Type * Type)}
    type request = variant (map fst interface)
    type response = variant (map snd interface)
    val mkCont : (request -> transaction unit) -> response -> transaction unit
end) : sig
    structure Client : sig
        val init : (M.request -> transaction unit) -> transaction unit
        val answer : M.response -> transaction unit
    end
    structure Server : sig
        val ask : M.request -> transaction unit
    end
end = struct

open M

table channels :
      {Client : client, Channel : channel request}
          PRIMARY KEY Client

table requests :
      {Client : client, Request : serialized request}
          PRIMARY KEY Client

val channel =
    cli <- self;
    chan <- channel;
    Lib.insertRow channels {Client = cli, Channel = chan};
    queryI1 (Lib.sqlWhereEq [_] [_] requests cli)
            (fn {Request = reqz} => send chan (deserialize reqz));
    return chan

fun ask req =
    cli <- self;
    rowq <- oneOrNoRows1 (Lib.sqlWhereEq [_] [_] channels cli);
    case rowq of
        None => Lib.insertRow requests {Client = cli, Request = serialize req}
      | Some {Channel = chan} => send chan req

val cont = mkCont ask

structure Client = struct
    fun init action = bind (rpc channel) (Lib.spawnListener action)
    fun answer resp = rpc (cont resp)
end

structure Server = struct
    val ask = ask
end

end

structure CR = ClientRequest(struct
    fun mkCont ask =
        Lib.cases
            {A = fn n => debug (Lib.plural n "object"); ask (make [#B] n),
             B = fn n => debug (Lib.plural n "thingy"); ask (make [#A] n)}
end)

fun test' _ =
    CR.Server.ask (make [#A] 9001);
    let
        fun sgl answerqSrc =
            answerq <- signal answerqSrc;
            case answerq of
                None => return <xml>Nothing to do</xml>
              | Some answer =>
                return <xml>
                  Submit: {Ui.button {Value = "Click me!", Onclick = answer ()}}
                </xml>
        fun action answerqSrc =
            Lib.cases
                {A = fn n => set answerqSrc (Some (fn () => CR.Client.answer (make [#A] (n+5)))),
                 B = fn n => CR.Client.answer (make [#B] (n-2))}
        val mkForm =
            answerqSrc <- source None;
            return <xml>
              <dyn signal={sgl answerqSrc}/>
              {Ui.button {Value = "Start listening",
                          Onclick = CR.Client.init (action answerqSrc)}}
            </xml>
    in
        return <xml>
          <body>
            <h1>Test</h1>
            Inspect ALL the elements!
            <active code={mkForm}/>
          </body>
        </xml>
    end

val test : transaction page =
    return <xml>
      <body>
        <form>
          <submit value="Make request" action={test'}/>
        </form>
      </body>
    </xml>
