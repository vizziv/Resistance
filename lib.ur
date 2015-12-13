fun id [t] (x : t) = x

fun maximum [t] (_ : ord t) : t -> list t -> t = List.foldl max
fun minimum [t] (_ : ord t) : t -> list t -> t = List.foldl min

fun cases [ts ::: {Type}] [u] (fs : $(map (fn t => t -> u) ts)) v = match v fs

fun mapiPartial [a] [b] (f : int -> a -> option b) =
    let
        fun mp' n acc ls =
            case ls of
                [] => List.rev acc
              | x :: ls => mp' (n+1) (case f n x of
                                          None => acc
                                        | Some y => y :: acc) ls
    in
        mp' 0 []
    end

fun distinct [a] (_ : eq a) (_ : ord a) (xs : list a) =
    let
        fun check xs =
            case xs of
                x0 :: x1 :: xs => if x0 = x1 then False else check (x1 :: xs)
              | _ => True
    in
        check (List.sort le xs)
    end

fun plural (n : int) (x : string) =
    let
        fun vowel c = case strindex "aeiou" c of
                          None => False
                        | Some _ => True
        val lenMinus1 = strlen x - 1
        val last = strsub x lenMinus1
    in
        show n ^ " " ^
        if n = 1
        then x
        else case last of
                 #"s" => x ^ "es"
               | #"x" => x ^ "es"
               | #"y" => if vowel (strsub x (lenMinus1 - 1))
                         then x ^ "s"
                         else substring x 0 lenMinus1 ^ "ies"
               | _ => x ^ "s"
    end

fun stringList (xs : list string) =
    let
        fun stringList' xs =
            case xs of
                [] => "]"
              | x :: xs => ", " ^ x ^ stringList' xs
    in
        case xs of
            [] => "[]"
          | x :: xs => "[" ^ x ^ stringList' xs
    end

fun showList [t] (_ : show t) (xs : list t) = stringList (List.mp show xs)

fun keep [keep ::: {Type}] [drop ::: {Type}] [keep ~ drop]
         (xs : $(keep ++ drop)) : $keep =
    xs --- drop

fun set [keep ::: {Type}] [change ::: {Type}] [keep ~ change]
        (xs : $(keep ++ change)) (ys : $change) : $(keep ++ change) =
    xs --- change ++ ys

fun extract [f ::: Name] [t] (xs : $[f = t]) : t = xs.f

fun spawnListener [t] (action : t -> transaction unit) (chan : channel t) =
    let
        fun listen () = x <- recv chan; action x; listen ()
    in
        spawn (listen ())
    end

fun sqlInjectRow [tables ::: {{Type}}] [agg ::: {{Type}}] [exps ::: {Type}]
                 [ts ::: {Type}] (f : folder ts)
                 (injs : $(map sql_injectable ts)) (xs : $ts) =
    @map2 [sql_injectable] [ident] [sql_exp tables agg exps]
          (@@sql_inject [tables] [agg] [exps]) f injs xs

fun insertRow [fields ::: {Type}] [uniques ::: {{Unit}}]
              (f : folder fields) (injs : $(map sql_injectable fields))
              (tab : sql_table fields uniques) (row : $fields) =
    dml (insert tab (@sqlInjectRow f injs row))

fun sqlWhereEq [nKey :: Name] [vals :: {Type}] [[nKey] ~ vals]
               [tKey] (_ : sql_injectable tKey)
               [other ::: {Type}] [[nKey] ~ other] [vals ~ other]
               [tTab] (_ : fieldsOf tTab ([nKey = tKey] ++ vals ++ other))
               (tab : tTab) (key : tKey)
               : sql_query [] [] _ _ =
    let
        val q1 =
            sql_query1
                [[]]
                {Distinct = False,
                 From = sql_from_table [#T] tab,
                 Where = sql_binary sql_eq
                                    (sql_field [#T] [nKey])
                                    (sql_inject key),
                 GroupBy = sql_subset [[T = (vals, [nKey = tKey] ++ other)]],
                 Having = sql_inject True,
                 SelectFields = sql_subset_all [_],
                 SelectExps = {}}
    in
        sql_query
            {Rows = q1,
             OrderBy = sql_order_by_Nil [[]],
             Limit = sql_no_limit,
             Offset = sql_no_offset}
    end

fun sqlLookup [nKey :: Name] [vals :: {Type}] [[nKey] ~ vals]
              [tKey] (_ : sql_injectable tKey)
              [other ::: {Type}] [[nKey] ~ other] [vals ~ other]
              [tTab] (_ : fieldsOf tTab ([nKey = tKey] ++ vals ++ other))
              (tab : tTab) (key : tKey) =
    oneRow1 (sqlWhereEq [nKey] [vals] tab key)

fun sqlLookup1 [nKey :: Name] [nVal :: Name] [[nKey] ~ [nVal]]
               [tKey] (_ : sql_injectable tKey)
               [other ::: {Type}] [[nKey] ~ other] [[nVal] ~ other]
               [tVal]
               [tTab] (_ : fieldsOf tTab ([nKey = tKey, nVal = tVal] ++ other))
               (tab : tTab) (key : tKey)
               : transaction tVal =
    row <- sqlLookup [nKey] [[nVal = tVal]] tab key;
    return row.nVal
