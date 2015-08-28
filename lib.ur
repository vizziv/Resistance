fun id [t] (x : t) = x

fun distinct [a] (_ : eq a) (_ : ord a) (xs : list a) =
    let
        fun check xs =
            case xs of
                x0 :: x1 :: xs => if x0 = x1 then False else check (x1 :: xs)
              | _ => True
    in
        check (List.sort le xs)
    end

fun showList [t] (_ : show t) (xs : list t) =
    let
        fun showList' xs =
            case xs of
                [] => "]"
              | x :: xs => ", " ^ show x ^ showList' xs
    in
        case xs of
            [] => "[]"
          | x :: xs => "[" ^ show x ^ showList' xs
    end

fun keep [keep ::: {Type}] [drop ::: {Type}] [keep ~ drop]
         (xs : $(keep ++ drop)) : $keep =
    xs --- drop

fun set [keep ::: {Type}] [change ::: {Type}] [keep ~ change]
        (xs : $(keep ++ change)) (ys : $change) : $(keep ++ change) =
    xs --- change ++ ys

fun extract [f ::: Name] [t] (xs : $[f = t]) : t = xs.f

fun sqlInjectRow [tables ::: {{Type}}] [agg ::: {{Type}}] [exps ::: {Type}]
                 [ts ::: {Type}] (f : folder ts)
                 (injs : $(map sql_injectable ts)) (xs : $ts) =
    @map2 [sql_injectable] [ident] [sql_exp tables agg exps]
          (@@sql_inject [tables] [agg] [exps]) f injs xs

fun insertRow [fields ::: {Type}] [uniques ::: {{Unit}}]
              (f : folder fields) (injs : $(map sql_injectable fields))
              (tab : sql_table fields uniques) (row : $fields) =
    insert tab (@sqlInjectRow f injs row)

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
                 GroupBy = sql_subset [[T = ([nKey = tKey] ++ vals ++ other,
                                             [])]],
                 Having = sql_inject True,
                 SelectFields = sql_subset [[T = (vals,
                                                  [nKey = tKey] ++ other)]],
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
