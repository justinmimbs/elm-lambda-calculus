module Lambda
    exposing
        ( Definition
        , Expression(..)
        , equivalent
        , eval
        , include
        , parseDefinition
        , parseExpression
        , print
        , simplify
        )

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)


type Expression
    = Name Char
    | Function Char Expression
    | Application Expression Expression


print : Expression -> String
print exp =
    case exp of
        Name name ->
            String.fromChar name

        Function argName bodyExp ->
            let
                ( argNames, bodyExp2 ) =
                    uncurryFunction [ argName ] bodyExp
            in
            "\\" ++ String.fromList argNames ++ "." ++ print bodyExp2

        Application funcExp argExp ->
            let
                left =
                    case funcExp of
                        Function _ _ ->
                            "(" ++ print funcExp ++ ")"

                        _ ->
                            print funcExp

                right =
                    case argExp of
                        Name _ ->
                            print argExp

                        _ ->
                            "(" ++ print argExp ++ ")"
            in
            left ++ right


uncurryFunction : List Char -> Expression -> ( List Char, Expression )
uncurryFunction argNames bodyExp =
    case bodyExp of
        Function argName nextBodyExp ->
            uncurryFunction (argName :: argNames) nextBodyExp

        _ ->
            ( List.reverse argNames, bodyExp )


eval : Expression -> Expression
eval exp =
    case exp of
        Name _ ->
            exp

        Function argName bodyExp ->
            Function argName (eval bodyExp)

        Application funcExp argExp ->
            case eval funcExp of
                Name name ->
                    Application (Name name) (eval argExp)

                Function argName bodyExp ->
                    -- normal order
                    substitute argExp argName bodyExp |> eval

                (Application _ _) as app ->
                    Application app (eval argExp)


substitute : Expression -> Char -> Expression -> Expression
substitute argExp argName bodyExp =
    let
        freeInArg =
            findFree [] [] argExp
    in
    substituteHelp [ argName ] freeInArg argExp argName bodyExp


substituteHelp : List Char -> List Char -> Expression -> Char -> Expression -> Expression
substituteHelp bound freeInArg argExp argName bodyExp =
    case bodyExp of
        Name name ->
            if name == argName then
                argExp
            else
                bodyExp

        Function argName2 bodyExp2 ->
            if argName == argName2 then
                -- argName is bound, so don't descend
                bodyExp
            else if List.member argName2 freeInArg then
                -- rename argName2 in bodyExp2
                let
                    conflictingNames =
                        bound ++ freeInArg ++ findFree [] [] bodyExp

                    newName =
                        "abcdefghijklmnopqrstuvwxyz"
                            |> String.toList
                            |> find (\c -> not (List.member c conflictingNames))
                            |> Maybe.withDefault '?'
                in
                Function newName (bodyExp2 |> rename newName argName2 |> substituteHelp (newName :: bound) freeInArg argExp argName)
            else
                Function argName2 (bodyExp2 |> substituteHelp (argName2 :: bound) freeInArg argExp argName)

        Application funcExp2 argExp2 ->
            Application
                (funcExp2 |> substituteHelp bound freeInArg argExp argName)
                (argExp2 |> substituteHelp bound freeInArg argExp argName)


{-| Rename free variables in an expression.
-}
rename : Char -> Char -> Expression -> Expression
rename newName targetName exp =
    case exp of
        Name name ->
            if name == targetName then
                Name newName
            else
                exp

        Function argName bodyExp ->
            if argName == targetName then
                -- targetName is bound, so don't descend
                exp
            else
                Function argName (bodyExp |> rename newName targetName)

        Application funcExp argExp ->
            Application
                (funcExp |> rename newName targetName)
                (argExp |> rename newName targetName)


{-| Find all names that are not bound as function argument names.
-}
findFree : List Char -> List Char -> Expression -> List Char
findFree bound free exp =
    case exp of
        Name name ->
            if List.member name bound then
                free
            else
                name :: free

        Function argName bodyExp ->
            free ++ findFree (argName :: bound) free bodyExp

        Application funcExp argExp ->
            free ++ findFree bound free funcExp ++ findFree bound free argExp



-- parseExpression


parseExpression : Parser Expression
parseExpression =
    Parser.lazy <|
        \() ->
            Parser.succeed
                -- application is left-associative
                (List.foldl (flip Application))
                |= parseTerm
                |= Parser.repeat Parser.zeroOrMore parseTerm


parseTerm : Parser Expression
parseTerm =
    Parser.lazy <|
        \() ->
            Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.symbol "("
                    |= parseExpression
                    |. Parser.symbol ")"
                , parseName
                , parseFunction
                ]


parseName : Parser Expression
parseName =
    Parser.map
        Name
        (parseChar isUnreserved)


parseFunction : Parser Expression
parseFunction =
    Parser.lazy <|
        \() ->
            Parser.succeed
                curryFunction
                |. Parser.symbol "\\"
                |= Parser.repeat Parser.oneOrMore (parseChar isUnreserved)
                |. Parser.symbol "."
                |= parseExpression


curryFunction : List Char -> Expression -> Expression
curryFunction argNames bodyExp =
    case argNames of
        argName :: rest ->
            Function argName (curryFunction rest bodyExp)

        [] ->
            bodyExp


parseChar : (Char -> Bool) -> Parser Char
parseChar predicate =
    Parser.map
        (String.uncons >> unwrap '_' Tuple.first)
        (Parser.keep (Parser.Exactly 1) predicate)


isUnreserved : Char -> Bool
isUnreserved char =
    (char /= '(')
        && (char /= ')')
        && (char /= '\\')
        && (char /= '.')
        && (char /= ' ')
        && (char /= '=')



-- Definition


type alias Definition =
    ( Char, Expression )


include : List Definition -> Expression -> Expression
include defs =
    includeHelp (List.reverse defs)


includeHelp : List Definition -> Expression -> Expression
includeHelp defs exp =
    case defs of
        ( argName, argExp ) :: rest ->
            includeHelp rest (Application (Function argName exp) argExp)

        [] ->
            exp


simplify : List Definition -> Expression -> Expression
simplify defs exp =
    case find (Tuple.second >> equivalent exp) defs of
        Just ( name, _ ) ->
            Name name

        Nothing ->
            case exp of
                Application funcExp argExp ->
                    Application (simplify defs funcExp) (simplify defs argExp)

                _ ->
                    exp



-- parseDefinition


parseDefinition : Parser Definition
parseDefinition =
    Parser.succeed (,)
        |= Parser.delayedCommitMap always
            (Parser.succeed identity
                |= parseChar isUnreserved
                |. spaceAllowed
            )
            (Parser.symbol "=")
        |. spaceAllowed
        |= parseExpression


spaceAllowed : Parser ()
spaceAllowed =
    Parser.ignore Parser.zeroOrMore isSpace


isSpace : Char -> Bool
isSpace =
    (==) ' '



-- equivalence


equivalent : Expression -> Expression -> Bool
equivalent a b =
    emptyRelation |> findCorrespondence a b |> isJust


{-| Attempt to find a correspondence between the same-scoped names of two
expressions with the same structure. Because of name masking, the resulting
relation can be partial; so while not useful itself, its presence indicates
that a correspondence exists.
-}
findCorrespondence : Expression -> Expression -> Relation -> Maybe Relation
findCorrespondence a b rel =
    case ( a, b ) of
        ( Name nameA, Name nameB ) ->
            rel
                |> extendRelation nameA nameB

        ( Function argNameA bodyExpA, Function argNameB bodyExpB ) ->
            rel
                |> replaceRelation argNameA argNameB
                |> findCorrespondence bodyExpA bodyExpB

        ( Application funcExpA argExpA, Application funcExpB argExpB ) ->
            rel
                |> findCorrespondence funcExpA funcExpB
                |> Maybe.andThen (findCorrespondence argExpA argExpB)

        _ ->
            Nothing


{-| Represents a binary relation, where `extend` and `replace` maintain a
partial bijection.
-}
type alias Relation =
    { left : Set Char
    , right : Set Char
    , pairs : Dict Char Char
    }


emptyRelation : Relation
emptyRelation =
    Relation Set.empty Set.empty Dict.empty


extendRelation : Char -> Char -> Relation -> Maybe Relation
extendRelation l r ({ left, right, pairs } as rel) =
    if (pairs |> Dict.get l) == Just r then
        Just rel
    else if not (Set.member l left) && not (Set.member r right) then
        Just (rel |> replaceRelation l r)
    else
        Nothing


replaceRelation : Char -> Char -> Relation -> Relation
replaceRelation l r { left, right, pairs } =
    Relation
        (left |> Set.insert l)
        (right |> Set.insert r)
        (pairs |> removeByValue r |> Dict.insert l r)



-- helpers


find : (a -> Bool) -> List a -> Maybe a
find pred list =
    case list of
        [] ->
            Nothing

        x :: rest ->
            if pred x then
                Just x
            else
                find pred rest


unwrap : b -> (a -> b) -> Maybe a -> b
unwrap default f m =
    case m of
        Just x ->
            f x

        Nothing ->
            default


removeByValue : v -> Dict comparable v -> Dict comparable v
removeByValue val dict =
    dict
        |> Dict.toList
        |> find (Tuple.second >> (==) val)
        |> unwrap dict (\( key, _ ) -> Dict.remove key dict)


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just _ ->
            True

        Nothing ->
            False
