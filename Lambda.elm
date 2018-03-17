module Lambda exposing (..)


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
            "(λ" ++ String.fromChar argName ++ "." ++ print bodyExp ++ ")"

        Application funcExp argExp ->
            print funcExp ++ print argExp


eval : Expression -> Expression
eval exp =
    case exp of
        Name _ ->
            exp

        Function a x ->
            Function a (eval x)

        Application x y ->
            case eval x of
                Name a ->
                    Application (Name a) (eval y)

                Function a body ->
                    -- normal order:
                    -- substitute y a body |> eval
                    -- applicative order:
                    substitute (eval y) a body

                _ ->
                    Debug.crash "Expression must have a fixed point"


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


{-| Find all names that are not bound to an arg of a containing function.
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



-- examples


ex1 =
    Application (Function 'x' (Name 'x')) (Name 'y')


ex2 =
    Application (Function 'x' (Application (Name 'x') (Name 'y'))) (Name 'y')


ex3 =
    Application (Function 'x' (Function 'y' (Application (Name 'x') (Name 'y')))) (Name 'y')


ex4 =
    -- (λx.(λy.x(λx.xy)))y
    Application
        (Function 'x' (Function 'y' (Application (Name 'x') (Function 'x' (Application (Name 'x') (Name 'y'))))))
        (Name 'y')
