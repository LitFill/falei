module Fraction exposing (..)

import Html


{-| alias for the Fraction type
-}
type alias Fraction =
    { a : Int
    , b : Int
    }


{-| smart constructor
-}
frac : Int -> Int -> Fraction
frac a b =
    reduce { a = a, b = b }


{-| convert from Int
-}
fromInt : Int -> Fraction
fromInt n =
    frac n 1


{-| convert to string
-}
show : Fraction -> String
show fraction =
    let
        { a, b } =
            reduce fraction

        sa =
            String.fromInt a

        sb =
            String.fromInt b
    in
    sa ++ " / " ++ sb


{-| reduce or simplify the fraction
-}
reduce : Fraction -> Fraction
reduce fraction =
    let
        { a, b } =
            fraction

        diver =
            gcd a b

        a2 =
            a // diver

        b2 =
            b // diver
    in
    { a = a2, b = b2 }


{-| for finding greatest common divisor of two ints
-}
gcd : Int -> Int -> Int
gcd x y =
    let
        aux a b =
            if a < b then
                aux b a

            else if b == 0 then
                a

            else
                aux b (a |> modBy b)
    in
    aux (abs x) (abs y)


{-| add two fractions
-}
add : Fraction -> Fraction -> Fraction
add a b =
    reduce <|
        frac
            (a.a * b.b + b.a * a.b)
            (a.b * b.b)


{-| negate a fraction
-}
neg : Fraction -> Fraction
neg fr =
    { fr | a = -fr.a }


{-| same as [a - b]
-}
sub : Fraction -> Fraction -> Fraction
sub a b =
    add a (neg b)


{-| multiply two fractions
-}
mult : Fraction -> Fraction -> Fraction
mult a b =
    reduce <|
        frac
            (a.a * b.a)
            (a.b * b.b)


{-| same as [a^-1]
-}
inverse : Fraction -> Fraction
inverse fr =
    { fr | a = fr.b, b = fr.a }


{-| same as [a / b]
-}
div : Fraction -> Fraction -> Fraction
div a b =
    mult a (inverse b)


{-| dummy main
-}
main : Html.Html msg
main =
    Html.text "Hello!"
