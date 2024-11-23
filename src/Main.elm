module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Fraction as Fr
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import List.Bag as LB


type Pewaris
    = Suami
    | Istri
    | AnakLaki
    | AnakPerempuan


showPewaris : Pewaris -> String
showPewaris pewaris =
    case pewaris of
        Suami ->
            "Suami"

        Istri ->
            "Istri"

        AnakLaki ->
            "AnakLaki"

        AnakPerempuan ->
            "AnakPerempuan"


readPewaris : String -> Maybe Pewaris
readPewaris str =
    case str of
        "Suami" ->
            Just Suami

        "Istri" ->
            Just Istri

        "AnakLaki" ->
            Just AnakLaki

        "AnakPerempuan" ->
            Just AnakPerempuan

        _ ->
            Nothing


type alias Bagian =
    Fr.Fraction


pembagian : Pewaris -> Bagian
pembagian pewaris =
    case pewaris of
        AnakLaki ->
            Fr.frac 1 2

        Suami ->
            Fr.frac 1 2

        Istri ->
            Fr.frac 1 2

        AnakPerempuan ->
            Fr.frac 1 2


type alias Jumlah =
    Int


type alias Rupiah =
    Int


type alias Config =
    List ( Pewaris, Jumlah )


showConfig : Config -> String
showConfig config =
    let
        sh ( p, j ) =
            "(" ++ showPewaris p ++ " : " ++ String.fromInt j ++ ")"
    in
    List.map sh config
        |> String.join ", "


type alias PewarisStr =
    String


type alias Hasil =
    Dict PewarisStr Bagian


insert : Pewaris -> Bagian -> Hasil -> Hasil
insert p b h =
    Dict.insert (showPewaris p) b h


get : Pewaris -> Hasil -> Maybe Bagian
get p hsl =
    Dict.get (showPewaris p) hsl


add : ( Pewaris, Bagian ) -> Hasil -> Hasil
add cfg hsl =
    case cfg of
        ( p, b ) ->
            case get p hsl of
                Nothing ->
                    insert p b hsl

                Just fr ->
                    insert p (Fr.add fr b) hsl


hitungHasil : Config -> Hasil
hitungHasil =
    List.map (\( p, _ ) -> ( p, pembagian p ))
        >> List.foldl add Dict.empty


type alias Model =
    { config : Config
    , harta : Int
    , hasil : Hasil
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { config = LB.empty
      , harta = 0
      , hasil = Dict.empty
      }
    , Cmd.none
    )


type Msg
    = TambahPewaris Pewaris Jumlah
    | UbahJumlahHarta Rupiah


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TambahPewaris pewaris jumlah ->
            let
                newConfig =
                    model.config
                        |> LB.insert jumlah pewaris
            in
            ( { model
                | config = newConfig
                , hasil = hitungHasil newConfig
              }
            , Cmd.none
            )

        UbahJumlahHarta rupiah ->
            ( { model | harta = rupiah }, Cmd.none )


showHarta : Rupiah -> String
showHarta =
    String.fromInt
        >> String.toList
        >> List.reverse
        >> split 3
        >> List.map List.reverse
        >> List.reverse
        >> List.map String.fromList
        >> String.join ","
        >> (\r -> "Rp. " ++ r)


split : Int -> List a -> List (List a)
split n list =
    case ( n, list ) of
        ( _, [] ) ->
            []

        ( 0, l ) ->
            [ l ]

        _ ->
            List.take n list :: split n (List.drop n list)


testing : String
testing =
    showHarta 1234568


view : Model -> Html Msg
view model =
    H.div []
        [ H.h1 [] [ H.text "Warisin" ]
        , H.div [] <|
            List.map
                (\pw -> H.button [ E.onClick <| TambahPewaris pw 1 ] [ H.text <| "tambah " ++ showPewaris pw ])
                [ Suami, Istri, AnakLaki, AnakPerempuan ]
        , H.p []
            [ model.config
                |> showConfig
                |> H.text
            ]
        , H.p []
            [ model.config
                |> List.map (Tuple.first >> pembagian >> Fr.show)
                |> String.join ", "
                |> H.text
            ]
        , H.p []
            [ model.hasil
                |> showHasil
                |> H.text
            ]
        , H.input
            [ A.type_ "number"
            , A.placeholder "Masukkan jumlah harta"
            , E.onInput
                (String.toInt
                    >> Maybe.withDefault 0
                    >> UbahJumlahHarta
                )
            ]
            []
        , H.p []
            [ model
                |> .harta
                |> showHarta
                |> H.text
            ]
        ]


showHasil : Hasil -> String
showHasil =
    Dict.toList
        >> List.map (\( p, b ) -> p ++ " : " ++ Fr.show b)
        >> String.join ", "


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
