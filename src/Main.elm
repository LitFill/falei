module Main exposing (..)

import Browser
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


type alias Model =
    { config : Config
    , harta : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { config = LB.empty
      , harta = 0
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
            ( { model
                | config =
                    model.config
                        |> LB.insert jumlah pewaris
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
        , H.button [ E.onClick <| TambahPewaris AnakLaki 1 ] [ H.text "tambah" ]
        , H.p []
            [ model
                |> .config
                |> showConfig
                |> H.text
            ]
        , H.input [ A.type_ "number", A.placeholder "Masukkan jumlah harta", E.onInput (String.toInt >> Maybe.withDefault 0 >> UbahJumlahHarta) ] []
        , H.p []
            [ model
                |> .harta
                |> showHarta
                |> H.text
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
