module TableExample exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import SortableTable as Table


-- ****
-- Model
-- ****


type alias CityData =
    { city : String
    , country : String
    , population : Int
    }


type alias Model =
    { cities : List CityData
    , sortState : Table.SortState
    }


initialModel : Model
initialModel =
    { cities =
        [ CityData "Shanghai" "China" 24153000
        , CityData "Beijing" "China" 18590000
        , CityData "Karachi" "Pakistan" 18000000
        , CityData "Istanbul" "Turkey" 14657000
        , CityData "Dhaka" "Bangladesh" 14543000
        , CityData "Tokyo" "Japan" 13617000
        , CityData "Moscow" "Russia" 13197596
        , CityData "Manila" "Philippines" 12877000
        , CityData "Tianjin" "China" 12784000
        , CityData "Mumbai" "India" 12400000
        ]
    , sortState = Table.initSortState "Population" Table.Desc
    }



-- ******
-- Update
-- ******


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReSort newSortState ->
            -- Typically make request for re-sorted data here
            ( { model | sortState = newSortState }, Cmd.none )



-- ********
-- Messages
-- ********


type Msg
    = ReSort Table.SortState



-- ****
-- View
-- ****


cellPadding : Attribute msg
cellPadding =
    style
        [ ( "padding-top", "1rem" )
        , ( "border-bottom", "1px solid #989898" )
        , ( "padding-right", "0.5rem" )
        ]


tableConfig : Table.Config CityData Msg
tableConfig =
    Table.createConfig
        [ Table.defineColumn "City" (text << .city) [ cellPadding ]
        , Table.defineColumn "Country" (text << .country) [ cellPadding ]
            |> Table.makeSortable ReSort
        , Table.defineColumn "Population" (text << toString << .population) [ cellPadding ]
            |> Table.makeSortable ReSort
        ]
        (Table.defaultCustomizations
            |> Table.customizeSort
                [ style
                    [ ( "height", "16px" )
                    , ( "vertical-align", "sub" )
                    , ( "fill", "#989898" )
                    ]
                ]
            |> Table.customizeTable
                [ style [ ( "border-collapse", "collapse" ) ] ]
         -- |> Table.customizeRows
         --     (\_ ->
         --         [ style [ ( "background-color", "pink" ) ] ]
         --     )
        )


view : Model -> Html Msg
view { cities, sortState } =
    div [ id "main", style [ ( "margin", "4rem" ) ] ]
        [ h2 [] [ text "City Populations" ]
        , div [] [ Table.view tableConfig sortState cities ]
        ]



-- ***
-- App
-- ***


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
