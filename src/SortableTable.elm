module SortableTable
    exposing
        ( Column
        , Config
        , Customizations
        , SortDir(..)
        , SortState
        , createConfig
        , createKeyedConfig
        , customizeBody
        , customizeHead
        , customizeRows
        , customizeSort
        , customizeTable
        , defaultCustomizations
        , defineColumn
        , initSortState
        , makeSortable
        , nonSortingView
        , sortInfo
        , view
        )

{-| This package exposes functions for displaying an HTML table that can be configured for sorting on different columns.


# Config

A table requires a config that should NOT be stored in your app's state. Define the config once and pass it into the view function.

@docs Config, Column, createConfig, createKeyedConfig, defineColumn, makeSortable


# State

The only state you will need to store in your app model is the `SortState` which stores which column is being sorted on in which direction. There are functions exposed for creating and extracting data about the sort state that can be utilitzed to react to a change in the sort state.

@docs SortState, SortDir, initSortState, sortInfo


# Customizations

You may specify certain customizations that will take effect in the table view. Most of the functions that are used to specify the customizations take a `List (Attribute ms)` so it can be very verbose.

@docs Customizations, customizeBody, customizeHead, customizeRows, customizeRows, customizeSort, customizeTable, defaultCustomizations


# View

There are two functions for displaying the table, `view` and `nonSortingView`. Most users will use the standard `view` function, but `nonSortingView` can be used to short-circuit some functionality if you don't need any of the sorting features.

@docs view, nonSortingView

-}

import Html exposing (..)
import Html.Attributes as Attrs
import Html.Events as Events
import Html.Keyed
import Icons
import Json.Decode as JD


-- ************
-- Sorting Info
-- ************


type alias ColumnId =
    String


{-| Sort direction can either be Asc or Desc
-}
type SortDir
    = Asc
    | Desc


{-| This is the state that needs to be stored in your application's model.
Helper methods are provided for creating and extracting data from the state.
-}
type SortState
    = SortState ColumnId SortDir


reverseSort : SortState -> SortState
reverseSort (SortState colId dir) =
    case dir of
        Asc ->
            SortState colId Desc

        Desc ->
            SortState colId Asc


changeSort : ColumnId -> SortState
changeSort colId =
    SortState colId Asc


{-| Used to extract sort info from a SortState if needed for new queries etc.
Given a SortState - returns a tuple of the columnId, and the SortDir
-}
sortInfo : SortState -> ( String, SortDir )
sortInfo (SortState colId sortDir) =
    ( colId, sortDir )


{-| Create an initial SortState given the a String columnId
-}
initSortState : ColumnId -> SortDir -> SortState
initSortState colId dir =
    SortState colId dir



-- *******
-- Columns
-- *******


type alias ColumnData a data msg =
    { a
        | label : String
        , viewData : data -> Html msg
        , cellAttrs : List (Attribute msg)
    }


{-| A column can either be sortable or non-sortable.
If it's sortable, then a sortMsg is needed to send new sort messages.
-}
type Column data msg
    = SimpleColumn (ColumnData {} data msg)
    | SortableColumn (ColumnData { sortMsg : SortState -> msg } data msg)


{-| Creates an unsortable column.
Accepts :
a String that wil be the label displayed in the colulmn header.
a function that will produce the table cell's Html given the data for that row
a list of (Attribute msg) that can be used to customize the cells for the column
-}
defineColumn : String -> (data -> Html msg) -> List (Attribute msg) -> Column data msg
defineColumn label viewData cellAttrs =
    SimpleColumn { label = label, viewData = viewData, cellAttrs = cellAttrs }


{-| Takes sortMsg and a pre-defined column and makes it sortable with the supplied sortMsg
-}
makeSortable : (SortState -> msg) -> Column data msg -> Column data msg
makeSortable sortMsg col =
    case col of
        SimpleColumn { label, viewData, cellAttrs } ->
            SortableColumn { label = label, viewData = viewData, cellAttrs = cellAttrs, sortMsg = sortMsg }

        SortableColumn sortableData ->
            SortableColumn { sortableData | sortMsg = sortMsg }



-- *************
-- Configuration
-- *************


{-| Stores the table configuration. Do NOT store this in your application model. Define the config (using the supplied helper methods) once and pass it to the Table's `view` function.
-}
type Config data msg
    = Config
        { columns : List (Column data msg)
        , toId : Maybe (data -> String)
        , customizations : Customizations data msg
        }


{-| Creates a table configuration without uniquely identifiable rows
Accepts:
a list of Column definitions
a set of Customizations for the table
-}
createConfig : List (Column data msg) -> Customizations data msg -> Config data msg
createConfig columns customizations =
    Config { columns = columns, toId = Nothing, customizations = customizations }


{-| Creates a table configuration with uniquely identifiable rows.
The toId function is used to uniquely identify a row based on it's data.
This is to enable use of Html.Keyed
-}
createKeyedConfig :
    (data -> String)
    -> List (Column data msg)
    -> Customizations data msg
    -> Config data msg
createKeyedConfig toId columns customizations =
    Config { columns = columns, toId = Just toId, customizations = customizations }



-- **************
-- Customizations
-- **************


{-| Stores a structure of customizations to be used in the table display. Can be constructed using the provided helper methods.
-}
type Customizations data msg
    = Customizations
        { tableAttrs : List (Attribute msg)
        , theadAttrs : List (Attribute msg)
        , tbodyAttrs : List (Attribute msg)
        , sortAttrs : List (Attribute msg)
        , rowAttrs : data -> List (Attribute msg)
        }


{-| Creates a set of default Customizations (which are no customizations at all)
This is a useful starting point for chaining specific customizations.

    defaultCustomizations
        |> customizeBody [ style [("backgroundColor", "pink")] ]
        |> customizeTable []
        -- and so on

-}
defaultCustomizations : Customizations data msg
defaultCustomizations =
    Customizations
        { tableAttrs = []
        , theadAttrs = []
        , tbodyAttrs = []
        , sortAttrs = []
        , rowAttrs = \_ -> []
        }


{-| Add html attributes to customize the <table> tag
-}
customizeTable : List (Attribute msg) -> Customizations data msg -> Customizations data msg
customizeTable attrs (Customizations current) =
    Customizations { current | tableAttrs = attrs }


{-| Add html attributes to customize the <thead> tag
-}
customizeHead : List (Attribute msg) -> Customizations data msg -> Customizations data msg
customizeHead attrs (Customizations current) =
    Customizations { current | theadAttrs = attrs }


{-| Add html attributes to the <tbody> tag
-}
customizeBody : List (Attribute msg) -> Customizations data msg -> Customizations data msg
customizeBody attrs (Customizations current) =
    Customizations { current | tbodyAttrs = attrs }


{-| Add html attributes to the <span> that contains the sort arrow in the header
-}
customizeSort : List (Attribute msg) -> Customizations data msg -> Customizations data msg
customizeSort attrs (Customizations current) =
    Customizations { current | sortAttrs = attrs }


{-| Adds html attributes to the <tr> tag. This customization is somewhat different than the other customizations because you must define a function that will be given the data for the specific row. This will let you customize rows based on the data it contains.
-}
customizeRows : (data -> List (Attribute msg)) -> Customizations data msg -> Customizations data msg
customizeRows customRow (Customizations current) =
    Customizations { current | rowAttrs = customRow }



-- ****
-- View
-- ****


{-| Use this if you don't care about any of the columns sorting.
-}
nonSortingView : Config data msg -> List data -> Html msg
nonSortingView config dataItems =
    tableView config Nothing dataItems


{-| The standard view function for creating the html that will produce the table with columns defined and data given.

Accepts:
the config which defines the columns & customizations for the table
the SortState which should be stored in your app model - and holds the current sort state of the table
a lit of data that will be displayed in the table

-}
view : Config data msg -> SortState -> List data -> Html msg
view config sort dataItems =
    tableView config (Just sort) dataItems


tableView : Config data msg -> Maybe SortState -> List data -> Html msg
tableView (Config { columns, toId, customizations }) maybeSort dataItems =
    let
        (Customizations { tableAttrs, theadAttrs, tbodyAttrs, rowAttrs }) =
            customizations
    in
    Html.table tableAttrs
        [ thead theadAttrs
            [ List.map (headerColumns maybeSort customizations) columns
                |> tr []
            ]
        , case toId of
            Just rowId ->
                List.map (keyedTableRow columns rowId rowAttrs) dataItems
                    |> Html.Keyed.node "tbody" tbodyAttrs

            Nothing ->
                List.map (tableRow columns rowAttrs) dataItems
                    |> tbody tbodyAttrs
        ]


headerColumns : Maybe SortState -> Customizations data msg -> Column data msg -> Html msg
headerColumns maybeSort (Customizations { sortAttrs }) col =
    case col of
        SimpleColumn { label } ->
            plainHeaderCell label

        SortableColumn { label, sortMsg } ->
            case maybeSort of
                Nothing ->
                    plainHeaderCell label

                Just ((SortState colId sort) as sortState) ->
                    if label == colId then
                        th
                            [ sortClick (reverseSort sortState) sortMsg
                            , Attrs.class "header-sort"
                            , Attrs.style [ ( "cursor", "pointer" ) ]
                            ]
                            [ span [] [ text label ]
                            , span [ Attrs.class "sort-arrow" ] [ sortArrow sortAttrs sort ]
                            ]
                    else
                        th
                            [ sortClick (changeSort label) sortMsg
                            , Attrs.class "header-sort"
                            , Attrs.style [ ( "cursor", "pointer" ) ]
                            ]
                            [ text label ]


sortClick : SortState -> (SortState -> msg) -> Attribute msg
sortClick newState sortMsg =
    sortMsg newState
        |> JD.succeed
        |> Events.on "click"


plainHeaderCell : String -> Html msg
plainHeaderCell label =
    th [] [ text label ]


sortArrow : List (Attribute msg) -> SortDir -> Html msg
sortArrow sortAttrs sort =
    case sort of
        Asc ->
            Icons.upArrow sortAttrs

        Desc ->
            Icons.downArrow sortAttrs


keyedTableRow :
    List (Column data msg)
    -> (data -> String)
    -> (data -> List (Attribute msg))
    -> data
    -> ( String, Html msg )
keyedTableRow columns toId rowAttrs dataItem =
    List.map extractColumnData columns
        |> List.map (tableCell dataItem)
        |> (\cells -> ( toId dataItem, tr (rowAttrs dataItem) cells ))


tableRow : List (Column data msg) -> (data -> List (Attribute msg)) -> data -> Html msg
tableRow columns rowAttrs dataItem =
    List.map extractColumnData columns
        |> List.map (tableCell dataItem)
        |> tr (rowAttrs dataItem)


tableCell : data -> ( data -> Html msg, List (Attribute msg) ) -> Html msg
tableCell dataItem ( viewData, cellAttrs ) =
    td cellAttrs [ viewData dataItem ]


extractColumnData : Column data msg -> ( data -> Html msg, List (Attribute msg) )
extractColumnData column =
    case column of
        SimpleColumn { viewData, cellAttrs } ->
            ( viewData, cellAttrs )

        SortableColumn { viewData, cellAttrs } ->
            ( viewData, cellAttrs )
