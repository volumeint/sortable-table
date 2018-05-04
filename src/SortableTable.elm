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


type SortDir
    = Asc
    | Desc


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
-}
sortInfo : SortState -> ( String, SortDir )
sortInfo (SortState colId sortDir) =
    ( colId, sortDir )


{-| Create an initial SortState
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


{-| Creates an unsortable column
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


type Config data msg
    = Config
        { columns : List (Column data msg)
        , toId : Maybe (data -> String)
        , customizations : Customizations data msg
        }


{-| Creates a table configuration without uniquely identifiable rows
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


type Customizations data msg
    = Customizations
        { tableAttrs : List (Attribute msg)
        , theadAttrs : List (Attribute msg)
        , tbodyAttrs : List (Attribute msg)
        , sortAttrs : List (Attribute msg)
        , rowAttrs : data -> List (Attribute msg)
        }


defaultCustomizations : Customizations data msg
defaultCustomizations =
    Customizations
        { tableAttrs = []
        , theadAttrs = []
        , tbodyAttrs = []
        , sortAttrs = []
        , rowAttrs = \_ -> []
        }


customizeTable : List (Attribute msg) -> Customizations data msg -> Customizations data msg
customizeTable attrs (Customizations current) =
    Customizations { current | tableAttrs = attrs }


customizeHead : List (Attribute msg) -> Customizations data msg -> Customizations data msg
customizeHead attrs (Customizations current) =
    Customizations { current | theadAttrs = attrs }


customizeBody : List (Attribute msg) -> Customizations data msg -> Customizations data msg
customizeBody attrs (Customizations current) =
    Customizations { current | tbodyAttrs = attrs }


customizeSort : List (Attribute msg) -> Customizations data msg -> Customizations data msg
customizeSort attrs (Customizations current) =
    Customizations { current | sortAttrs = attrs }


customizeRows : (data -> List (Attribute msg)) -> Customizations data msg -> Customizations data msg
customizeRows customRow (Customizations current) =
    Customizations { current | rowAttrs = customRow }



-- ****
-- View
-- ****


nonSortingView : Config data msg -> List data -> Html msg
nonSortingView config dataItems =
    tableView config Nothing dataItems


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
