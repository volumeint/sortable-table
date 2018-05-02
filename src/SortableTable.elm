module Components.SortableTable
    exposing
        ( Column
        , Config
        , SortDir(..)
        , SortState
        , createConfig
        , createKeyedConfig
        , defineColumn
        , initSortState
        , makeSortable
        , nonSortingView
        , sortInfo
        , view
        )

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Html.Styled.Keyed
import Json.Decode as JD
import Styling.Common exposing (inlineIconStyle)


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
    = Config { columns : List (Column data msg), toId : Maybe (data -> String) }


{-| Creates a table configuration without uniquely identifiable rows
-}
createConfig : List (Column data msg) -> Config data msg
createConfig columns =
    Config { columns = columns, toId = Nothing }


{-| Creates a table configuration with uniquely identifiable rows.
The toId function is used to uniquely identify a row based on it's data.
This is to enable use of Html.Keyed
-}
createKeyedConfig : (data -> String) -> List (Column data msg) -> Config data msg
createKeyedConfig toId columns =
    Config { columns = columns, toId = Just toId }



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
tableView (Config { columns, toId }) maybeSort dataItems =
    Html.Styled.table [ Attrs.class "striped bordered sortable-table" ]
        [ thead []
            [ List.map (headerColumns maybeSort) columns
                |> tr []
            ]
        , case toId of
            Just rowId ->
                List.map (keyedTableRow columns rowId) dataItems
                    |> Html.Styled.Keyed.node "tbody" []

            Nothing ->
                List.map (tableRow columns) dataItems
                    |> tbody []
        ]


headerColumns : Maybe SortState -> Column data msg -> Html msg
headerColumns maybeSort col =
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
                            , Attrs.css
                                [ Css.hover [ cursor pointer ]
                                , color (rgba 0 0 0 0.54)
                                , fontSize (px 12)
                                , whiteSpace noWrap
                                ]
                            ]
                            [ span [] [ text label ]
                            , span
                                [ Attrs.class "material-icons"
                                , Attrs.css [ inlineIconStyle ]
                                ]
                                [ text (sortArrow sort) ]
                            ]
                    else
                        th
                            [ sortClick (changeSort label) sortMsg
                            , Attrs.class "header-sort"
                            , Attrs.css
                                [ Css.hover [ cursor pointer ]
                                , color (rgba 0 0 0 0.54)
                                , fontSize (px 12)
                                , whiteSpace noWrap
                                ]
                            ]
                            [ text label ]


sortClick : SortState -> (SortState -> msg) -> Attribute msg
sortClick newState sortMsg =
    sortMsg newState
        |> JD.succeed
        |> Events.on "click"


plainHeaderCell : String -> Html msg
plainHeaderCell label =
    th
        [ Attrs.css
            [ color (rgba 0 0 0 0.54)
            , fontSize (px 12)
            , whiteSpace noWrap
            ]
        ]
        [ text label ]


sortArrow : SortDir -> String
sortArrow sort =
    case sort of
        Asc ->
            "arrow_upward"

        Desc ->
            "arrow_downward"


keyedTableRow : List (Column data msg) -> (data -> String) -> data -> ( String, Html msg )
keyedTableRow columns toId dataItem =
    List.map extractColumnData columns
        |> List.map (tableCell dataItem)
        |> (\cells -> ( toId dataItem, tr [] cells ))


tableRow : List (Column data msg) -> data -> Html msg
tableRow columns dataItem =
    List.map extractColumnData columns
        |> List.map (tableCell dataItem)
        |> tr []


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
