# SortableTable

This package is very similar to (and draws most of the insipiration from) [Evan's Sortable Table](https://github.com/evancz/elm-sortable-table) package.  However this package allows you to perform an external command on sort/re-sort as necessary.  While Evan's sortable table is does the sorting for you with the data in memory, this will not work for a sufficienty large dataset that requires pagination from the server. 

The `SortableTable`'s view function takes a table config and `SortState` that again is very simlar to Evan's with a few tweaks.  For instance, the `Config` requires a `(msg -> SortState)` function that describes a Msg that will be fired containing the new `SortState` that will allow you to perform additional request etc. as needed. 

Please take a look at the documentation in the SortableTable module. As well as the [small example](examples/src/TableExample.elm).  Additionaly there is a small example below.


## Installation

```bash
elm-package install volumeint/sortable-table
```

## Usage

```elm

import SortableTable as Table

-- Message

type Msg
  = ReSort Table.SortState


-- Model

  { sortState : Table.SortState
  , cities : List { city : String, country : String, population : Int }
  }


-- Config

{-| Data records look like this - so defining columns below 

  { city : String
  , country : String
  , population : Int
  }

-}

tableConfig : Table.Config Msg
tableConfig = 
  Table.createConfig
    [ Table.defineColumn "City" (text << .city) [] 
    , Table.defineColumn "Country" (text << .country) []
    , Table.defineColumn "Population" (text << toString << .population) []
      |> Table.makeSortable ReSort
    ]
    Table.defaultCusomizations


-- View

view : Model -> Html Msg
view { sortState, citites } =
  div [] [ Table.view tableConfig sortState cities ]


```

