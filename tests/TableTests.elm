module TableTests exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import SortableTable as Table
import Test exposing (..)


-- *******
-- Fuzzers
-- *******


sortDirFuzzer : Fuzzer Table.SortDir
sortDirFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Table.Asc
        , Fuzz.constant Table.Desc
        ]


type alias TableRecord =
    { idVal : Int
    , floatVal : Float
    , boolVal : Bool
    , stringVal : String
    }


tableRecordFuzzer : Fuzzer TableRecord
tableRecordFuzzer =
    Fuzz.map TableRecord Fuzz.int
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.string


tableDataFuzzer : Fuzzer (List TableRecord)
tableDataFuzzer =
    Fuzz.list tableRecordFuzzer



-- *****
-- Tests
-- *****


suite : Test
suite =
    describe "Sortable Table Tests"
        [ fuzz2 Fuzz.string sortDirFuzzer "sortInfo" <|
            \colId sortDir ->
                Table.initSortState colId sortDir
                    |> Table.sortInfo
                    |> Expect.equal ( colId, sortDir )
        ]
