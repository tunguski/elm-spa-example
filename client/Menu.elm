module Menu exposing (menuDefinition)

import Member.Member as Member
import Model exposing (..)
import Msg exposing (..)
import Report.Report as Report
import Task.Task as Task


menuDefinition : Menu
menuDefinition =
    { small = False
    , expanded = False
    , def =
        [ { title = "Tasks"
          , elements =
                [ { title = "Search"
                  , menuEntry = ME_Task Task.Search
                  , icon = "search"
                  }
                , { title = "Add"
                  , menuEntry = ME_Task Task.Add
                  , icon = "tasks"
                  }
                ]
          }
        , { title = "Members"
          , elements =
                [ { title = "Search"
                  , menuEntry = ME_Member Member.Search
                  , icon = "search"
                  }
                , { title = "Add"
                  , menuEntry = ME_Member Member.Add
                  , icon = "user-plus"
                  }
                ]
          }
        , { title = "Reports"
          , elements =
                [ { title = "General"
                  , menuEntry = ME_Report Report.Search
                  , icon = "pie-chart"
                  }
                ]
          }
        ]
    }
