module Types exposing (..)


-- The current page can be either the Overview, or a Team (with a name)
type Page
  = Overview
  | Team String


-- Data for an individual team
type alias TeamData =
  { author: String
  , copy : List (String, CopySectionData)
  , name : String
  , transfers : List TransferData
  }


-- Data for an individual copy section within a team page
type alias CopySectionData =
  { copy : String
  , heading : String
  , image : String
  , player : String
  }


-- Data for an individual transfer
type alias TransferData =
  { player : String
  , transferType : TransferType
  , location : String
  , feeStr : String
  , feeNum : Maybe Float
  , signedFrom : Maybe String
  , nationality : Maybe String
  , region : Maybe String
  , age : Maybe Int
  , position : Maybe String
  }


-- A tranfer is either in or out
type TransferType = In | Out


-- Data for one of the 'top ten' items
type alias TopTenData =
  { fee : String
  , from : String
  , image : String
  , player : String
  , to : String
  }


-- Options defining a bar chart
type alias BarChartOptions =
  { heading : String
  , className : String
  , items : BarChartItems
  }


-- (label, value) data pairs for a bar chart
type alias BarChartItems =
  List (String, Float)


-- Options defining a double bar chart
type alias DoubleBarChartOptions =
  { className : String
  , formatPrefix : String
  , formatSuffix : String
  , totalLabelA : String
  , totalLabelB : String
  , items : DoubleBarChartItems
  }


-- (label, value, value) data triples for a double bar chart
type alias DoubleBarChartItems =
  List (String, Float, Float)


-- Options defining the world map chart
type alias MapChartOptions =
  { heading : String
  , items : MapChartItems
  }


-- (region, nationality, nationality count) triples for the world map chart
type alias MapChartItems = 
  List ( String, String, Int )
