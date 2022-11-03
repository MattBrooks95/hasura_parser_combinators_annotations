module Json where

import qualified Data.HashMap.Lazy as HM

data JValue = JObject (HM.HashMap String JValue)
    | JArray [JValue]
    | JString String
    | JNumber Double
    | JBool Bool
    | JNull
    deriving Show
