module Args where

import Data.List
import Text.Read (readMaybe)
import Result
import qualified Data.List as List
import Control.Exception (SomeException (SomeException), catch)

data Args = Args
  { argImageConfigFile :: Maybe String, -- ^ Given with the `-imageConfigFile` argument
    argSceneFile :: Maybe String, -- ^ Given with the `-sceneFile` argument
    argOutFile :: Maybe String, -- ^ Given with the `-outFile` argument
    argNrSamples :: Maybe Int -- ^ Given with the `-imageNrSamples` argument
  }
  deriving (Eq, Show)

data ParseArgsError = InvalidArgs deriving (Eq, Show)

type ArgMap = [(String, String)]

-- >>> toArgMap ["-x", "y"]
-- Success [("x","y")]
--
-- >>> toArgMap ["-x", "y", "-a", "b"]
-- Success [("x","y"),("a","b")]
--
-- >>> toArgMap ["x", "y"]
-- Error InvalidArgs
--
-- >>> toArgMap ["-x", "y", "-z"]
-- Error InvalidArgs
toArgMap :: [String] -> Result ParseArgsError ArgMap
toArgMap args
  | List.length args `mod` 2 == 1 = Error InvalidArgs
  | any (not . isPrefixOf "-") (map snd $ filter (even . fst) $ zip [0..] args) = Error InvalidArgs
  | otherwise = Success $ List.zip keys values
  where
    keys = map (dropWhile (== '-')) $ filter (isPrefixOf "-") args
    values = filter (not . isPrefixOf "-") args

-- >>> getArg "key" [("key", "value")]
-- Just "value"
getArg :: String -> ArgMap -> Maybe String
getArg key argMap = 
  case argMap of
    [] -> Nothing
    x:xs -> if  fst x == key then 
      Just (snd x)
      else
        getArg key xs


-- >>> readArg "name" [("name", "1")] :: Maybe Int
-- Just 1
--
-- >>> readArg "name" [("name", "one")] :: Maybe Int
-- Nothing
--
-- >>> readArg "number" [("name", "1")] :: Maybe Int
-- Nothing
readArg :: (Read a) => String -> ArgMap -> Maybe a
readArg key args = 
  case getArg key args of
    Nothing -> Nothing
    Just x -> case reads x of
              [(x, "")] -> Just x
              _         -> Nothing


-- >>> procArgs ["-imageNrSamples", "200", "-outFile", "image.bmp"]
-- Success (Args {argImageConfigFile = Nothing, argSceneFile = Nothing, argOutFile = Just "image.bmp", argNrSamples = Just 200})
procArgs :: [String] -> Result ParseArgsError Args
procArgs args = 
  case toArgMap args of 
    Error _ -> Error InvalidArgs
    Success argMap -> Success Args 
      { argImageConfigFile = getArg "imageConfigFile" argMap,
        argSceneFile = getArg "sceneFile" argMap,
        argOutFile = getArg "outFile" argMap,
        argNrSamples = readArg "imageNrSamples" argMap
      }
