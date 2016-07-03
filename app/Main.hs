{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Applicative
import           Data.Traversable
import           Data.Maybe
import           Data.Either

import           GHC.Generics

import           System.Random

import           Data.ByteString.Char8 (ByteString(..))
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy.Char8 as BSLC8

import           Data.Text (Text(..))
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)

import           Data.UUID
import           Data.Time.Clock
import           Data.Time.ISO8601

import           Data.Aeson as JSON
import           Data.Aeson.Types

import           Text.Blaze
import           Text.Blaze.Html5 (Html(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text

import           Database.Redis hiding (decode)

import           Snap
import           Snap.Util.FileServe

import Lib


writeHtmlBody :: Html -> Snap ()
writeHtmlBody = writeLazyText . renderHtml . H.html . H.body


writeError :: Text -> Snap ()
writeError = writeHtmlBody . H.code . H.toHtml

splitOnSpace :: ByteString -> [ByteString]
splitOnSpace = BSC8.splitWith (== ' ')


numberInput name = H.input H.! A.type_ "number" H.! A.name name H.! A.value "0" H.! A.style "direction: rtl;"


hiddenUserInput (user:users) = do
    H.input H.! A.type_ "hidden" H.! A.name "user" H.! A.value (H.stringValue . BSC8.unpack $ user)
    hiddenUserInput users
hiddenUserInput [] = return ()


poolUsers :: Connection -> ByteString -> Snap ()
poolUsers conn poolId = do
    usersRaw <- getQueryParam "user"

    let users = maybe [] splitOnSpace $ usersRaw

    case users of
        [] -> writeError "users missing"
        users -> writeHtmlBody $ do
            H.h1 $ do
                H.toHtml $ decodeUtf8 poolId
                H.toHtml (" :: " :: Text)
                H.toHtml $ Text.intercalate "+" $ map decodeUtf8 users
            H.hr

            H.form H.! A.action (H.stringValue $ "/snoop/" ++ BSC8.unpack poolId) H.! A.method "GET" $ do
                numberInput "travel_distance"
                H.toHtml (" Km" :: Text)
                H.br
                numberInput "fuel_cost"
                H.toHtml (" SEK" :: Text)
                H.br
                hiddenUserInput users
                H.input H.! A.type_ "submit" H.! A.value "Submit"
            H.hr


pool :: Connection -> Snap ()
pool conn = do
    poolId <- getParam "poolId"

    case poolId of
        Just poolId -> poolUsers conn poolId
        Nothing -> writeError "PoolId missing"


root :: Connection -> Snap ()
root conn =
    route [ ("pools/:poolId", method GET $ pool conn)
          ]

main :: IO ()
main = connect defaultConnectInfo >>= quickHttpServe . root
