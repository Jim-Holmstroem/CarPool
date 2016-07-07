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

import           Text.Read
import           Data.Text (Text(..))
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)

import           Data.UUID
import           Data.Time.Clock
import           Data.Time.ISO8601

import           Text.Blaze
import           Text.Blaze.Html5 (Html(..))
import           Text.Blaze.Html5 ((!))
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


numberInput name = H.input ! A.type_ "number" ! A.name name ! A.value "0" ! A.style "direction: rtl;"


submitInput :: Html
submitInput = H.input ! A.type_ "submit" ! A.value "Submit"


hiddenUsersInput (user:users) = do
    H.input ! A.type_ "hidden" ! A.name "user" ! A.value (H.stringValue . BSC8.unpack $ user)
    hiddenUsersInput users
hiddenUsersInput [] = return ()


selectMultiple name options = H.select ! A.name "user" ! A.multiple "" $ mapM_ selectOption options
    where selectOption name = H.option ! A.value (H.stringValue $ BSC8.unpack name) $ H.toHtml $ BSC8.unpack name


noUsers :: Connection -> ByteString -> Snap ()
noUsers conn poolId = do
    response <- liftIO $ runRedis conn $ smembers (BSC8.append "users:" poolId)

    case response of
        Right users -> writeHtmlBody $ do
            H.h1 "Select Users"
            H.hr
            H.form ! A.action (H.stringValue $ "/pools/" ++ BSC8.unpack poolId) ! A.method "GET" $ do
                selectMultiple "user" users
                H.br
                submitInput
        Left (Error message) -> writeError $ decodeUtf8 message
        Left _ -> writeError "Unknown Error"


confirm :: Connection -> Snap ()
confirm conn = do
    Just poolId <- getParam "poolId"

    Just use <- getQueryParam "use"
    Just cost <- getQueryParam "cost"

    usersRaw <- getQueryParam "user"  -- FIXME code duplication
    let users = maybe [] splitOnSpace $ usersRaw

    writeHtmlBody $ do
        H.h1 $ H.toHtml ("Confirm" :: Text)
        H.hr
        H.toHtml $ Text.concat [ "Users "
                               , decodeUtf8 . BSC8.intercalate "," $ users
                               , " drove "
                               , decodeUtf8 use
                               , "Km "
                               , "and "
                               , "payed "
                               , decodeUtf8 cost
                               , "SEK"
                               ]
        H.hr
        H.form ! A.method "GET" ! A.action (H.stringValue $ "/submit/" ++ BSC8.unpack poolId) $ do
            submitInput


data Trip = Trip { uuid      :: UUID
                 , timestamp :: UTCTime
                 , user      :: BSC8.ByteString
                 , use       :: Double
                 , cost      :: Double
                 }
    deriving (Read, Show, Eq, Generic)


mkTrip :: BSC8.ByteString -> Double -> Double -> IO Trip
mkTrip user use cost = Trip <$> randomIO <*> getCurrentTime <*> return user <*> return use <*> return cost


submit :: Connection -> Snap ()
submit conn = do
    Just poolId <- getParam "poolId"

    Just useRaw <- getQueryParam "use"
    Just costRaw <- getQueryParam "cost"

    let useTotal = read . BSC8.unpack $ useRaw
    let costTotal = read . BSC8.unpack $ costRaw

    usersRaw <- getQueryParam "user"
    let users = maybe [] splitOnSpace $ usersRaw 

    response <- liftIO $ do
        trip <- mkTrip (head users) useTotal costTotal  -- FIXME split trips
        runRedis conn $ multiExec $ do
            lpush (BSC8.append "trips:"  poolId) [Data.UUID.toASCIIBytes . uuid $ trip]
            hmset (BSC8.concat [ "trip:", poolId, ":", Data.UUID.toASCIIBytes . uuid $ trip]) [ ("timestamp", BSC8.pack . formatISO8601 $ timestamp trip)
                                                                                              , ("user", user trip)
                                                                                              , ("use", BSC8.pack . show . use $ trip)
                                                                                              , ("cost", BSC8.pack . show . cost $ trip)
                                                                                              ]

    writeHtmlBody $ do
        H.h1 $ H.toHtml ("Success" :: Text)
        H.hr

        H.form ! A.method "GET" ! A.action (H.stringValue $ "/pools/" ++ BSC8.unpack poolId) $ do
            hiddenUsersInput users
            submitInput

poolUsers :: Connection -> ByteString -> Snap ()
poolUsers conn poolId = do
    usersRaw <- getQueryParam "user"

    let users = maybe [] splitOnSpace $ usersRaw

    case users of
        [] -> noUsers conn poolId
        users -> writeHtmlBody $ do
            H.h1 $ do
                H.toHtml $ decodeUtf8 poolId
                H.toHtml (" :: " :: Text)
                H.toHtml $ Text.intercalate "+" $ map decodeUtf8 users
            H.hr

            H.form ! A.method "GET" ! A.action (H.stringValue $ "/confirm/" ++ BSC8.unpack poolId) $ do
                numberInput "use"
                H.toHtml (" Km" :: Text)
                H.br
                numberInput "cost"
                H.toHtml (" SEK" :: Text)
                H.br
                hiddenUsersInput users
                submitInput
            H.hr
            H.a ! A.href (H.stringValue $ "/pools/" ++ BSC8.unpack poolId) $ do
                H.toHtml ("Set other users" :: Text)


pool :: Connection -> Snap ()
pool conn = do
    poolId <- getParam "poolId"

    case poolId of
        Just poolId -> poolUsers conn poolId
        Nothing -> writeError "PoolId missing"


root :: Connection -> Snap ()
root conn =
    route [ ("pools/:poolId", method GET $ pool conn)
          , ("confirm/:poolId", method GET $ confirm conn)
          , ("submit/:poolId", method GET $ submit conn)
          ]


main :: IO ()
main = connect defaultConnectInfo >>= quickHttpServe . root
