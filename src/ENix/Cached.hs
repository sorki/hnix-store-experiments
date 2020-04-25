{-# LANGUAGE OverloadedStrings   #-}
module ENix.Cached where

import Control.Monad
import Data.Bifunctor
import Data.Either
import qualified Data.List

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Attoparsec.Text
import Data.ByteUnits

import System.Nix.StorePath
import Nix.NarInfo

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status

nixosCache :: BSC.ByteString
nixosCache = "cache.nixos.org"

nixosStoreRoot :: String
nixosStoreRoot = "/nix/store"

inNixosCache = inCache nixosCache

-- | Query cache over HTTP and check if `StorePath`
-- is present
inCache :: BSC.ByteString
        -> Manager
        -> StorePath
        -> IO (Either String Bool)
inCache whichCache conMan p = do
  let narinfoPath = storePathToNarInfo p
      r = defaultRequest {
              host = whichCache
            , port = 443
            , path = narinfoPath
            , secure = True
            }

  res <- httpLbs r conMan
  --print (statusCode $ responseStatus res, narinfoPath)
  case statusCode $ responseStatus res of
    200 -> return $ pure True
    404 -> return $ pure False
    err -> return $ Left $ "Server " ++ (BSC.unpack whichCache) ++ " responded with error code " ++ (show err)

-- | Fetch narinfo
--
-- Returns Righ Just `SimpleNarInfo` in case of success, Right Nothing
-- if path is missing or Left error for other HTTP codes.
fetchNarInfo :: BSC.ByteString
             -> Manager
             -> StorePath
             -> IO (Either String (Maybe SimpleNarInfo))
fetchNarInfo whichCache conMan p = do
  let narinfoPath = storePathToNarInfo p
      r = defaultRequest {
              host = whichCache
            , port = 443
            , path = narinfoPath
            , secure = True
            }

  res <- httpLbs r conMan
  case statusCode $ responseStatus res of
    200 -> do
      let ni = parseOnly parseNarInfo
                  $ T.pack $ BSC.unpack $ BSL.toStrict $ responseBody res
      return $ second Just ni
    404 -> return $ Right Nothing
    err -> return $ Left $ "Server " ++ (BSC.unpack whichCache) ++ " responded with error code " ++ (show err)

-- | Pretty printer for human-readable bytes value of `NarInfo`
prettySize :: Integer -> String
prettySize bytesVal = getShortHand . getAppropriateUnits
  $ ByteValue (fromIntegral bytesVal) Bytes

-- | Construct NAR URL from cacheDomain and `NarInfo`
narURL :: BSC.ByteString
       -> SimpleNarInfo
       -> T.Text
narURL cacheDomain narInfo = T.concat [
    "https://"
  , T.pack $ BSC.unpack $ cacheDomain
  , "/"
  , url narInfo
  ]

-- | Parse path from ByteString and check if it is cached
pathParseCheckCached :: String         -- expected store path prefix
                     -> BSC.ByteString -- cache domain
                     -> BSC.ByteString -- store path
                     -> IO (Either String Bool)
pathParseCheckCached storeRoot cacheDomain strPath = do
  m <- newManager tlsManagerSettings
  let pth = parsePath storeRoot strPath
  case pth of
    Left e -> return $ Left $ "Path parse error - " ++ e
    Right p -> inCache cacheDomain m p

-- | Run cache check on a list of `StorePath`s.
testPaths' :: BSC.ByteString
           -> [StorePath]
           -> IO [(StorePath, Either String Bool)]
testPaths' cacheDomain ps = withTLS $ \man -> do
  eps <- mapM (\pth -> (,) <$> pure pth <*> inCache cacheDomain man pth)
    (Data.List.nub ps)
  return eps

-- | Run cache check on a list of `StorePath`s.
--
-- Returns list of paths that were successfuly checked
-- with the result or string with merged errors.
testPaths :: BSC.ByteString
          -> [StorePath]
          -> IO (Either String [(StorePath, Bool)])
testPaths cacheDomain ps = do
  eps <- testPaths' cacheDomain ps
  case any (isLeft . snd) eps of
    True -> return $ Left
      $ "Error(s) occurred: "
      ++ ((unlines . map (fromLeft "" . snd) . filter (isLeft . snd)) $ eps)
    False -> return $ Right
      $ map (\(p, Right res) -> (p, res))
      $ filter (isRight . snd) eps

testFilterPaths fn cacheDomain ps = do
  res <- testPaths cacheDomain ps
  case res of
    Right paths -> return $ Right $ map fst $ filter (fn . snd) paths
    Left er -> return $ Left er


-- | Filter paths available in cache
cachedPaths :: BSC.ByteString
            -> [StorePath]
            -> IO (Either String [StorePath])
cachedPaths = testFilterPaths id

-- | Filter paths _not_ in cache
uncachedPaths :: BSC.ByteString
              -> [StorePath]
              -> IO (Either String [StorePath])
uncachedPaths = testFilterPaths not

-- | Parse and filter paths _not_ in cache
uncachedPathsParse :: String            -- expected store path prefix
                   -> BSC.ByteString    -- cache domain
                   -> [BSC.ByteString]  -- paths to parse
                   -> IO (Either String [StorePath])
uncachedPathsParse storeRoot cacheDomain strs = withTLS $ \man -> do
  let pths = map (parsePath storeRoot) strs
  when (any isLeft pths) $ do
    error $ "Path parse error(s): "
          ++ (unlines . lefts $ pths)

  uncachedPaths cacheDomain $ rights pths

withTLS act = newManager tlsManagerSettings >>= act

-- stdin filter used by app/InCache
main = do
  x <- BSC.getContents
  y <- uncachedPathsParse nixosStoreRoot nixosCache
        $ filter (not . (".drv" `BSC.isSuffixOf`)) -- XXX: questionable
        $ BSC.lines x
  case y of
    Left er -> error $ "Error occured: " ++ er
    Right ps -> mapM_ (BSC.putStrLn . storePathToRawFilePath) ps

--
-- DRAGONS
--
set = defaultManagerSettings
  { managerConnCount = 1000 }
withMan act = newManager set >>= act

--inCache :: StorePath -> IO Bool
inCacheNoTLS man p = do
  let narinfoPath = storePathToNarInfo p
      r = defaultRequest {
              host = nixosCache
            , path = narinfoPath
            }

  res <- httpLbs r man
  print (statusCode $ responseStatus res, narinfoPath)
  case statusCode $ responseStatus res of
    200 -> return True
    _   -> return False


cachedPathsNoTLS ps = withMan $ \man -> filterM (inCacheNoTLS man) ps
cachedPathsParseNoTLS ps = withMan $ \man -> filterM (\raw -> do
  case parsePath nixosStoreRoot raw of
    Left er -> error er
    Right x -> inCacheNoTLS man x
  ) ps

-- LAZY
sample_pth = let Right p = parsePath "/nix/store" "/nix/store/x5m45fcnky99r0k41kmdwmjb7zw5k4z4-binutils-2.31.1"
  in p

sample_pthFake = let Right p = parsePath "/nix/store" "/nix/store/y5m45fcnky99r0k41kmdwmjb7zw5k4z4-binutils-2.31.1"
  in p
