{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Skeleton.Kernel.Core.Secure (
         saltValue
       , authPass
       ) where

import  qualified Data.Text.Lazy                        as T
import  qualified Data.ByteString.Lazy                  as L
import  qualified Data.ByteString.Char8                 as C
import            Data.Digest.Pure.MD5
import            System.Random
import            Skeleton.Kernel.Internal.Model


md5Encrypt :: String
           -> T.Text
md5Encrypt pass'' = let pass' = L.fromStrict $ C.pack pass'' in
                    T.pack . show $ md5 pass'


saltValue :: IO T.Text
saltValue = do
  gen  <- getStdGen
  let (rand :: Int, _) = random gen
  return . T.pack $ show rand


authPass :: User
         -> T.Text
         -> T.Text
         -> Bool
authPass (User _ _ p1 _ _ _) salt' = 
  let value = md5Encrypt $ (++) (T.unpack p1) (T.unpack salt') in
  (==) value
