{-# OPTIONS_GHC -fplugin=InstanceImpl #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Control.Monad (unless)
import System.Exit

main :: IO ()
main = do
  let pt = Pt 3 4
  unless (pt.r == 5) . die $
    "ptr.r == " ++ show pt.r ++ " /= 5"

data Pt = Pt { x, y :: !Double }

instance impl Pt where
  r :: Double
  r = sqrt $ sq self.x + sq self.y
    where sq v = v*v

  θ :: Double
  θ = atan2 self.y self.x
