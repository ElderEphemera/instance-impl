{-# OPTIONS_GHC -fplugin=InstanceImpl #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad (unless)
import Data.String (IsString(fromString))
import System.Exit (die)

main :: IO ()
main = do
  let pt = Pt 3 (4::Double)
  unless (pt.r == 5) . die $
    "ptr.r == " ++ show pt.r ++ " /= 5"

  unless (pt.display == "<3.0,4.0>") . die $
    "ptr.display == \"" ++ pt.display ++ "\" /= \"<3.0,4.0>\""

data Pt a = Pt { x, y :: !a }

instance impl (Pt a) where
  x' :: Real a => Double
  x' = realToFrac self.x

  r :: Floating a => a
  r = sqrt $ sq self.x + sq self.y
    where sq v = v*v

  display :: (Show a, IsString str) => str
  display = fromString $ "<" ++ show self.x ++ "," ++ show self.y ++ ">"
