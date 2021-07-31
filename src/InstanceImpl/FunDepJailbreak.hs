{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module InstanceImpl.FunDepJailbreak where

class Jailbreak a | -> a
instance Jailbreak a => Jailbreak a
