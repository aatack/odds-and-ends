module Fractory.Flow where

data Medium = Item | Chest | Tank
data RealNumber = RealNumber Int Int

data Flow = Flow { item :: String
                 , frequency :: Rational
                 , medium :: Medium
                 }

-- Integrate a flow over a number of seconds.
integrate :: Flow -> Rational -> Rational
integrate flow period = period * (frequency flow)
