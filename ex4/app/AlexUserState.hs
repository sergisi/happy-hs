-- | Definition of AlexUserState
-- | This is in another file for legacy reasons
module AlexUserState
  (
-- * State mantained
    AlexUserState (..),
-- * Initial State
    alexInitUserState,
  )
where
{- | Current free state of the machine
         If it is a new machine, it will be 0, and it will keep adding as
         the states are needed.
     -}
type AlexUserState = Int

{-| Initial State of Alex monad

>>> alexInitUserState
0

-}
alexInitUserState :: AlexUserState
alexInitUserState = 0
