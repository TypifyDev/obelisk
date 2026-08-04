module Jenga.Frontend.Shuffle where

import System.Random

-- IO so that we can get randomization
shuffle :: [a] -> IO [a]
shuffle [] = pure []
shuffle xs = do
  picked <- randomRIO (1, length xs) -- so that the length of the first is always 1+
  let (pre, secnd) = splitAt picked xs -- picked is also the length of the first
  case pre of
    []       -> pure secnd -- unreachable: picked >= 1 guarantees pre is non-empty
    (h:rest) -> do
      xs' <- shuffle $ secnd <> rest
      pure $ h : xs'

shuffleWell :: [a] -> IO [a]
shuffleWell (a_:b_:[]) = randomRIO (1,10) >>= \x_ -> if (x_ :: Int) < 6 then pure (a_:b_:[]) else pure (b_:a_:[] )
shuffleWell xs = shuffle =<< pure . reverse =<< shuffle xs
