module Long where

import Control.Concurrent (threadDelay)
import System.IO.Unsafe (unsafePerformIO)

longComputationWhichReturns x = unsafePerformIO (threadDelay 1000000 >> return x)
