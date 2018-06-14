import Zerem.Prelude
import qualified Zerem as Z
import Gauge
import Data.List (foldl')
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = defaultMain
  [ bgroup "sum" $
    let bench' name f = bench name $ whnf f (100000 :: Int) in
    [ bench' "zerem Identity" $ \high ->
        runIdentity $ Z.sum $ Z.enumFromTo 1 high
    , bench' "zerem ST" $ \high ->
        runST $ Z.sum $ Z.enumFromTo 1 high
    , bench' "Prelude.sum" $ \high -> sum [1..high]
    , bench' "foldl' on lists" $ \high -> foldl' (+) 0 [1..high]
    , bench' "foldl' on boxed vector" $ \high ->
        VB.foldl' (+) 0 $ VB.enumFromTo 1 high
    , bench' "foldl' on unboxed vector" $ \high ->
        VU.foldl' (+) 0 $ VU.enumFromTo 1 high
    ]
  ]
