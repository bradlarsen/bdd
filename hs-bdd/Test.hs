import BDD
import Debug.Trace

main :: IO ()
main = newMgrWithHint 100 100 $ \mgr -> do
    trace "a" $ do
    nodesUsed mgr >>= print
    trace "b" $ do
    var2 mgr
    trace "c" $ do
    nodesUsed mgr >>= print
    trace "d" $ do
    bdd <- crash mgr
    bddNot mgr bdd print
    return ()

var2 :: Mgr s -> IO ()
var2 mgr = do
    bddIthVar mgr 2 $ \v2 -> do
    print v2

crash :: Mgr s -> IO (Bdd s)
crash mgr = bddIthVar mgr 2 return
