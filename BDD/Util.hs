module BDD.Util where

import BDD.Raw
import Control.Exception (finally)

-- | Brackets an action that requires a BDD manager with manager
-- creation and destruction.
withBddMgr
  :: Int                  -- ^ the number of variables
  -> (BddMgr -> IO a)     -- ^ the action to run
  -> IO a
withBddMgr nVars act = do
    mgr <- bdd_mgr_create (fromIntegral nVars)
    act mgr `finally` bdd_mgr_destroy mgr
