{-# LANGUAGE RankNTypes #-}

module Edge where

import Control.Arrow (Arrow(arr, first), ArrowChoice(left))
import Control.Category (Category((.), id))
import Control.Monad ((>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (get, put)
import Pipes (await)
import Pipes.Core (Pipe, request, respond, (\>\), (/>/), push, (>~>))
import Pipes.Internal (unsafeHoist)
import Pipes.Lift (evalStateP)
import Prelude hiding ((.), id)

newtype Edge m r a b = Edge { unEdge :: a -> Pipe a b m r }

instance (Monad m) => Category (Edge m r) where
    id  = Edge push
    (Edge p2) . (Edge p1) = Edge (p1 >~> p2)

instance (Monad m) => Arrow (Edge m r) where
    arr f = Edge (push />/ respond . f)
    first (Edge p) = Edge $ \(b, d) ->
        evalStateP d $ (up \>\ unsafeHoist lift . p />/ dn) b
      where
        up () = do
            (b, d) <- request ()
            lift $ put d
            return b
        dn c = do
            d <- lift get
            respond (c, d)

instance (Monad m) => ArrowChoice (Edge m r) where
    left (Edge k) = Edge (bef >=> (up \>\ (k />/ dn)))
      where
          bef x = case x of
              Left  b -> return b
              Right d -> do
                  _ <- respond (Right d)
                  x2 <- request ()
                  bef x2
          up () = do
              x <- request ()
              bef x
          dn c = respond (Left c)

runEdge :: (Monad m) => Edge m r a b -> Pipe a b m r
runEdge e = await >>= unEdge e
