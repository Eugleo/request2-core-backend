{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.Id.AddId where

import Data.Coerce (Coercible)
import Data.Functor.Contravariant
import Database.Selda
import GHC.Generics (Generic (..))
import Generic.Data (gcoerce)
import Generic.Data.Surgery
import Generic.Data.Surgery.Internal (Arborify, Linearize)


type AddId a b w =
    ( Generic a,
      Generic b,
      ToOR_ a,
      FromOR_ w b,
      InsRField "_id" 0 (ID b) w (Linearize (Rep a))
    )


addId ::
    forall a b w.
    ( Generic a,
      Generic b,
      ToOR_ a,
      FromOR_ w b,
      InsRField "_id" 0 (ID b) w (Linearize (Rep a))
    ) =>
    ID b ->
    a ->
    b
addId _id = gcoerce . fromOR' . insertRField @"_id" @0 . (,) _id . toOR


-- Internals

type ToOR_ a = (ToOR (Rep a) (Linearize (Rep a)))


type FromOR_ w b =
    ( FromOR (Arborify w) w,
      Functor (Arborify w),
      Contravariant (Arborify w),
      Linearize (Arborify w) ~ w,
      Coercible (Arborify w) (Rep b)
    )
