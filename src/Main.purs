module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List (List)
import Data.List as List
import Data.Monoid (mempty)
import Data.Traversable (traverse_)
import Type.Prelude (class AppendSymbol, class IsSymbol, class RowToList, Proxy(Proxy), SProxy(SProxy), reflectSymbol)
import Type.Row (Cons, Nil, kind RowList)

foreign import kind SList
foreign import data SNil :: SList
foreign import data SCons :: Symbol -> SList -> SList

data SLProxy (xs :: SList) = SLProxy

class SListAppend (l :: SList) (r :: SList) (o :: SList)
  | l r -> o
instance nilSLA :: SListAppend SNil o o
instance consSLA ::
  ( SListAppend tail r o'
  ) => SListAppend (SCons s tail) r (SCons s o')

class SListPrefixItems (prefix :: Symbol) (i :: SList) (o :: SList)
  | prefix i -> o
instance nilSLPI :: SListPrefixItems prefix SNil SNil
instance consSLPI ::
  ( SListPrefixItems prefix tail o'
  , AppendSymbol prefix "." prefix'
  , AppendSymbol prefix' s s'
  ) => SListPrefixItems prefix (SCons s tail) (SCons s' o')

class NestedLabels a (xs :: SList) | a -> xs
instance zzzNL :: NestedLabels a SNil
instance recordNL ::
  ( RowToList row rl
  , NestedLabelsFields rl xs
  ) => NestedLabels { | row } xs

class NestedLabelsFields (rl :: RowList) (xs :: SList)
  | rl -> xs
instance nilNLF :: NestedLabelsFields Nil SNil
instance consNLF ::
  ( IsSymbol name
  , NestedLabels ty children
  , SListPrefixItems name children children'
  , NestedLabelsFields tail rest
  , SListAppend (SCons name children') rest result
  ) => NestedLabelsFields (Cons name ty tail) result

class SListToStrings (xs :: SList) where
  sListToStrings :: SLProxy xs -> List String
instance nilSLTS :: SListToStrings SNil where
  sListToStrings _ = mempty
instance consSLTS ::
  ( IsSymbol s
  , SListToStrings tail
  ) => SListToStrings (SCons s tail) where
  sListToStrings _ =
    let
      first = reflectSymbol (SProxy :: SProxy s)
      rest = sListToStrings (SLProxy :: SLProxy tail)
    in
      List.Cons first rest

nestedLabels
  :: forall a xs
   . NestedLabels a xs
  => Proxy a
  -> SLProxy xs
nestedLabels _ = SLProxy

type MyRecord =
  { apple ::
  { banana ::
  { cherry ::
  { dairy ::
  { eagle ::
  { thing :: String }}}}}}

-- this has 6 overlapping instance warnings:
labels
  :: SLProxy
      (SCons "apple"
      (SCons "apple.banana"
      (SCons "apple.banana.cherry"
      (SCons "apple.banana.cherry.dairy"
      (SCons "apple.banana.cherry.dairy.eagle"
      (SCons "apple.banana.cherry.dairy.eagle.thing" SNil))))))
labels = nestedLabels (Proxy :: Proxy MyRecord)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  traverse_ log (sListToStrings labels)

-- apple
-- apple.banana
-- apple.banana.cherry
-- apple.banana.cherry.dairy
-- apple.banana.cherry.dairy.eagle
-- apple.banana.cherry.dairy.eagle.thing
