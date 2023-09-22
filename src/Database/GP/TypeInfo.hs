{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.GP.TypeInfo
  ( TypeInfo,
    fieldNames,
    fieldTypes,
    constructorName,
    typeInfo,
    HasConstructor (..),
    HasSelectors (..),
  )
where

import           Data.Kind       (Type)
import           GHC.Generics
import           Type.Reflection (SomeTypeRep (..), Typeable, typeRep)

-- | A data type holding meta-data about a type.
--   The Phantom type parameter `a` ensures type safety for reflective functions
--   that use this type to create type instances.
data TypeInfo a = TypeInfo
  { constructorName :: String,
    fieldNames      :: [String],
    fieldTypes      :: [SomeTypeRep]
  }

-- | this function is a smart constructor for TypeInfo objects.
--   It takes a value of type `a` and returns a `TypeInfo a` object.
--   If the type has no named fields, an error is thrown.
--   If the type has more than one constructor, an error is thrown.
typeInfo :: forall a. (HasConstructor (Rep a), HasSelectors (Rep a), Generic a) => TypeInfo a
typeInfo =
  TypeInfo
    { constructorName = gConstrName x,
      fieldNames = map fst (gSelectors x),
      fieldTypes = map snd (gSelectors x)
    } where x = undefined :: a

-- Generic implementations
gConstrName :: (HasConstructor (Rep a), Generic a) => a -> String
gConstrName = genericConstrName . from

class HasConstructor (f :: Type -> Type) where
  genericConstrName :: f x -> String

instance HasConstructor f => HasConstructor (D1 c f) where
  genericConstrName (M1 x) = genericConstrName x

-- instance (HasConstructor x, HasConstructor y) => HasConstructor (x :+: y) where
--   genericConstrName (L1 l) = genericConstrName l
--   genericConstrName (R1 r) = genericConstrName r

instance Constructor c => HasConstructor (C1 c f) where
  genericConstrName = conName

-- field names & types
gSelectors :: forall a. (HasSelectors (Rep a)) => a -> [(String, SomeTypeRep)]
gSelectors _x = selectors @(Rep a)

class HasSelectors rep where
  selectors :: [(String, SomeTypeRep)]

instance HasSelectors f => HasSelectors (M1 D x f) where
  selectors = selectors @f

instance HasSelectors f => HasSelectors (M1 C x f) where
  selectors = selectors @f

instance (Selector s, Typeable t) => HasSelectors (M1 S s (K1 R t)) where
  selectors =
    [(selName (undefined :: M1 S s (K1 R t) ()), SomeTypeRep (typeRep @t))]

instance (HasSelectors a, HasSelectors b) => HasSelectors (a :*: b) where
  selectors = selectors @a ++ selectors @b

-- instance HasSelectors U1 where
--   selectors = []