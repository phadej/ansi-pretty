{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Text.Pretty.ANSI.Leijen.AnsiPretty
-- License     : BSD3
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
module Text.PrettyPrint.ANSI.Leijen.AnsiPretty (
  -- * Class
  AnsiPretty(..),
  -- * Generics
  -- ** GHC
  ghcAnsiPretty,
  ghcAnsiPrettyWith,
  -- ** SOP
  sopAnsiPretty,
  sopAnsiPrettyWith,
  sopAnsiPrettyS,
  -- ** Options
  AnsiPrettyOpts(..),
  defAnsiPrettyOpts,
  -- * Re-exports
  -- | 'Text.PrettyPrint.ANSI.Leijen'
   module PP,
   -- ** From generics-sop
   ConstructorName,
   FieldName,
  ) where

import           Control.Arrow (first)

import           Data.List as L
import           Data.List.CommonPrefix (CommonPrefix(CommonPrefix), getCommonPrefix)
import           Data.List.NonEmpty as NonEmpty
import qualified Data.Semigroup
import           Data.Semigroup hiding (All)
import qualified GHC.Generics as GHC
import           Generics.SOP as SOP
import           Generics.SOP.GGP as SOP
import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<>), (<$>), semiBraces, Pretty)

#if __HADDOCK__
import qualified Text.PrettyPrint.ANSI.Leijen
#endif

import qualified Text.PrettyPrint.ANSI.Leijen as L
import qualified Data.Foldable as Foldable

-- For instances
import           Data.Int
import           Data.Word
import           Numeric.Natural
import qualified Data.Aeson as Aeson
import qualified Data.Array.IArray as Array
import qualified Data.Array.Unboxed as Array
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.Fixed as Fixed
import qualified Data.Sequence as Seq
import qualified Data.Scientific as Sci
import qualified Data.Set as Set
import qualified Data.Tagged as Tagged
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import qualified Data.Time as Time
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U

#if !MIN_VERSION_generics_sop(0,2,0)
type SListI (a :: k) = SingI a
type SList (a :: k) = Sing a
sList :: forall xs. SListI xs => SList xs
sList = sing
#endif

-- | Generically derivable colorful analogue of 'Text.PrettyPrint.ANSI.Leijen.Pretty'
class AnsiPretty a where
  ansiPretty :: a -> Doc

  default ansiPretty :: (GHC.Generic a, All2 AnsiPretty (GCode a), GFrom a, GDatatypeInfo a) => a -> Doc
  ansiPretty = ghcAnsiPretty

  ansiPrettyList :: [a] -> Doc
  ansiPrettyList = encloseSep (dullgreen lbracket) (dullgreen rbracket) (dullgreen colon) . fmap ansiPretty

semiBraces :: [Doc] -> Doc
semiBraces = encloseSep (dullblue lbrace) (dullblue rbrace) (dullblue semi)

commaParens :: [Doc] -> Doc
commaParens = encloseSep (dullblue lparen) (dullblue rparen) (dullblue comma)

prettyNewtype :: ConstructorName -> Doc -> Doc
prettyNewtype = const id

prettyField :: AnsiPretty a => String -> a -> Doc
prettyField name value = black (text name) <+> blue equals <+> ansiPretty value

ansiPrettyNewtype :: AnsiPretty a => String -> a -> Doc
ansiPrettyNewtype name x = hang 2 (cyan (text name)) </> ansiPretty x

ansiPrettyMap :: (AnsiPretty k, AnsiPretty v) => String -> [(k, v)] -> Doc
ansiPrettyMap name kv = hang 2 (cyan (text name)) </> encloseSep (dullgreen lbracket) (dullgreen rbracket) (dullgreen colon) (fmap f kv)
  where f (k, v) = ansiPretty k <+> blue colon <+> ansiPretty v

prettyRecord :: String -> [(FieldName, Doc)] -> Doc
prettyRecord name fields = hang 2 (cyan (text name) </> semiBraces (L.map (uncurry prettyField) fields'))
  where fields' = L.map (first (L.drop (L.length fieldNamePrefix))) fields
        fieldNamePrefix = maybe [] (getCommonPrefix . sconcat) $ (fmap . fmap) (CommonPrefix . fst) (nonEmpty fields)

data AnsiPrettyOpts = AnsiPrettyOpts
  { poPrettyNewtype :: ConstructorName -> Doc -> Doc
  , poPrettyRecord  :: ConstructorName -> [(FieldName, Doc)] -> Doc
  }

defAnsiPrettyOpts :: AnsiPrettyOpts
defAnsiPrettyOpts = AnsiPrettyOpts prettyNewtype prettyRecord

-- GHC

ghcAnsiPretty :: forall a. (GHC.Generic a, All2 AnsiPretty (GCode a), GFrom a, GDatatypeInfo a) => a -> Doc
ghcAnsiPretty = ghcAnsiPrettyWith defAnsiPrettyOpts

ghcAnsiPrettyWith :: forall a. (GHC.Generic a, All2 AnsiPretty (GCode a), GFrom a, GDatatypeInfo a) => AnsiPrettyOpts -> a -> Doc
ghcAnsiPrettyWith opts x = sopAnsiPrettyS opts (gfrom x) (gdatatypeInfo (Proxy :: Proxy a))

-- SOP

sopAnsiPrettyWith :: forall a. (Generic a, HasDatatypeInfo a, All2 AnsiPretty (Code a)) => AnsiPrettyOpts -> a -> Doc
sopAnsiPrettyWith opts x = sopAnsiPrettyS opts (from x) (datatypeInfo (Proxy :: Proxy a))

sopAnsiPretty :: forall a. (Generic a, HasDatatypeInfo a, All2 AnsiPretty (Code a)) => a -> Doc
sopAnsiPretty = sopAnsiPrettyWith defAnsiPrettyOpts

sopAnsiPrettyS :: (All2 AnsiPretty xss) => AnsiPrettyOpts -> SOP I xss -> DatatypeInfo xss -> Doc
sopAnsiPrettyS  opts (SOP (Z (I x :* Nil))) (Newtype _ _ ci)  = poPrettyNewtype opts (constructorName ci) (ansiPretty x)
sopAnsiPrettyS  opts (SOP (Z xs)) (ADT _ _ (ci :* Nil)) = poPrettyRecord opts (constructorName ci) (gAnsiPrettyP xs (fieldInfo ci))
sopAnsiPrettyS _opts (SOP (Z _ )) _ = error "gAnsiPrettyS: redundant Z case" -- TODO
sopAnsiPrettyS  opts (SOP (S xss)) (ADT m d (_ :* cis)) = sopAnsiPrettyS opts (SOP xss) (ADT m d cis)
sopAnsiPrettyS _opts (SOP (S _)) _  = error "gAnsiPrettyS: redundant S case"

gAnsiPrettyP :: (All AnsiPretty xs) => NP I xs -> NP FieldInfo xs -> [(FieldName, Doc)]
gAnsiPrettyP Nil Nil = []
gAnsiPrettyP (I x :* xs) (FieldInfo f :* fis) = (f, ansiPretty x) : gAnsiPrettyP xs fis
#if __GLASGOW_HASKELL__ < 800
gAnsiPrettyP _ _ = error "gAnsiPrettyP: redundant case"
#endif

#if !MIN_VERSION_generics_sop(0,2,3)
constructorName :: ConstructorInfo a -> ConstructorName
constructorName (Constructor name) = name
constructorName (Infix name _ _) = name
constructorName (Record name _) = name
#endif

fieldInfo :: ConstructorInfo xs -> NP FieldInfo xs
fieldInfo (Constructor _) = constructorFieldInfos 0 sList
fieldInfo (Infix _ _ _) = FieldInfo "_lhs" :* FieldInfo "_rhs" :* Nil
fieldInfo (Record _ fi) = fi

constructorFieldInfos :: forall (xs :: [*]). Int -> SList xs -> NP FieldInfo xs
constructorFieldInfos _ SNil  = Nil
constructorFieldInfos n SCons = FieldInfo ("_" <> show n) :* constructorFieldInfos (n+1) sList

-- Instances

instance AnsiPretty Integer where
  ansiPretty = dullyellow . integer

instance AnsiPretty Int where
  ansiPretty = dullyellow . int

instance AnsiPretty Float where
  ansiPretty = dullyellow . float

instance AnsiPretty Double where
  ansiPretty = dullyellow . double

instance AnsiPretty Doc where
  ansiPretty = id

instance AnsiPretty Bool where
  ansiPretty True = dullyellow $ string "True"
  ansiPretty False = dullyellow $ string "False"

instance AnsiPretty Char where
  ansiPretty c = string [c]
  ansiPrettyList = string

instance AnsiPretty a => AnsiPretty [a] where
  ansiPretty = ansiPrettyList

instance AnsiPretty a => AnsiPretty (Maybe a) where
  ansiPretty (Just x) = ansiPretty x
  ansiPretty Nothing  = dullcyan (string "Nothing")

instance (AnsiPretty a, AnsiPretty b) => AnsiPretty (Either a b)

-- Tuple
instance (AnsiPretty a, AnsiPretty b) => AnsiPretty (a, b) where
  ansiPretty (a, b) = commaParens [ansiPretty a, ansiPretty b]
instance (AnsiPretty a, AnsiPretty b, AnsiPretty c) => AnsiPretty (a, b, c) where
  ansiPretty (a, b, c) = commaParens [ansiPretty a, ansiPretty b, ansiPretty c]
instance (AnsiPretty a, AnsiPretty b, AnsiPretty c, AnsiPretty d) => AnsiPretty (a, b, c, d) where
  ansiPretty (a, b, c, d) = commaParens [ansiPretty a, ansiPretty b, ansiPretty c, ansiPretty d]
instance (AnsiPretty a, AnsiPretty b, AnsiPretty c, AnsiPretty d, AnsiPretty e) => AnsiPretty (a, b, c, d, e) where
  ansiPretty (a, b, c, d, e) = commaParens [ansiPretty a, ansiPretty b, ansiPretty c, ansiPretty d, ansiPretty e]

-- Word
instance AnsiPretty Word where ansiPretty = dullyellow . integer . toInteger

instance AnsiPretty Word8 where ansiPretty = dullyellow . integer . toInteger
instance AnsiPretty Word16 where ansiPretty = dullyellow . integer . toInteger
instance AnsiPretty Word32 where ansiPretty = dullyellow . integer . toInteger
instance AnsiPretty Word64 where ansiPretty = dullyellow . integer . toInteger

instance AnsiPretty Int8 where ansiPretty = dullyellow . integer . toInteger
instance AnsiPretty Int16 where ansiPretty = dullyellow . integer . toInteger
instance AnsiPretty Int32 where ansiPretty = dullyellow . integer . toInteger
instance AnsiPretty Int64 where ansiPretty = dullyellow . integer . toInteger

instance AnsiPretty Natural where ansiPretty = dullyellow . integer . toInteger

instance Fixed.HasResolution e => AnsiPretty (Fixed.Fixed e) where ansiPretty = dullyellow . text . show
#if MIN_VERSION_base(4,9,0)
instance (AnsiPretty a) => AnsiPretty (Ratio.Ratio a) where
#else
instance (AnsiPretty a, Integral a) => AnsiPretty (Ratio.Ratio a) where
#endif
  ansiPretty r = ansiPretty (Ratio.numerator r) <+> dullyellow (char '%') <+> ansiPretty (Ratio.denominator r)

-- Generic instances
instance AnsiPretty a => AnsiPretty (CommonPrefix a)

-- aeson
instance AnsiPretty Aeson.Value where
    ansiPretty (Aeson.Object o)
        = encloseSep (dullgreen lbrace) (dullgreen rbrace) (dullgreen comma)
        $ fmap f $ HashMap.toList o
      where
        f (k, v) = dullwhite (ansiPretty k) L.<> blue colon <+> ansiPretty v

    ansiPretty (Aeson.Array a)
        = encloseSep (dullgreen lbracket) (dullgreen rbracket) (dullgreen comma)
        $ fmap ansiPretty $ V.toList a

    ansiPretty (Aeson.Number s)
        = maybe (ansiPretty s) (ansiPretty :: Int -> Doc)
        $ Sci.toBoundedInteger s

    ansiPretty (Aeson.String s)   = ansiPretty (show s)
    ansiPretty (Aeson.Bool True)  = dullyellow $ string "true"
    ansiPretty (Aeson.Bool False) = dullyellow $ string "false"
    ansiPretty Aeson.Null         = cyan (text "Null")

-- array
instance (AnsiPretty i, AnsiPretty e, Array.Ix i) => AnsiPretty (Array.Array i e) where ansiPretty = ansiPrettyMap "Array" . Array.assocs
instance (AnsiPretty i, AnsiPretty e, Array.Ix i, Array.IArray Array.UArray e) => AnsiPretty (Array.UArray i e) where ansiPretty = ansiPrettyMap "UArray" . Array.assocs

-- containers
instance AnsiPretty IntSet.IntSet where
  ansiPretty = ansiPrettyNewtype "IntSet" . IntSet.toList
instance AnsiPretty v => AnsiPretty (IntMap.IntMap v) where
  ansiPretty = ansiPrettyMap "IntMap" . IntMap.toList
instance AnsiPretty a => AnsiPretty (Set.Set a) where
   ansiPretty = ansiPrettyNewtype "Set" . Set.toList
instance (AnsiPretty k, AnsiPretty v) => AnsiPretty (Map.Map k v) where
  ansiPretty = ansiPrettyMap "Map" . Map.toList

instance AnsiPretty a => AnsiPretty (Seq.Seq a) where ansiPretty = ansiPrettyNewtype "Seq" . Foldable.toList

-- semigroups
instance AnsiPretty a => AnsiPretty (NonEmpty a) where
  ansiPretty = ansiPretty . toList

instance AnsiPretty a => AnsiPretty (Min a)
instance AnsiPretty a => AnsiPretty (Max a)
instance AnsiPretty a => AnsiPretty (First a)
instance AnsiPretty a => AnsiPretty (Last a)
instance AnsiPretty m => AnsiPretty (WrappedMonoid m)
instance AnsiPretty a => AnsiPretty (Dual a)
instance AnsiPretty Data.Semigroup.All
instance AnsiPretty Any
instance AnsiPretty a => AnsiPretty (Sum a)
instance AnsiPretty a => AnsiPretty (Product a)
instance AnsiPretty a => AnsiPretty (Option a)
instance (AnsiPretty a, AnsiPretty b) => AnsiPretty (Arg a b)

-- scientific
instance AnsiPretty Sci.Scientific where ansiPretty = dullyellow . text . show

-- tagged
instance AnsiPretty a => AnsiPretty (Tagged.Tagged t a) where ansiPretty = ansiPretty . Tagged.untag

-- text
instance AnsiPretty LT.Text where ansiPretty = ansiPretty . LT.unpack
instance AnsiPretty ST.Text where ansiPretty = ansiPretty . ST.unpack

-- time
instance AnsiPretty Time.UTCTime where ansiPretty = ansiPretty . show
instance AnsiPretty Time.Day where ansiPretty = ansiPretty . show
instance AnsiPretty Time.TimeZone where ansiPretty = ansiPretty . show
instance AnsiPretty Time.TimeOfDay where ansiPretty = ansiPretty . show
instance AnsiPretty Time.LocalTime where ansiPretty = ansiPretty . show
instance AnsiPretty Time.ZonedTime where ansiPretty = ansiPretty . show
-- instance AnsiPretty Time.UniversalTime where ansiPretty = ansiPretty . show
instance AnsiPretty Time.DiffTime where ansiPretty = ansiPretty . show
instance AnsiPretty Time.NominalDiffTime where ansiPretty = ansiPretty . show

-- vector
instance AnsiPretty a => AnsiPretty (V.Vector a) where ansiPretty = ansiPrettyNewtype "Vector" . V.toList
instance (AnsiPretty a, S.Storable a) => AnsiPretty (S.Vector a) where ansiPretty = ansiPrettyNewtype "S.Vector" . S.toList
instance (AnsiPretty a, U.Unbox a) => AnsiPretty (U.Vector a) where ansiPretty = ansiPrettyNewtype "U.Vector" . U.toList

-- unordered-containers
instance AnsiPretty a => AnsiPretty (HashSet.HashSet a) where ansiPretty = ansiPrettyNewtype "HashSet" . HashSet.toList

instance (AnsiPretty k, AnsiPretty v) => AnsiPretty (HashMap.HashMap k v) where
  ansiPretty = ansiPrettyMap "HashMap" . HashMap.toList
