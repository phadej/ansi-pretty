{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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
  AnsiPrettyOpts(..),
  defAnsiPrettyOpts,
  -- ** GHC
  ghcAnsiPretty,
  ghcAnsiPrettyWith,
  -- ** SOP
  sopAnsiPretty,
  sopAnsiPrettyWith,
  sopAnsiPrettyS,
  -- * Re-exports
  -- | 'Text.PrettyPrint.ANSI.Leijen'
   module PP,
  ) where

import           Control.Arrow (first)
import           Data.List as L
import           Data.List.CommonPrefix (CommonPrefix(CommonPrefix), getCommonPrefix)
import           Data.List.NonEmpty as NonEmpty
import           Data.Semigroup (sconcat, (<>))
import           Data.Text as T
import           Data.Time
import qualified GHC.Generics as GHC
import           Generics.SOP as SOP
import           Generics.SOP.GGP as SOP
import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<>), (<$>), semiBraces, Pretty)

class AnsiPretty a where
  ansiPretty :: a -> Doc

  default ansiPretty :: (GHC.Generic a, All2 AnsiPretty (GCode a), GFrom a, GDatatypeInfo a, SingI (GCode a)) => a -> Doc
  ansiPretty = ghcAnsiPretty

  ansiPrettyList :: [a] -> Doc
  ansiPrettyList = encloseSep (dullgreen lbracket) (dullgreen rbracket) (dullgreen colon) . fmap ansiPretty

semiBraces :: [Doc] -> Doc
semiBraces = encloseSep (dullblue lbrace) (dullblue rbrace) (dullblue semi)

prettyNewtype :: ConstructorName -> Doc -> Doc
prettyNewtype = const id

prettyField :: AnsiPretty a => String -> a -> Doc
prettyField name value = black (text name) <+> blue equals <+> ansiPretty value

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

ghcAnsiPretty :: forall a. (GHC.Generic a, All2 AnsiPretty (GCode a), GFrom a, GDatatypeInfo a, SingI (GCode a)) => a -> Doc
ghcAnsiPretty = ghcAnsiPrettyWith defAnsiPrettyOpts

ghcAnsiPrettyWith :: forall a. (GHC.Generic a, All2 AnsiPretty (GCode a), GFrom a, GDatatypeInfo a, SingI (GCode a)) => AnsiPrettyOpts -> a -> Doc
ghcAnsiPrettyWith opts x = sopAnsiPrettyS opts (gfrom x) (gdatatypeInfo (Proxy :: Proxy a))

-- SOP

sopAnsiPrettyWith :: forall a. (Generic a, HasDatatypeInfo a, All2 AnsiPretty (Code a)) => AnsiPrettyOpts -> a -> Doc
sopAnsiPrettyWith opts x = sopAnsiPrettyS opts (from x) (datatypeInfo (Proxy :: Proxy a))

sopAnsiPretty :: forall a. (Generic a, HasDatatypeInfo a, All2 AnsiPretty (Code a)) => a -> Doc
sopAnsiPretty = sopAnsiPrettyWith defAnsiPrettyOpts

sopAnsiPrettyS :: (All2 AnsiPretty xss) => AnsiPrettyOpts -> SOP I xss -> DatatypeInfo xss -> Doc
sopAnsiPrettyS  opts (SOP (Z (I x :* Nil))) (Newtype _ _ ci)  = poPrettyNewtype opts (constructorName ci) (ansiPretty x)
sopAnsiPrettyS  opts (SOP (Z xs)) (ADT _ _ (ci :* Nil)) = poPrettyRecord opts (constructorName ci) (gAnsiPrettyP xs (fieldInfo ci))
sopAnsiPrettyS _opts (SOP (Z _ )) _ = error "gAnsiPrettyS: redundant Z case"
sopAnsiPrettyS  opts (SOP (S xss)) (ADT m d (_ :* cis)) = sopAnsiPrettyS opts (SOP xss) (ADT m d cis)
sopAnsiPrettyS _opts (SOP (S _)) _  = error "gAnsiPrettyS: redundant S case"

gAnsiPrettyP :: (All AnsiPretty xs) => NP I xs -> NP FieldInfo xs -> [(FieldName, Doc)]
gAnsiPrettyP Nil Nil = []
gAnsiPrettyP (I x :* xs) (FieldInfo fieldName :* fis) = (fieldName, ansiPretty x) : gAnsiPrettyP xs fis
gAnsiPrettyP _ _ = error "gAnsiPrettyP: redundant case"

constructorName :: ConstructorInfo a -> ConstructorName
constructorName (Constructor name) = name
constructorName (Infix name _ _) = name
constructorName (Record name _) = name

fieldInfo :: ConstructorInfo xs -> NP FieldInfo xs
fieldInfo (Constructor _) = constructorFieldInfos 0 sing
fieldInfo (Infix _ _ _) = FieldInfo "_lhs" :* FieldInfo "_rhs" :* Nil
fieldInfo (Record _ fi) = fi

constructorFieldInfos :: forall (xs :: [*]). Int -> Sing xs -> NP FieldInfo xs
constructorFieldInfos _ SNil  = Nil
constructorFieldInfos n SCons = FieldInfo ("_" <> show n) :* constructorFieldInfos (n+1) sing

-- Instances

instance AnsiPretty Integer where
  ansiPretty = dullyellow . integer

instance AnsiPretty Int where
  ansiPretty = dullyellow . int

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

instance AnsiPretty Text where
  ansiPretty = ansiPretty . T.unpack

instance AnsiPretty UTCTime where
  ansiPretty = ansiPretty . show
