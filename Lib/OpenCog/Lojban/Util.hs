{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE GADTs              #-}
module OpenCog.Lojban.Util where

import OpenCog.AtomSpace
import Data.Typeable

import Tersmu

pattern CN name <-Node "ConceptNode" name _
pattern PN name <-Node "PredicateNode" name _
pattern VN name <-Node "VariableNode" name _

pattern AL l <- Link "AndLink" l _
pattern LL l <- Link "ListLink" l _
pattern SL l <- Link "SetLink" l _
pattern SSL l <- Link "SatisfyingSetLink" [l] _
pattern EvalL p a <- Link "EvaluationLink" [p,a] _
pattern CtxL c a <- Link "ContextLink" [c,a] _

cCN name tv = Node "ConceptNode" name tv
cPN name tv = Node "PredicateNode" name tv
cVN name    = Node "VariableNode" name noTv
cAN name    = Node "AnchorNode" name noTv
cNN name    = Node "NumberNode" name noTv

cLL a           = Link "ListLink"                             a noTv
cSL a           = Link "SetLink"                              a noTv
cInL tv a b     = Link "InheritanceLink"                  [a,b] tv
cImL tv a b     = Link "ImplicationLink"                  [a,b] tv
cIFaoIFL tv a b = Link "And"          [cImL tv a b,cImL tv b a] tv
cEvalL tv a b   = Link "EvaluationLink"                   [a,b] tv
cSSL tv a       = Link "SatisfyingSetLink"                  [a] tv
cExL tv a b     = Link "ExistsLink"                       [a,b] tv
cFAL tv a b     = Link "ForAllLink"                       [a,b] tv
cPL     a b     = Link "PutLink"                          [a,b] noTv
cGL     a       = Link "GetLink"                            [a] noTv
cAL  tv a       = Link "And"                                  a tv
cOL  tv a       = Link "Or"                                   a tv
cNL  tv a       = Link "Not"                                [a] tv
cCtxL tv a b    = Link "ContextLink"                      [a,b] tv
cLamdaL tv a b  = Link "LambdaLink"                       [a,b] tv

isVN :: Atom -> a -> (String -> a) -> a
isVN (VN n) _ f = f n
isVN _ d _      = d

highTv :: TruthVal
highTv = stv 1 0.9

lowTv :: TruthVal
lowTv = stv 0.000001 0.01

showJbo :: JboShow t => t -> String
showJbo a = evalBindful $ logjboshow True a

if' :: Bool -> a -> a -> a
if' True a _ = a
if' False _ a = a

infixr 1 ?
(?) :: Bool -> a -> a -> a
(?) = if'

infixr 9 .$
(.$) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.$) f2 f1 a = f2 . (f1 a)

mapfst :: (a -> b) -> (a,t) -> (b,t)
mapfst f (a,b) = (f a,b)
