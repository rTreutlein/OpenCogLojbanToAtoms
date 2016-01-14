{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
module OpenCog.Lojban.State where

import OpenCog.AtomSpace hiding (get)
import OpenCog.Lojban.Util
import Control.Monad.Trans.State hiding (State)
import Data.Typeable

import qualified Data.Map as M

type State = ([Atom] -> String -> Maybe Atom -> Atom --Atomfinal link funkction
             , [Atom]                        --Argument List
             , String                                --Name
             , Maybe Atom                        --Possible context
             , [(Atom,Atom -> Atom)]   --Other Links
             ,[Atom]                   --List of variables for Questions
             ,String)                                --Input sentence

type IOState a = StateT State IO a

emptyState :: String -> State
emptyState s = (defaultLink,[],"",Nothing,[],[],s)

defaultLink :: [Atom] -> String -> Maybe Atom -> Atom
defaultLink args name context =
    let eval = cEvalL highTv (cPN name lowTv) (cLL $ reverse args)
    in case context of
    Nothing -> eval
    Just cxt -> Link "ContextLink" [cxt,eval] highTv

addAtom :: (Atom,Atom -> Atom) -> IOState ()
addAtom atom = do
    (l,ar,n,c,at,v,s) <- get
    put (l,ar,n,c,atom:at,v,s)

mergeAtoms :: [(Atom,Atom -> Atom)] -> IOState ()
mergeAtoms atoms = do
    (l,ar,n,c,at,v,s) <- get
    put (l,ar,n,c,atoms++at,v,s)

addVar :: Atom -> IOState ()
addVar atom = do
    (l,ar,n,c,at,v,s) <- get
    put (l,ar,n,c,at,atom : v,s)

getVar :: IOState (Atom)
getVar = do
    (l,ar,n,c,at,v:vs,s) <- get
    put (l,ar,n,c,at,vs,s)
    return v

addArg :: Atom -> IOState ()
addArg atom = do
    (l,ar,n,c,at,v,s) <- get
    put (l,atom : ar,n,c,at,v,s)

getArg :: IOState Atom
getArg = do
    (l,e:ar,n,c,at,v,s) <- get
    put (l,ar,n,c,at,v,s)
    return e

addContext :: Atom -> String -> IOState ()
addContext atom name = do
    (l,ar,n,c,at,v,s) <- get
    newC <- case c of
        Nothing -> return atom
        Just context -> combineConcepts name atom context
    put (l,ar,n,Just newC,at,v,s)

combineConcepts :: String -> Atom -> Atom -> IOState Atom
combineConcepts name c1 c2 = addAtom i1 >> addAtom i2 >> return c3
    where c3 = Node "ConceptNode" name lowTv
          i1 = (c1,(\c1 -> cInL highTv c1 c3))
          i2 = (c2,(\c2 -> cInL highTv c2 c3))

getContext :: IOState (Maybe Atom)
getContext = do
    (l,ar,n,c,at,v,s) <- get
    return c

setLink :: ([Atom] -> String -> Maybe Atom -> Atom) -> IOState ()
setLink l = do
    (_,ar,n,c,at,v,s) <- get
    put (l,ar,n,c,at,v,s)

getLink :: IOState ([Atom] -> String -> Maybe Atom -> Atom)
getLink = do
    (l,ar,n,c,at,v,s) <- get
    return l

getName :: IOState String
getName = do
    (l,ar,n,c,at,v,s) <- get
    return n

getSentence :: IOState String
getSentence = do
    (l,ar,n,c,at,v,s) <- get
    return s

setName :: String -> IOState ()
setName n = do
    (l,ar,_,c,at,v,s) <- get
    put (l,ar,n,c,at,v,s)
