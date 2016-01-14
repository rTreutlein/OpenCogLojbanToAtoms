{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
module OpenCog.Lojban.SuReal where

import OpenCog.AtomSpace hiding (get,insert)
import qualified OpenCog.AtomSpace as A
import OpenCog.Lojban.Util
import Data.Foldable
import Data.List
import Control.Monad.Trans.State hiding (State)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import qualified Data.Map as M

type SuRealState = ()

type SuRealStateT a = StateT SuRealState AtomSpace a

atomeseToLojban :: Atom -> AtomSpace Atom
atomeseToLojban a@(LL [SL [SL (s:r)]]) = do
    --liftIO $ print a
    --liftIO $ putStrLn "--------------------------"
    --liftIO $ print r
    --liftIO $ putStrLn "--------------------------"
    lojban <- evalStateT (sayLink s) ()
    return $ cLL [cAN "LojbanAnswer"
            ,cCN lojban noTv
            ]
atomeseToLojban a@(LL [SL [sat]]) = do
    mtv <- evaluate sat
    let lojban = case mtv of
            Just tv -> tvToLojban tv
            Nothing -> "mi na djuno (this might be an error)"
    return $ cLL [cAN "LojbanAnswer"
            ,cCN lojban noTv
            ]
atomeseToLojban a = do
    --liftIO $ putStrLn "empty input:"
    --liftIO $ print a
    return $ cSL []

{-getCmene :: Atom -> SuRealStateT ()
getCmene (Gen (EvaluationLink _ (PredicateNode "cmene" _)(ListLink [a,b]))) = do
    (Gen (ConceptNode name  _)) <- pure a
    (Gen (ConceptNode named _)) <- pure b
    (nmap) <- get
    put (M.insert named name nmap)
getCmene a = pure ()-}

tvToLojban :: TruthVal -> String
tvToLojban tv
    | (tvMean tv) > 0.5 = "go'i"
    | (tvMean tv) <= 0.5 = "nago'i"

(+|+) :: String -> String -> String
(+|+) = (\a b -> a ++ (' ':b))

sayLink :: Atom -> SuRealStateT String
sayLink a = do
    (p,(h:r),mc) <- sayLink' a
    return $ prun $ case mc of
        Just c -> h +|+ c +|+ p +|+ (foldl (+|+) "" r)
        Nothing -> h +|+ p +|+ (foldl (+|+) "" r)

sayLink' :: Atom -> SuRealStateT (String,[String],Maybe String)
sayLink' (EvalL pred (LL args)) = do
    ar <- mapM sayArgument args
    lp <- sayPredicate pred
    return (lp,ar,Nothing)
sayLink' (AL ls) = do
    a <- mapM sayLink' ls
    let (lp,_,c) = head a
        lal      = foldr (\(_,ar,_) ls -> ar:ls) [] a
        (ar)     = foldr1 (zipWith (\a b -> a == b ? a $ a ++ " e " ++ b)) lal
    return (lp,ar,c)
sayLink' (CtxL (CN ctx) eval) = do
    (lp,ar,_) <- sayLink' eval
    return (lp,ar,Just ctx)

sayLink' a = error $ "sayLink not implemented for: " ++ show a



saySubLink :: Atom -> SuRealStateT String
saySubLink a = do
    r <- saySubLink' a
    return (prun r)

prun :: String -> String
prun a = case " zo'e" `isSuffixOf` a of
    True -> prun $ take (length a - 5) a
    False -> a

saySubLink' :: Atom -> SuRealStateT String
saySubLink' (EvalL pred (LL args)) = do
    sa <- mapM sayArgument args
    sp <- sayPredicate pred
    let mod = case elemIndex "#var#" sa of
              {Just 0 -> ""; Just 1 -> "se "; Just 2 -> "te ";
              Just 3 -> "ve "; Just 4 -> "xe ";
              Nothing -> error "no var in SatisfyingSetLink"}
        rargs = tail $ swap (head sa) ("#var#") sa
    return $ "lo" +|+ mod ++ sp +|+ (foldl (+|+) "" rargs)
saySubLink' (CtxL c l) = do
    let (CN cname) = c
    link <- saySubLink l
    return $ case "nonveridical" `isInfixOf` cname of
              True -> "le " ++ (drop 3 link)
              False -> link
saySubLink' a = error $ "saySubLink not implemented for: " ++ show a

swap a b = map (\x -> if x == a then b else if x == b then a else x)

sayArgument :: Atom -> SuRealStateT String
sayArgument (CN s) = do
    nmap <- get
    res <- resolvName s
    case res of
            Just r -> return $ "la" +|+ r
            Nothing -> resolvConcept s
sayArgument (VN _) = return "#var#"
sayArgument (SSL l) = saySubLink l
sayArgument a = error $ "sayArgument not implemented for: " ++ show a

sayPredicate :: Atom -> SuRealStateT String
sayPredicate (PN s) = return s
sayPredicate a = error $ "sayPredicate not implemented for: " ++ show a

type Name = String

resolvName :: Name -> SuRealStateT (Maybe String)
resolvName n = do
    let query = getNameOf n
    lift $ A.insert query
    res <- lift $ execute query
    case res of
        Just (SL [CN name]) -> return $ Just name
        Just (SL []) -> return $ Nothing
        Nothing -> return $ Nothing
        Just a -> error $ show a ++ " name: " ++ n

resolvConcept :: Name -> SuRealStateT String
resolvConcept "zo'e" = return "zo'e"
resolvConcept n = do
    let query = getAbstractionOfInstance n
    lift $ A.insert query
    res <- lift $ execute query
    case res of
        Just (SL [CN name]) -> return $ name
        Nothing -> error $ "no instance of a concept or name " ++ n

getAbstractionOfInstance n = cGL $ cInL highTv (cCN n lowTv) (cVN "$var")

getNameOf n = cGL $
                cEvalL highTv
                   (cPN "cmene" lowTv)
                   (cLL [cVN "$var"
                        ,cCN n lowTv
                        ])
