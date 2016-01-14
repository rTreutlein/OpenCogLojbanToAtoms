{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TypeOperators      #-}
module OpenCog.Lojban.Parser where

import OpenCog.Lojban.State
import OpenCog.Lojban.Util

import OpenCog.AtomSpace

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State hiding (State)
import Control.Arrow

import Data.Maybe
import Data.List hiding (insert)
import qualified Data.Map as M
import System.Random

import Tersmu hiding (addArg)

import Debug.Trace
mytrace a = traceShow a a

lojbanToAtomese :: Atom -> AtomSpace Atom
lojbanToAtomese (LL [SL []])         = return $ cSL []
lojbanToAtomese (LL [SL [CN text ]]) = do
    parsed <- liftIO $ lojbanToJboText text
    case parsed of
        Just p -> convertJboText p text
        _      -> error $ "couldn't parse the text, it is not valid lojban: " ++ text
lojbanToAtomese a = error $ "input was in the wrong format, expection a ConceptNode got:" ++ show a

convertJboText :: JboText -> String -> AtomSpace Atom
convertJboText txt s = do
    let ftxt  = filter (\case {TexticuleProp _ -> True ; _ -> False }) txt
        props = reverse $ map (\(TexticuleProp p) -> p) ftxt
    cps <- liftIO $ mapM (convertJboProp s) props
    case cps of
        [] -> error $ "couldn't convert the lojban text to atomese" ++ s
        _  -> do
            let ((ids,f,_,q):xs) = cps
                gensentence      = f (getConstants ids xs) ""
                atomsfs          = foldr (\(_,_,at,_) acc -> at++acc) [] cps
                atoms            = map (\(a,f) -> f a) atomsfs
                anchor           = Node "AnchorNode" (case q of
                                                        0 -> "StatmentAnchor"
                                                        _ -> "QuestionAnchor")
                                                     noTv
            setlink <- case (q,gensentence) of
                (0,sentence) -> getResult $
                    cSL (sentence:atoms)
                (1,sentence) -> do
                    let question = cAL highTv (sentence:atoms)
                    getResult $ cPL sentence $ cGL question
                (2,sentence) -> do
                    let question = cAL highTv (sentence:atoms)
                    getResult $ Link "SatisfactionLink" [question] highTv
            return $ cLL [anchor,setlink]

getResult :: Atom -> AtomSpace Atom
getResult a = do
    insert a
    mb <- execute a
    remove a
    case mb of
        Just b -> return b
        _ -> error $ "could not execute atom " ++ show a

type AtomProp = ([String] --Names of the variables that rae not defined
                ,M.Map String Atom -> String -> Atom
                ,[(Atom,Atom -> Atom)] --Other Atoms
                ,Int) --Sentence Type Statement/Question

getConstants :: [String] -> [AtomProp] -> M.Map String Atom
getConstants [] _ = M.empty
getConstants (x:xs) props =
    let ([(ids,f,at,q)],rest) = partition (\(ids,_,_,_) -> x `elem` ids) props
    in case filter (\id -> x /= id) ids of
        []   -> M.insert x (toSatisfyingSet $ f M.empty x) $ getConstants xs rest
        fids -> let map = getConstants fids rest
                in M.insert x (toSatisfyingSet $ f map x) $ getConstants xs rest

convertJboProp :: String -> JboProp -> IO AtomProp
convertJboProp s (Modal QTruthModal p) = do
    (l,ar,n,c,at,v,_) <- execStateT (convertJboProp' p) $ emptyState s
    let varIds   = foldr (\v r -> v `isVN` r $ (\n -> 'B'`notElem` n ? n:r $ r)) [] ar
        args     = map conceptsToVariables .$ replaceVars ar
        atv      = map (mapfst conceptsToVariables) at
        sentence = (\map id -> l (args map id) n c)
    return (varIds,sentence,atv,2)
convertJboProp s p = do
    (l,ar,n,c,at,v,_) <- execStateT (convertJboProp' p) $ emptyState s
    let q        = if null v then 0 else 1
        varIds   = foldr (\v r -> v `isVN` r $ (\n -> 'B'`notElem` n ? n:r $ r)) [] ar
        args     = (null v ? id $ map conceptsToVariables) .$ replaceVars ar
        sentence = (\map id -> l (args map id) n c)
    return (varIds,sentence,at,q)

--If it is a question all the Concept nodes have to be Variable nodes
--As they are define elsewhere
conceptsToVariables :: Atom -> Atom
conceptsToVariables = \case {(CN n) -> (cVN n); a -> a }

--predicateToVariables :: [(Atom,Atom -> Atom)] -> [Atom] -> [(Atom,Atom -> Atom)]
--predicateToVariables (o@(Gen (PredicateNode name _),b):xs) reps = (res,b):resxs
--    where res = if bool then (Gen $ VariableNode name) else o
--          resxs = predicateToVariables xs reps
--          bool = foldl (\b (Gen (VariableNode vname)) -> b || (name == vname)) False reps

--replace All variables that are not part of questions
--with their definiton
replaceVars :: [Atom] -> M.Map String Atom -> String -> [Atom]
replaceVars [] map id    = []
replaceVars(x:xs) map id = case x of
    (VN n) -> (if n==id || 'B' `elem` n then x else map M.! n) : replaceVars xs map id
    _ -> x : replaceVars xs map id

--(\m id r e -> e `isGVN` e (\n -> n==id || 'B' `elem` n ? e map M.! n)):r
--case' e = case e of ()

filterVars :: [String] -> [String] -> ([String],String)
filterVars (x:xs) ids =
    if x `elem` ids
    then (xs,x)
    else first ((:) x) $ filterVars xs ids

toSatisfyingSet :: Atom -> Atom
toSatisfyingSet atom = cSSL highTv atom

{-toLinkWithVars :: Atom -> IO (Atom,Gen VariableT)
toLinkWithVars l = do
    stdgen <- newStdGen
    let var         = VariableNode ("$" ++ take 20 (randomRs ('a','z') stdgen))
        (eval,node) = getNode l var
        eq          = Gen (EqualLink var `appGen` node :: Atom EqualT)
    return (Gen $ AndLink highTv [eval,eq],Gen var)-}


getNode :: Atom -> Atom -> (Atom,Atom)
getNode (Link "EvaluationLink" [p,l] tv) var =
    (cEvalL tv var l,p)
getNode (Link "ContextLink" [c,l] tv) var =
    (cCtxL tv c nl,node)
    where (nl,node) = getNode l var

convertConnected :: Connective -> JboProp -> JboProp -> IOState ()
convertConnected con p1 p2 = do
    s <- getSentence
    (l1,args1,n1,c1,atoms1,var1,_) <- liftIO $ execStateT (convertJboProp' p1) $ emptyState s
    (l2,args2,n2,c2,atoms2,var2,_) <- liftIO $ execStateT (convertJboProp' p2) $ emptyState s
    let sublink1 =l1 args1 n1 c1
        sublink2 =l2 args2 n2 c2
        link = case con of
            Or ->    \_ _ _ -> cOL      highTv [sublink1,sublink2]
            And ->   \_ _ _ -> cAL      highTv [sublink1,sublink2]
            Impl ->  \_ _ _ -> cImL     highTv sublink1 sublink2
            Equiv -> \_ _ _ -> cIFaoIFL highTv sublink1 sublink2
    put (link, [], "", Nothing, atoms1 ++ atoms2,[],s)

convertJboProp' :: JboProp -> IOState ()
convertJboProp' (Rel (Among s) [t]) = do
    convertJboTerm s
    cs <- getArg
    convertJboTerm t
    ct <- getArg
    addArg $ Link "SubsetLink" [ct,cs] highTv
convertJboProp' (Rel Equal t) = do
    mapM_ convertJboTerm t
    sct <- getArg
    fct <- getArg
    addArg $ Link "SimilarityLink " [fct,sct] highTv
convertJboProp' (Rel r t) = do
    convertJboRel r
    mapM_ convertJboTerm t
convertJboProp' m@(Modal mo p) = do
    convertJboProp' p
    let name = myShowModal m
    convertJboModal mo name
convertJboProp' (Connected con p1 p2) = convertConnected con p1 p2
convertJboProp' (Not x) = do
    s <- getSentence
    (l,ar,n,c,at,v,_) <- liftIO $ execStateT (convertJboProp' x) $ emptyState s
    setLink (\args name cxt -> cNL highTv $ l args name cxt)
convertJboProp' Eet = error "ParseProblem"
convertJboProp' (Quantified x1 x2 x3) = do
    rnd <- liftIO $ getStdRandom random
    convertJboProp' $ x3 rnd
    convertQuantifier x1
    --cmp <- mapM (\x -> convertJboProp' $ x 0) x2 --XXX What is this extra prop for?
convertJboProp' e = error $ show e ++ "JboProp' Incompelte"
--convertJboProp' (NonLogConnected x1 x2 x3) = _convertJboProp'_body

convertQuantifier :: JboQuantifier -> IOState ()
--convertQuantifier (MexQuantifier x) = _convertQuantifier_body
convertQuantifier (LojQuantifier x) = convertLojQuantifer x
convertQuantifier QuestionQuantifier = return ()
--convertQuantifier (RelQuantifier x) = _convertQuantifier_body

convertLojQuantifer  :: LojQuantifier -> IOState ()
convertLojQuantifer Exists = do
    l <- getLink
    v <- getVar
    setLink (\arg n c -> cExL highTv v (l arg n c))
convertLojQuantifer Forall = do
    l <- getLink
    v <- getVar
    setLink (\arg n c -> cFAL highTv v (l arg n c))
convertLojQuantifer (Exactly x) = return () --XXX Do something here

conToPred :: Atom -> Atom
conToPred (Node "ConceptNode" name tv) = cPN name tv

myShowModal :: JboProp -> String
myShowModal (Modal m p) = myshow m ++ rest
    where rest = case p of (Modal _ _) -> myShowModal p
                           otherwise   -> ""

myshow :: JboModalOp -> String
myshow (JboTagged tag mp) = showJbo tag
myshow (WithEventAs a) = "WithEvant"
myshow QTruthModal = "QTruthModal"
myshow NonVeridical = "nonVeridical"

convertJboModal :: JboModalOp -> String -> IOState ()
convertJboModal (JboTagged tag mt) name = do
    ct <- convertJboTag tag
    let mns = convertJboTerm <$> mt
    case mns of
        Nothing -> addContext ct name
        Just ns -> do
            ns
            c <- getArg
            let p = conToPred ct
                e = (\p -> let vp = case p of
                                            e@(PN a) -> e
                                            e@(VN a) -> e
                                            _ -> error "Not a var or pred 1: "
                           in cEvalL highTv vp $ cLL [c])
            addAtom (p,e)
            addContext c name
convertJboModal NonVeridical name =
    addContext (cCN "nonveridical" noTv) "NonVeridical"
--convertJboModal (WithEventAs x) = _convertJboModal_body
--convertJboModal QTruthModal = _convertJboModal_body
convertJboModal x _ = error $ "convertJboModal incomplet: " ++ show x

convertJboTag :: JboTag -> IOState Atom
convertJboTag (DecoratedTagUnits u) = do
    cu <- mapM convertDTagUnit u
    return $ head cu

convertDTagUnit :: (Show r, Show t) => DecoratedAbsTagUnit r t -> IOState Atom
convertDTagUnit (DecoratedTagUnit tagNahe tagSE tagNai tagUnit) = convertTagUnit tagUnit

convertTagUnit :: (Show r, Show t) => AbsTagUnit r t -> IOState Atom
convertTagUnit (TenseCmavo s) = return (cCN s lowTv)
--convertTagUnit (CAhA x) = _convertTagUnit_body
--convertTagUnit (FAhA { fahaHasMohi = x_fahaHasMohi, fahaCmavo = x_fahaCmavo }) = _convertTagUnit_body
convertTagUnit (ROI { roiroi = x_roiroi, roiIsSpace = x_roiIsSpace, roiQuantifier = x_roiQuantifier }) = do
    cm <- convertAbsMex x_roiQuantifier
    return $ cCN (x_roiroi++cm) lowTv
--convertTagUnit (TAhE_ZAhO { taheZoheIsSpace = x_taheZoheIsSpace, taheZahoCmavo = x_taheZahoCmavo }) = _convertTagUnit_body
convertTagUnit (BAI x) = return (cCN x lowTv)
--convertTagUnit (FIhO x) = _convertTagUnit_body
--convertTagUnit CUhE = _convertTagUnit_body
--convertTagUnit KI = _convertTagUnit_body
convertTagUnit a = error $ show a ++ "Missing TagUnit implementation"

convertAbsMex :: AbsMex r t -> IOState String
convertAbsMex (MexInt x) = return $ show x
--convertAbsMex (Operation x1 x2) = _convertAbsMex_body
--convertAbsMex (ConnectedMex x1 x2 x3 x4) = _convertAbsMex_body
--convertAbsMex (QualifiedMex x1 x2) = _convertAbsMex_body
--convertAbsMex (MexNumeralString x) = _convertAbsMex_body
--convertAbsMex (MexLerfuString x) = _convertAbsMex_body
--convertAbsMex (MexSelbri x) = _convertAbsMex_body
--convertAbsMex (MexSumti x) = _convertAbsMex_body
--convertAbsMex (MexArray x) = _convertAbsMex_body

toEval :: Atom -> [Atom] -> Atom
toEval p c = cEvalL highTv p (cLL c)

convertJboRel :: JboRel -> IOState ()
convertJboRel (Brivla s) = setName s
convertJboRel (Moi t c) = do
    let name = showJbo t
    setName (c++name)
convertJboRel (Tanru jbovpred jborel) = do --
    convertJboRel jborel
    name <- getName
    cp <- convertJboVPred jbovpred
    setName (cp++name)
convertJboRel (AbsProp a prop) = do
    ca <- convertAbstractor a
    s <- getSentence
    (l,a,n,mc,atoms,var,_) <- liftIO $ execStateT (convertJboProp' prop) $ emptyState s
    --XXX v is still ignored
    mergeAtoms atoms
    let name = evalBindful $ logjboshow True prop
        atom =l a n mc
    case mc of
        Just c  -> error "convertJboRel error epr;kfm" --addAtom (ContextLink highTv `appGen` c `appGen` atom :: Atom ContextT)
        Nothing -> addAtom $ (cPN (ca++name) lowTv,
                        \p -> let vp = case p of
                                            e@(PN a) -> e
                                            e@(VN a) -> e
                                            _ -> error "Not a var or pred 1: "
                               in Link "EquivalenceLink"
                                   [cLamdaL highTv
                                        (cVN "1")
                                        (cEvalL highTv
                                           (vp)
                                           (cLL [cVN "1"]))
                                   ,cLamdaL highTv
                                        (cVN "2")
                                        (cCtxL highTv
                                            (cVN "2")
                                            (atom))
                                   ] highTv
                       )
    setName (ca++name)
convertJboRel (AbsPred a pred) = do
    let name = evalBindful $ logjboshow True (AbsPred a pred)
    setName name
convertJboRel a@(ScalarNegatedRel _ _) =
    setName (showJbo a)
--convertJboRel (Among s) = convertJboTerm s >>= (\c -> return $ conToPred c)
--convertJboRel (TanruConnective s1 s2 s3) = _convertJboRel_body
--convertJboRel (UnboundBribasti s) = _convertJboRel_body
--convertJboRel (BoundRVar s) = _convertJboRel_body
--convertJboRel (RVar s) = _convertJboRel_body
--convertJboRel (OperatorRel s) = _convertJboRel_body
--convertJboRel (TagRel s) = _convertJboRel_body
convertJboRel  a = error $ show a ++ "JboRel Incompelte"

convertJboVPred :: JboVPred -> IOState String
convertJboVPred a = return $ evalBindful $ logjboshow True a
    --let pred = vPredToPred vpred
    --    prop = pred (BoundVar 1)
    --(_,Just (EvaluationLink _ p _)) <- convertJboProp' prop
    --return $ getName p

--convertJboNpred :: JboNPred -> IOState String
--convertJboNpred (JboNPred arity pred) =return $ evalBindful $ logjboshow True
--    let args = map BoundVar $ take arity [1..]
--        prop = pred args
--    (_,Just (EvaluationLink _ p _)) <- convertJboProp' prop
--    return $ getName p

convertAbstractor :: Abstractor -> IOState String
convertAbstractor (NU s) = return s

convertJboTerm :: JboTerm -> IOState ()
convertJboTerm (NonAnaph name)                  = do
    --s <- getSentence                          Deterministc name generation
    --let ci = ConceptNode (name++('#':s)) lowTv    not context dependent
    ci <- randConceptNode
    addAtom (ci,(\ci -> cInL highTv ci $ cCN name lowTv))
    addArg ci
convertJboTerm (Constant n _)                = addArg $ cVN (show n)
convertJboTerm (Value v)                     = convertJboMex v
convertJboTerm (Named s)                     = do
    named <- randConceptNode
    addAtom (named,\named -> cEvalL highTv
                (cPN "cmene" lowTv)
                (cLL [cCN s lowTv,named]))
    addArg named
convertJboTerm (PredNamed x) = do
    error $ "FixMe convertJboTerm PredNamed"
    stdgen <- liftIO newStdGen
    let names   = apply (splitAt 20) $ randomRs ('a','z') stdgen
        meaning = cCN (names!!1) lowTv
        name    = cCN (names!!2) lowTv
        named   = cCN (names!!3) lowTv
    s <- getSentence
    (l,ar,n,c,at,v,_) <- liftIO $ execStateT (convertJboProp' $ x $ NonAnaph (names!!1)) $ emptyState s
    --addAtom (Gen name ,(\name -> Gen $ evalLink "smuni" [Gen meaning, name])) --XXX why does this line still exixt
    addAtom (named,(\named -> evalLink "cmene" [name, named]))
    addArg named
convertJboTerm (BoundVar n)                  = do
    let c = cVN ("B" ++ show n)
    addArg c
    addVar c
convertJboTerm (UnboundSumbasti x)           = convertSumtiAtom x --What is that exactly again?
convertJboTerm (JboQuote (ParsedQuote text)) = addArg $ cCN (show text) lowTv
convertJboTerm Unfilled                      = addArg $ cCN "zo'e" $ stv 1 1
convertJboTerm (JoikedTerms a t1 t2) | a == "ce" || a == "jo'u" = do
    convertJboTerm t1
    ct1 <- getArg
    convertJboTerm t2
    ct2 <- getArg
    let connectWith pred c1 c2 = do
            c3 <- randConceptNode
            addAtom (c3,(\c3 -> evalLink pred [c3, c1, c2]))
            addArg c3
    case a of
        "ce"     -> addArg (cSL [ct1,ct2])
        "jo'u"   -> addArg (cSL [ct1,ct2])
        "ce'o"   -> addArg (cLL [ct1,ct2])
        "sece'o" -> addArg (cLL [ct2,ct1])
        -- "fa'u"   -> do
        "joi"    -> connectWith "mass" ct1 ct2
        "je'o"   -> connectWith "union" ct1 ct2
        "ku'a"   -> connectWith "intersection" ct1 ct2
        "pi'u"   -> connectWith "crossProduct" ct1 ct2
        "sepi'u" -> connectWith "crossProduct" ct2 ct1
--convertJboTerm (Var x) = _convertJboTerm_body
--convertJboTerm (JboErrorQuote x) = _convertJboTerm_body
--convertJboTerm (JboNonJboQuote x) = _convertJboTerm_body
--convertJboTerm (TheMex x) = _convertJboTerm_body
--convertJboTerm (Valsi x) = _convertJboTerm_body
--convertJboTerm (QualifiedTerm x1 x2) = _convertJboTerm_body
convertJboTerm a = error $ show a ++ "JboTerm Incompelte"

evalLink s l = cEvalL highTv (cPN s lowTv) (cLL l)

apply :: (t -> (a, t)) -> t -> [a]
apply f l = x : r
    where (x,xs) = f l
          r      = apply f xs

randConceptNode = do
    stdgen <- liftIO newStdGen
    return $ cCN (take 20 $ randomRs ('a','z') stdgen) lowTv

convertSumtiAtom :: SumtiAtom -> IOState ()
convertSumtiAtom (LerfuString s) = convertLerfu $ head s

convertLerfu :: Lerfu -> IOState ()
convertLerfu (LerfuChar s) = addArg $ cCN [s] lowTv
convertLerfu (LerfuValsi s) = addArg $ cCN s lowTv

convertJboMex :: JboMex -> IOState ()
convertJboMex (MexInt i) = addArg $ cNN (show i)
