-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}

-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3)  ==  (((() . +) . 2) . 3)
--          ==>  Scons (Scons (Scons Snil (Ssym "+"))
--                            (Snum 2))
--                     (Snum 3)
--                   
-- (/ (* (- 68 32) 5) 9)
--     ==  (((() . /) . (((() . *) . (((() . -) . 68) . 32)) . 5)) . 9)
--     ==>
-- Scons (Scons (Scons Snil (Ssym "/"))
--              (Scons (Scons (Scons Snil (Ssym "*"))
--                            (Scons (Scons (Scons Snil (Ssym "-"))
--                                          (Snum 68))
--                                   (Snum 32)))
--                     (Snum 5)))
--       (Snum 9)

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                pChar '\n'; return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment); return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(shorthand-quote E)"
-- La notation "`E" est équivalente à "(shorthand-backquote E)"
-- La notation ",E" est équivalente à "(shorthand-comma E)"
pQuote :: Parser Sexp
pQuote = do { c <- satisfy (\c -> c `elem` "'`,"); pSpaces; e <- pSexp;
              return (Scons
                      (Scons Snil
                             (Ssym (case c of
                                     ',' -> "shorthand-comma"
                                     '`' -> "shorthand-backquote"
                                     _   -> "shorthand-quote")))
                      e) }

-- Une liste (Tsil) est de la forme ( [e .] {e} )
pTsil :: Parser Sexp
pTsil = do _ <- char '('
           pSpaces
           (do { _ <- char ')'; return Snil }
            <|> do hd <- (do e <- pSexp
                             pSpaces
                             (do _ <- char '.'
                                 pSpaces
                                 return e
                              <|> return (Scons Snil e)))
                   pLiat hd)
    where pLiat :: Sexp -> Parser Sexp
          pLiat hd = do _ <- char ')'
                        return hd
                 <|> do e <- pSexp
                        pSpaces
                        pLiat (Scons hd e)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pTsil <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _ s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                        --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
    where showHead (Scons Snil e') = showString "(" . showSexp' e'
          showHead (Scons e1' e2')
            = showHead e1' . showString " " . showSexp' e2'
          showHead e = showString "(" . showSexp' e . showString " ."

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire                                          --
---------------------------------------------------------------------------

type Var = String

data Ltype = Lint               -- Int
           | Lboo               -- Bool
           | Larw Ltype Ltype   -- τ₁ → τ₂
           | Ltup [Ltype]       -- tuple τ₁...τₙ
           deriving (Show, Eq)

data Lexp = Lnum Int                    -- Constante entière.
          | Lvar Var                    -- Référence à une variable.
          | Lhastype Lexp Ltype         -- Annotation de type.
          | Lcall Lexp Lexp             -- Appel de fonction, avec un argument.
          | Lfun Var Lexp               -- Fonction anonyme prenant un argument.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Llet [(Var, Lexp)] Lexp
          | Lif Lexp Lexp Lexp          -- Expression conditionelle.
          | Ltuple [Lexp]               -- Construction de tuple
          | Lfetch Lexp [Var] Lexp      -- lecture d'un tuple
          deriving (Show, Eq)


---------------------------------------------------------------------------
-- Conversion de Sexp à Lexp                                             --
---------------------------------------------------------------------------

sexp2list :: Sexp -> [Sexp]
sexp2list s = loop s []
    where
      loop (Scons hds tl) acc = loop hds (tl : acc)
      loop Snil acc = acc
      loop _ _ = error ("Improper list: " ++ show s)


-- Analyse une Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s
s2l (se@(Scons _ _)) = case sexp2list se of
    (Ssym "hastype" : e : t : []) -> Lhastype (s2l e) (s2t t) 
    (Ssym "call" : fun : arguments) -> let funRe arg = case arg of
                                            []  -> error ("Unrecognized call expression: " 
                                                            ++ (showSexp se))
                                            [x] -> Lcall (s2l fun) (s2l x)
                                            (x:xs) -> Lcall (funRe xs) (s2l x)
                                       in funRe (reverse arguments)
    (Ssym "fun" : e) -> let funRe u = case u of
                                            []  -> error ("Unrecognized function expression: " 
                                                            ++ (showSexp se))
                                            (Ssym x:y:[]) -> Lfun (x) (s2l y)
                                            (Ssym x:xs) -> Lfun (x) (funRe xs)
                                            _ -> error ("Surement une variable invalide dans la fonction: "
                                                            ++ (showSexp se))
                                       in funRe e

    --On cherche d'abord la forme de notre let (si on a multiples 'd' ou non.
    --Puis sur nos 'd', on appel recursivement la fonction externe 's2let'
    --qui va evaluer 'd' pour les 3 sortes de 'd'.
    (Ssym "let" : a) -> let funRe u = case u of
                             [] -> error ("Unrecognized let expression: " 
                                                     ++ (showSexp se))
                             [_] -> error ("Uncomplete let expression: " 
                                                     ++ (showSexp se))
                             [d,_] -> 
                                   let funRe1 m = case sexp2list m of
                                        [Ssym x,e] -> [(x , s2l e)]  
                                        [Ssym x,t,e] -> 
                                            [(x, Lhastype (s2l e) (s2t t))]
                                        (Ssym x:xs)-> [(x, s2let xs)]
                                        _ -> error ("Mauvais appel de variables dans let: " 
                                                ++ (showSexp se))
                                   in funRe1 d
                             (d:ds) -> let funRe2 m = case sexp2list m of
                                            [Ssym x,e] -> (x , s2l e) : funRe ds
                                            [Ssym x,t,e] -> 
                                                (x, Lhastype (s2l e) (s2t t)) : funRe ds
                                            (Ssym x:xs)-> (x, s2let xs) : funRe ds
                                            _ -> error ("Mauvais appel de variables dans let: " 
                                                    ++ (showSexp se))
                                        in funRe2 d
                       in Llet (funRe a) (s2l (last a))


    (Ssym "if" : a : b : c : []) -> Lif (s2l a) (s2l b) (s2l c)
    (Ssym "tuple" : x ) -> Ltuple (map s2l x)
    (Ssym "fetch" : tuple : v : e : []) -> 
        Lfetch (s2l tuple) (map showSexp (sexp2list v)) (s2l e)
    _ -> error ("Unrecognized Psil expression: " ++ (showSexp se))
s2l se = error ("Unrecognized Psil expression: " ++ (showSexp se))

--Voici 's2let', qui prend notre 'd' transforme en liste de Sexp.
--Si 'd' est une declaration de fonction avec son type, alors
--nous devons faire appel a 2 fonctions externes pour construire
--notre retour 'Lhastype'.
s2let :: [Sexp] -> Lexp
s2let table = let funRe u var typ = case u of
                                [] -> error ("Faulty variables in let")
                                [a, b] -> 
                                    Lhastype (s2letFun var b) (s2letType typ a)
                                (x:xs) -> 
                                    let funRe2 g = case sexp2list g of
                                            [a, b] -> funRe xs (a:var) (b:typ)
                                            _ -> error ("Appel 'let' invalid" 
                                                    ++ (showSexp g))
                                    in funRe2 x
                    in funRe table [] []


--Fonction modifie de "s2l Lfun" pour construire des Lfun imbrique
s2letFun :: [Sexp] -> Sexp -> Lexp
s2letFun var e = let funRe u = case u of
                                 []  -> error ("Unrecognized function expression"
                                                 ++ (showSexp e))
                                 (Ssym x:[]) -> Lfun (x) (s2l e)
                                 (Ssym x:xs) -> Lfun (x) (funRe xs)
                                 _ -> error ("Surement une variable invalide dans la fonction: " 
                                                ++ (showSexp e))
                               in funRe (reverse var)
--Fonction similaire a 's2t' pour construire des types en
--sucre syntaxique
s2letType :: [Sexp] -> Sexp -> Ltype
s2letType typ t = let funRe u = case u of 
                       [] -> error ("Empty type call in let")
                       [x] -> Larw (s2t x) (s2t t)
                       (x:xs) -> Larw (s2t x) (funRe xs)
                in funRe (reverse typ)

s2t :: Sexp -> Ltype
s2t (Ssym "Int") = Lint
s2t (Ssym "Bool") = Lboo
s2t (se@(Scons _ _)) = case sexp2list se of
    (Ssym "Tuple" : t ) -> Ltup (map s2t t)
    --cas pour autres est forcement un "->"
    --mais il n'est pas au debut donc il faut 
    --verifier a l'interieur                        
    (a) -> let funRe u = case u of 
                       [x,Ssym "->",z] -> Larw (s2t x) (s2t z)
                       (x:xs) -> Larw (s2t x) (funRe xs)
                       _ -> error ("Unrecognized arrow type: " 
                                    ++ (showSexp se))
                in funRe a

s2t s = error ("Unrecognized Psil type: " ++ (showSexp s))

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

-- Type des valeurs renvoyées par l'évaluateur.
data Value = Vnum Int
           | Vbool Bool
           | Vtuple [Value]
           | Vfun (Maybe String) (Value -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec p (Vtuple vs) = showValues "[" vs
        where showValues _ [] = showString "]"
              showValues sep (v:vs')
                = showString sep . showsPrec p v . showValues " " vs'
    showsPrec _ (Vfun (Just n) _)
      = showString "<fun " . showString n . showString ">"
    showsPrec _ (Vfun Nothing _) = showString "<fun>"

type Env = [(Var, Value, Ltype)]

-- L'environnement initial qui contient les fonctions prédéfinies et leur type.
env0 :: Env
env0 = [prim "+"  (+) Vnum  Lint,
        prim "-"  (-) Vnum  Lint,
        prim "*"  (*) Vnum  Lint,
        prim "/"  div Vnum  Lint,
        prim "="  (==) Vbool Lboo,
        prim ">=" (>=) Vbool Lboo,
        prim "<=" (<=) Vbool Lboo]
       where prim name op cons typ
               = (name,
                  Vfun (Just name)
                       (\ (Vnum x) -> Vfun Nothing
                                          (\ (Vnum y) -> cons (x `op` y))),
                  Larw Lint (Larw Lint typ))

-- Point d'entrée de l'évaluation
eval :: Env -> Lexp -> Value
eval env e
  -- Extrait la liste des variables et la liste de leur valeurs,
  -- et ignore leurs types, qui n'est plus utile pendant l'évaluation.
  = eval2 (map (\(x,_,_) -> x) env) e (map (\(_,v,_) -> v) env)

e2lookup :: [Var] -> Var -> Int          -- Find position within environment
e2lookup env x = e2lookup' env 0
    where e2lookup' :: [Var] -> Int -> Int
          e2lookup' [] _ = error ("Variable inconnue: " ++ show x)
          e2lookup' (x':_) i | x == x' = i
          e2lookup' (_:xs) i = e2lookup' xs (i+1)

-------------- La fonction d'évaluation principale.  ------------------------
-- Au lieu de recevoir une liste de paires (Var, Val), on passe la liste
-- des noms de variables (`senv`) et la liste des valeurs correspondantes
-- (`venv`) séparément de manière à ce que (eval2 senv e) renvoie une
-- fonction qui a déjà fini d'utiliser `senv`.
eval2 :: [Var] -> Lexp -> ([Value] -> Value)
eval2 _    (Lnum n) = \_ -> Vnum n
eval2 senv (Lhastype e _) = eval2 senv e
eval2 senv (Lvar x)
  -- Calcule la position que la variable aura dans `venv`.
  = let i = e2lookup senv x
    -- Renvoie une fonction qui n'a plus besoin de chercher et comparer le nom.
    -- De cette manière, si la fonction renvoyée par (eval2 senv v) est appelée
    -- plusieurs fois, on aura fait la recherche dans `senv` une seule fois.
    in \venv -> venv !! i
eval2 senv (Lfun var e) = \vals -> Vfun Nothing (\val -> eval2 (var:senv) e (val:vals))

--On isole notre 'value -> value' de la fonction 'f' et lui passe
--comme argument la valeur de l'argument de Lcall
eval2 senv (Lcall f a) = \vals -> let e1' = eval2 senv f 
                                      v1 = e1' vals
                                  in case v1 of
                                     Vfun _ fun -> fun (eval2 senv a vals)
                                     _ -> (eval2 senv a vals)

--On cree 2 listes des variables et leurs expressions evalue.
--Puis on evalue notre expression de let dans l'environement
--de tous nos variables(et leurs valeurs).
eval2 senv (Llet var e) = \venv -> 
              --liste des variables
                        let noms n var' = case var' of
                                    [(x,_)] -> x:n
                                    (x,_):xs -> x : (noms n xs)
                                    _ -> error ("Mauvaise construction de let"
                                                ++ " ,variables incorrectes")
                            newSenv = noms senv var
                            valeurs v var' = case var' of
                                    [(_,y)] -> 
                                        let v' = eval2 newSenv y newVals 
                                        in v': v
                                    (_,y):xs -> 
                                        let v' = eval2 newSenv y newVals 
                                        in v' : (valeurs v xs)
                                    _ -> error ("Mauvaise construction de let"
                                                    ++ " ,expressions incorrectes")
                            newVals = valeurs venv var
                        in eval2 newSenv e newVals


eval2 senv (Lif e1 e2 e3) = \vals -> 
                            case eval2 senv e1 vals of
                                    (Vbool True) -> eval2 senv e2 vals
                                    (Vbool False) -> eval2 senv e3 vals
                                    _ -> error ("Uknown 'if' condition result")

eval2 senv (Ltuple e) = \vals -> let fonc x = eval2 (senv) (x) (vals)
                                     listeVal = map fonc e
                                  in Vtuple listeVal

eval2 senv (Lfetch e1 var e2) = \vals -> 
                                case e1 of
                                        Ltuple tup ->
                                --utilise let pour evaluer notre expressions
                                --avec les nouvelles variables 
                                            eval2 senv (Llet (zip var 
                                                tup) e2) vals
                                        _ -> error ("Expression Lfetch does not contain a Tuple type as first argument")


---------------------------------------------------------------------------
-- Vérificateur de types                                                 --
---------------------------------------------------------------------------

type TEnv = [(Var, Ltype)]
type TypeError = String

-- Les valeurs ne servent à rien pendant la vérification de type,
-- donc extrait la partie utile de `env0`.
tenv0 :: TEnv
tenv0 = (map (\(x,_,t) -> (x,t)) env0)

tlookup :: [(Var, a)] -> Var -> a
tlookup [] x = error ("Variable inconnue: " ++ x)
tlookup ((x',t):_) x | x == x' = t
tlookup (_:env) x = tlookup env x

infer :: TEnv -> Lexp -> Ltype
infer _ (Lnum _) = Lint
infer tenv (Lvar x) = tlookup tenv x
infer _ (Lfun _ _)     = error "Can't infer type of `fun`"
infer _ (Lfetch _ _ _) = error "Can't infer type of `fetch`"
infer _ (Lif _ _ _)    = error "Can't infer type of `if`"
infer tenv (Lhastype e t) = case check tenv e t of
                               Nothing -> t
                               Just n -> error n
infer tenv (Lcall e1 e2) = case infer tenv e1 of --e1=>(t1->t2)
                                                        --e2 <= t1
                                (Larw t1 t2) -> case check tenv e2 t1 of
                                            Nothing -> t2
                                            Just n -> error n
                                _ -> error "Mauvaise construction de call"

--Utilisation similaire que l'eval2 let
infer tenv (Llet var e) = let build env var' = case var' of
                                        [(x,y)] -> 
                                            (x, infer newTenv y):env
                                        (x,y):xs -> 
                                            (x, infer newTenv y) : (build env xs)
                                        _ -> error "Mauvaise construction de let"
                              newTenv = build tenv var
                          in infer newTenv e



infer tenv (Ltuple e) = Ltup (map (infer tenv) e )


-- ¡¡¡ COMPLETER ICI !!! --

check :: TEnv -> Lexp -> Ltype -> Maybe TypeError
check tenv (Lfun x body) (Larw t1 t2) = check ((x,t1):tenv) body t2
check _ (Lfun _ _) t = Just ("Expected a function type: " ++ show t)
check tenv (Lif e1 e2 e3) t = case check tenv e1 Lboo of
                                Nothing -> case check tenv e2 t of
                                            Nothing -> case check tenv e3 t of
                                                        Nothing -> Nothing
                                                        Just n-> error n
                                            Just n-> error n
                                Just n-> error n
check tenv (Lfetch e1 var e2) t = case infer tenv e1 of
                          --Creation du nouvel environement avec l'aide de 'zip'
                                    Ltup n -> case check ((zip var n)
                                                        ++tenv) e2 t of
                                                    Nothing -> Nothing
                                                    Just o -> error o
                                    _ -> error "Incorrect fetch type"
-- ¡¡¡ COMPLETER ICI !!! --
check tenv e t
  -- Essaie d'inférer le type et vérifie alors s'il correspond au
  -- type attendu.
  = let t' = infer tenv e
    in if t == t' then Nothing
       else Just ("Type mismatch: " ++ show t ++ " != " ++ show t')


---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename
  = do filestring <- readFile filename
       (hPutStr stdout)
           (let sexps s = case parse pSexps filename s of
                            Left _ -> [Ssym "#<parse-error>"]
                            Right es -> es
            in (concat
                (map (\ sexp -> let { ltyp = infer tenv0 lexp
                                   ; lexp = s2l sexp
                                   ; val = eval env0 lexp }
                               in "  " ++ show val
                                  ++ " : " ++ show ltyp ++ "\n")
                     (sexps filestring))))

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

typeOf :: String -> Ltype
typeOf = infer tenv0 . lexpOf

valOf :: String -> Value
valOf = eval env0 . lexpOf




