module ReplLanguage where

import           Control.Monad.State

import           Data.Maybe          (fromMaybe)

import           WhileLanguage

-- a ReplExpr is either an assignment or a program
data ReplExpr = Ass Definition | Prog ProgramExpr

-- a definition binds a program to a name
data Definition = Def Id ProgramExpr
data ProgramExpr = AP VarName VarName Int -- x := y + n
                 | AM VarName VarName Int -- x := y - n
                 | Seq ProgramExpr ProgramExpr -- P1;P2 : sequential composition
                 | L1 VarName ProgramExpr -- LOOP x P : repeat P value(x) times
                 | L2 Int ProgramExpr -- LOOP n P : repeat P n times
                 | W Predicate ProgramExpr -- WHILE (pred(x)) P : while pred(x) is true repeat P
                 | IF Predicate ProgramExpr ProgramExpr -- IF (pred(x)) P ELSE Q
                 | I Id -- the name of a previously defined programm

data Predicate = P VarName Ordering Int

type Id = String
type VarName = String

{--
do [x',y'] <- resolveVar [x,y]
   return (x' =$ y' +$ n)

do p' <- translate p
   q' <- translate q'
   return (p' & q')
--}

translate :: ProgramExpr -> ReplM WhileExpr
translate (AP x y n) =
  do
    xi <- resolveVar x
    yi <- resolveVar y
    return $ xi =$ yi +$ n
translate (AM x y n) =
  do
    xi <- resolveVar x
    yi <- resolveVar y
    return $ xi =$ yi -$ n
translate (Seq p q) =
  do
    t1 <- translate p
    t2 <- translate q
    return $ t1 & t2
translate (L1 x p) =
  do
    xi <- resolveVar x
    t <- translate p
    return $ loop xi t
translate (L2 n p) =
  do
    xi <- resolveVar =<< freshVar
    t <- translate p
    return $ (xi =$ xi +$ n) & t
translate (W n p) = undefined
translate (IF pre p q) = undefined
translate (I id) = undefined

type ReplM = State VarTable
data VarTable = VT { next  :: Int
                   , table :: [(VarName,Int)] }


merge :: (Eq k, Eq v) => [(k,v)] -> [(k,v)] -> Maybe [(k,v)]
merge [] ys = Just ys
merge xs [] = Just xs
merge ((k,v):xs) ys = case lookup k ys of
                        Just v' -> if v == v' then merge xs ys else Nothing
                        Nothing -> merge xs ((k,v):ys)

newVT :: VarTable
newVT = VT 0 []

nameDefined :: VarName -> ReplM Bool
nameDefined = undefined



resolveVar :: VarName -> ReplM Int
resolveVar = undefined

resolveVars :: [VarName] -> ReplM [Int]
resolveVars = undefined

freshVar :: ReplM VarName
freshVar = undefined

names :: [VarName]
names = letters ++ ((++) <$> names <*> letters )
  where letters = map (:[]) ['a'..'z']
