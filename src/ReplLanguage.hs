module ReplLanguage where

import           Control.Monad.State

import           WhileLanguage
import           WhilePrograms

-- a ReplExpr is either an assignment or a program
data ReplExpr = Asngmt Definition | Prog ProgramExpr

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
                 | Clear VarName -- clear a varibale, i.e., set its value to 0
                                 --(this is almost the same as if the variable were undefined)

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
    tp <- translate p
    tq <- translate q
    return $ tp & tq
translate (L1 x p) =
  do
    xi <- resolveVar x
    tp <- translate p
    return $ loop xi tp
translate (L2 n p) =
  do
    fi <- resolveVar =<< freshVar
    tp <- translate p
    return $ (fi =$ fi +$ n) & loop fi tp
translate (W (P x ord n) prog) =
  do
    f1 <- resolveVar =<< freshVar
    f2 <- resolveVar =<< freshVar
    xi <- resolveVar x
    tp <- translate prog
    return $ case ord of
      GT -> whileGreaterThen xi n tp f1
      LT -> whileLessThen xi n tp f1
      EQ -> whileEqual xi n tp f1 f2
translate (IF (P x ord n) p q) =
  do
    f1 <- resolveVar =<< freshVar
    f2 <- resolveVar =<< freshVar
    f3 <- resolveVar =<< freshVar
    xi <- resolveVar x
    tp <- translate p
    tq <- translate q
    return $ case ord of
      GT -> ifGreaterThen xi n tp tq f1 f2
      LT -> ifLessThen xi n tp tq f1 f2
      EQ -> ifEqualThen xi n tp tq f1 f2 f3
translate (I i) = resolveId i >>= translate
translate (Clear x) =
  do
    xi <- resolveVar x
    return $ clearVar xi

type ReplM = State DefTable
data DefTable = DT { nextFreeVar :: Int
                   , vars        :: [(VarName,Int)]
                   , progs       :: [(Id,ProgramExpr)] }


merge :: (Eq k, Eq v) => [(k,v)] -> [(k,v)] -> Maybe [(k,v)]
merge [] ys = Just ys
merge xs [] = Just xs
merge ((k,v):xs) ys = case lookup k ys of
                        Just v' -> if v == v' then merge xs ys else Nothing
                        Nothing -> merge xs ((k,v):ys)

newDT :: DefTable
newDT = DT 0 [] []

nameDefined :: VarName -> ReplM Bool
nameDefined = undefined

-- this does NOT lookup a value of a variable insted the result is the
-- "address" of that variable, i.e., the entry in the VarTable witch is
-- associated with the VarName
resolveVar :: VarName -> ReplM Int
resolveVar = undefined

resolveVars :: [VarName] -> ReplM [Int]
resolveVars = undefined

freshVar :: ReplM VarName
freshVar = undefined

names :: [VarName]
names = letters ++ ((++) <$> names <*> letters )
  where letters = map (:[]) ['a'..'z']

resolveId :: Id -> ReplM ProgramExpr
resolveId = undefined
