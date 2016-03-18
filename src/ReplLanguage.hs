module ReplLanguage where

import           Control.Monad.Except
import           Control.Monad.Identity
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
    [xi, yi] <- resolveVars [x, y]
    return $ xi =$ yi +$ n
translate (AM x y n) =
  do
    [xi, yi] <- resolveVars [x, y]
    return $ xi =$ yi -$ n
translate (Seq p q) =
  do
    [p', q'] <- mapM translate [p, q]
    return $ p' & q'
translate (L1 x p) =
  do
    xi <- resolveVar x
    p' <- translate p
    return $ loop xi p'
translate (L2 n p) =
  do
    t <- tempVar
    p' <- translate p
    return $ (t =$ t +$ n) & loop t p'
translate (W (P x ord n) p) =
  do
    t1 <- tempVar
    t2 <- tempVar
    xi <- resolveVar x
    p' <- translate p
    return $ case ord of
      GT -> whileGreaterThen xi n p' t1
      LT -> whileLessThen xi n p' t1
      EQ -> whileEqual xi n p' t1 t2
translate (IF (P x ord n) p q) =
  do
    t1 <- tempVar
    t2 <- tempVar
    t3 <- tempVar
    xi <- resolveVar x
    p' <- translate p
    q' <- translate q
    return $ case ord of
      GT -> ifGreaterThen xi n p' q' t1 t2
      LT -> ifLessThen xi n p' q' t1 t2
      EQ -> ifEqualThen xi n p' q' t1 t2 t3
translate (I i) = resolveId i >>= translate
translate (Clear x) =
  do
    xi <- resolveVar x
    return $ clearVar xi

type ReplM = ExceptT String (StateT DefTable Identity)
data DefTable = DT { unusedVars      :: [Int]
                   , unusedTempNames :: [Int]
                   , usedVars        :: [(VarName,Int)]
                   , programs        :: [(Id,ProgramExpr)] }


merge :: (Eq k, Eq v) => [(k,v)] -> [(k,v)] -> Maybe [(k,v)]
merge [] ys = Just ys
merge xs [] = Just xs
merge ((k,v):xs) ys = case lookup k ys of
                        Just v' -> if v == v' then merge xs ys else Nothing
                        Nothing -> merge xs ((k,v):ys)

newDT :: DefTable
newDT = DT [0..] [0..] [] []

nameDefined :: VarName -> ReplM Bool
nameDefined x = do
  DT {usedVars = table} <- get
  return $ case lookup x table of
    Just _ -> True
    Nothing -> False

-- this does NOT lookup the value of a variable insted the result is the
-- "address" of that variable, i.e., the entry in the DefTable witch is
-- associated with the VarName.
-- If a variable is not defined in the DefTable than a new entry is created
resolveVar :: VarName -> ReplM Int
resolveVar x = do
  (DT (next:free) ftemps table progs) <- get
  case lookup x table of
    Just i -> return i
    Nothing -> do
      put $ DT free
               ftemps
               ((x,next):table)
               progs
      return next

resolveVars :: [VarName] -> ReplM [Int]
resolveVars = mapM resolveVar

tempVar :: ReplM Int
tempVar = do
  (DT (next:fVars) (i:fTemps) table progs) <- get
  put $ DT fVars
           fTemps
           (('_' : show i, next) : table)
           progs
  return next

resolveId :: Id -> ReplM ProgramExpr
resolveId x = do
  DT {programs = table} <- get
  case lookup x table of
    Just p -> return p
    Nothing -> throwError $ "program identifier " ++ x ++ " not defined!"
