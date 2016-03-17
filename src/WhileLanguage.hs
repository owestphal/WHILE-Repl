module WhileLanguage (
  WhileExpr,
  eval,
  (&),
  loop,
  while,
  (=$),
  (+$),
  (-$),
  emptyProgram
  ) where

import           Data.Maybe (fromMaybe)

-- datatype for WHILE-expressions / WHILE-programs
data WhileExpr = AP VarName VarName Int -- x := y + n
          | AM VarName VarName Int -- x := y - n
          | S WhileExpr WhileExpr -- P1;P2 : sequential composition
          | L VarName WhileExpr -- LOOP x P : repeat P value(x) times
          | W VarName WhileExpr -- WHILE x /= 0 P : while value(x) is not 0 repeat P
          | EmptyProgram -- the empty program that does nothing

-- all variable names are of the form x_i, so they are basically just Int
type VarName = Int

-- verbose constructors for expressions
infixl 4 &
(&) :: WhileExpr -> WhileExpr -> WhileExpr
p            & EmptyProgram = p
EmptyProgram & q            = q
p            & q            = S p q
loop :: VarName -> WhileExpr -> WhileExpr
loop _ EmptyProgram = EmptyProgram
loop x p            = L x p
while :: VarName -> WhileExpr -> WhileExpr
while _ EmptyProgram = EmptyProgram
while x p            = W x p
emptyProgram :: WhileExpr
emptyProgram = EmptyProgram

-- x =$ y +$ n :: WhileExpr
-- x =$ y -$ n :: WhileExpr
infixl 6 =$
infixl 5 +$
infixl 5 -$
type DelayedAssignment = (VarName,VarName)
(=$) :: VarName -> VarName -> DelayedAssignment
(=$) = (,)
(+$) :: DelayedAssignment -> Int -> WhileExpr
(x,y) +$ n = AP x y n
(-$) :: DelayedAssignment -> Int -> WhileExpr
(x,y) -$ n = AM x y n

-- representation of the enviornment, i.e. the value of each variable
-- if there is no entry for a variable its value is zero
type Env = [(VarName,Int)]

insert :: (VarName,Int) -> Env -> Env
insert x env = x : env

delete :: VarName -> Env -> Env
delete x = filter ((/= x).fst)

emptyEnv :: Env
emptyEnv = []

assignValue :: VarName -> Int -> Env -> Env
assignValue name n | n <= 0    = delete name
                   | otherwise = insert (name,n) . delete name

lookupValue :: VarName -> Env -> Int
lookupValue name env = fromMaybe 0 $ lookup name env

isZero :: VarName -> Env -> Bool
isZero name env = name `notElem` map fst env

-- evaluation of expression in a given enviornment
eval :: WhileExpr -> Env -> Env
eval (AP x y n) env = assignValue x (lookupValue y env + n) env
eval (AM x y n) env = assignValue x (lookupValue y env - n) env
eval (S e1 e2) env = eval e2 $ eval e1 env
eval (L x e) env = foldr eval env $ replicate (lookupValue x env) e
eval (W x e) env = if isZero x env
                     then env
                     else eval (W x e) (eval e env)
eval EmptyProgram env = env

-- tests/examples
p1 = 0 =$ 0 +$ 4
p2 = p1 & 1 =$ 0 +$ 3
p3 = p1 & loop 0 (0 =$ 0 +$ 1)
p4 = p1 & while 0 (1 =$ 1 +$ 1 & 0 =$ 0 -$ 3)
