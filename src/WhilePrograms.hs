-- this module provides translations for some of the syntatic features of the
-- repl language. It is meant to be used only for that purpose and therefore lacks
-- some security invariants to make sure all function behave as expected if invoked
-- with "arbitrary" arguments.
module WhilePrograms (
  clearVar,
  whileGreaterThen,
  whileLessThen,
  whileEqual,
  ifGreaterThen,
  ifLessThen,
  ifEqualThen
  ) where

import           WhileLanguage

type FreshVar = Int -- this type indicates that the variable should be a 0-valued
                    -- variable that is used internaly only
type VarName = Int

clearVar :: VarName -> WhileExpr
clearVar x = loop x (x =$ x -$ 1)

clearVars :: [VarName] -> WhileExpr
clearVars = foldl (&) emptyProgram . map clearVar

whileGreaterThen :: VarName -> Int -> WhileExpr -> FreshVar -> WhileExpr
whileGreaterThen x n p f =
  greaterThen x n f &
  while f
  (
    p &
    greaterThen x n f
  )

ifGreaterThen :: VarName -> Int -> WhileExpr -> WhileExpr -> FreshVar -> FreshVar -> WhileExpr
ifGreaterThen x n p q f1 f2 =
  greaterThen x n f1 &
  f2 =$ f2 +$ 1 & -- indicator variable
  while f1 -- Then
  (
    p &
    clearVars [f1, f2]  -- stop the loop by setting f1 to 0
  ) &                   -- and set f2 to 0 to skip else clause
  while f2 -- ELSE
  (
    q &
    clearVar f2 -- stop the loop by setting f2 to 0
  )

ifLessThen :: VarName -> Int -> WhileExpr -> WhileExpr -> FreshVar -> FreshVar -> WhileExpr
ifLessThen x n p q f1 f2 =
  lessThen x n f1 &
  f2 =$ f2 +$ 1 & -- indicator variable
  while f1 -- Then
  (
    p &
    clearVars [f1, f2]  -- stop the loop by setting f1 to 0
  ) &                   -- and set f2 to 0 to skip else clause
  while f2 -- ELSE
  (
    q &
    clearVar f2 -- stop the loop by setting f2 to 0
  )

ifEqualThen :: VarName -> Int -> WhileExpr -> WhileExpr -> FreshVar -> FreshVar -> FreshVar -> WhileExpr
ifEqualThen x n p q f1 f2 f3 =
  equals x n f1 f2 &
  f3 =$ f3 +$ 1 & -- indicator variable
  while f1 -- Then
  (
    p &
    clearVars [f1, f3]  -- stop the loop by setting f1 to 0
  ) &                   -- and set f3 to 0 to skip else clause
  while f3 -- ELSE
  (
    q &
    clearVar f3 -- stop the loop by setting f3 to 0
  )

whileLessThen :: VarName -> Int -> WhileExpr -> FreshVar -> WhileExpr
whileLessThen x n p f =
  lessThen x n f &
  while f
  (
    p &
    clearVar f &  -- "f := 0"
    lessThen x n f
  )

whileEqual :: VarName -> Int -> WhileExpr -> FreshVar -> FreshVar -> WhileExpr
whileEqual x n p f1 f2 =
  equals x n f1 f2 &    -- "x == n ?"
  while f2
  (
    p &
    clearVars [f1,f2] & -- "f1 := 0; f2 := 0"
    equals x n f1 f2    -- "x == n ?"
  )

-- greaterThen x n f tests "x > n",
-- if "x > n" holds f has a value other Then 0 and 0 otherwise
greaterThen :: VarName -> Int -> FreshVar -> WhileExpr
greaterThen x n f = f =$ x -$ n -- "f := x - n"

-- greaterThen x n f tests "x < n",
-- if "x < n" holds f has a value other Then 0 and 0 otherwise
lessThen :: VarName -> Int -> FreshVar -> WhileExpr
lessThen x n = sub n x -- "f := n - x"

-- equals x n f1 f2 tests "x == n",
-- if "x == n" holds f2 has value 1 afterwards and 0 otherwise
equals :: VarName -> Int -> FreshVar -> FreshVar -> WhileExpr
equals x n f1 f2 =
  f1 =$ x -$ n &  -- "f1 := x - n"
  sub n x f2 &    -- "f2 := n - x"
  loop f2
  (
    f1 =$ f1 +$ 1 -- "f1 := f1 +f2"
  ) &
  clearVar f2 &   -- "f2 := 0"
  f2 =$ f2 +$ 1 & -- "f2 := 1"
  loop f1
  (
    f2 =$ f2 -$ 1 -- "f2 := 1 - f1"
  )

--  sub n x f is a WHILE program that computes "f := n - x"
sub :: Int -> VarName -> FreshVar -> WhileExpr
sub n x f =
  f =$ f +$ n &
  loop x
  (
    f =$ f -$ 1 -- "n - x"
  )
