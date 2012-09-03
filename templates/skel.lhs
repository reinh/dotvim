@FILE@
@AUTHOR_EMAIL@

> module @FILE@ where

Imports
-------

We need our test modules:

> import Test.HUnit
> import Test.QuickCheck

Basic Declarations
------------------

@CURSOR@

Test cases
----------

> tests = TestList $ map TestCase
>   [assertEqual "add tests here"  1 1
>   ]

QuickCkeck properties go here.

> prop_empty c1 = (c1::Int) == c1

> runTests = do
>   runTestTT tests
>   quickCheck prop_empty

For now, main will run our tests. Eventually, we will want to move the tests to their own files.

> main :: IO ()
> main = runTests
