import Test.HUnit

fact 0 = 1
fact n = n * fact (n - 1)

test1 = TestCase (assertEqual "0! = 1" 1 (fact 0))