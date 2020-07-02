package reductions


import org.junit._

class ParallelParenthesesBalancingSuite {

  import ParallelParenthesesBalancing._

  @Test def `calculate unbalanced pars in segment with no paralelization`: Unit = {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, input.length) == expected,
        s"balance($input) should be $expected")

    check("", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
    check(")()(", false)
    check("(())", true)
    check("))()(())()((", false)
  }

  @Test def `calculate unbalanced pars in segment with two paralelizations`: Unit = {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, input.length / 2) == expected,
        s"balance($input) should be $expected")

    check("..()..", true)
    check("(.().)", true)
    check(")))(((", false)
  }

  @Test def `calculate unbalanced pars in segment with multiple paralelizations`: Unit = {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 1) == expected,
        s"parbalance($input) should be $expected")

//    check("..()..", true)
    check("(.().)", true)
//    check(")))(((", false)
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)

}
