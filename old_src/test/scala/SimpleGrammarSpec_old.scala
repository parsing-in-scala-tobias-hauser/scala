


import org.scalatest._

import SimpleGrammar._

class SimpleGrammarSpec extends FlatSpec {
  it should "parse and simplify arithmetic expressions" in {
    assert(parseAE("1234") == Leaf('num, "1234"))

    assert(parseAE("sum of 1 and 2") ==
      Branch('add, List(
        Leaf('num, "1"),
        Leaf('num, "2"))))

    assert(parseAE("product of sum of 1 and 2 and 1234") ==
      Branch('mul, List(
        Branch('add, List(
          Leaf('num, "1"),
          Leaf('num, "2"))),
        Leaf('num, "1234"))))
  }

  // Your tests here
  //"You" should "write more tests" in fail("not finished yet")
  
  "parseNonterminal" should "produce a parser for the given nonterminal" in {
  	assert(parseNonterminal(add, ae)("sum of 1 and 1") == 
  		Some( 
  			(Branch('add,List( 
  			Leaf('num,"1"), 
  			Leaf('num,"1")))), ""))
  	
  	assert(parseNonterminal(mul, ae)("product of 1 and 1") == 
  		Some(
  			(Branch('mul,List( 
  			Leaf('num,"1"), 
  			Leaf('num,"1")))), ""))  	
  }
  
  "parseRHS" should "create a List of syntax tree nodes for all parts mentioned in the right hand side of a production rule" in {  
  	assert(parseRHS(sumOf ~ exp ~ and ~ exp, ae)("sum of 1 and 1") ==
  		Some((List(Leaf('num,"1"), Leaf('num,"1")), "")) )     
    }
  
}//end



