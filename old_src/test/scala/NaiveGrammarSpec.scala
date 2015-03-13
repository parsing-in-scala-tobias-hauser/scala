/** Tests for exercise 3.2 */

import org.scalatest._

import NaiveGrammar._

class NaiveGrammarSpec extends FlatSpec {

  def parseAE = parseGrammar(ae)

  val n1234: Tree =
    Branch('exp, List(Leaf('num, "1234")))

  val sumOf1And2: Tree =
    Branch('exp, List(
      Branch('add, List(
        Leaf('keyword, "sum of "),
        Branch('exp, List(Leaf('num, "1"))),
        Leaf('keyword, " and "),
        Branch('exp, List(Leaf('num, "2")))))))

  "ae" should "parse arithmetic expressions" in {
    assert(parseAE("1234") == n1234)

    assert(parseAE("sum of 1 and 2") == sumOf1And2)

    assert(parseAE("product of sum of 1 and 2 and 1234") ==
      Branch('exp, List(
        Branch('mul, List(
          Leaf('keyword, "product of "),
          sumOf1And2,
          Leaf('keyword, " and "),
          n1234)))))
  }


  it should "parse and simplify arithmetic expressions" in {
    assert(parseAndSimplifyAE("1234") == Leaf('num, "1234"))

    assert(parseAndSimplifyAE("sum of 1 and 2") ==
      Branch('add, List(
        Leaf('num, "1"),
        Leaf('num, "2"))))
        
    
    
    assert(parseAndSimplifyAE("product of sum of 1 and 2 and 1234") ==
      Branch('mul, List(
        Branch('add, List(
          Leaf('num, "1"),
          Leaf('num, "2"))),
        Leaf('num, "1234"))))
        
        
  }
  
 

  
  //My Tests:
  
  "parseNonterminal" should "produce a parser for the given nonterminal" in {
  	assert(parseNonterminal(add, ae)("sum of 1 and 1") == 
  		Some( 
  			(Branch('add,List(Leaf('keyword,"sum of "), 
  			Branch('exp,List(Leaf('num,"1"))), 
  			Leaf('keyword," and "), 
  			Branch('exp,List(Leaf('num,"1"))))), 
  			"")
  		) 
  	)
  	
  	assert(parseNonterminal(mul, ae)("product of 1 and 1") == 
  		Some(
  			(Branch('mul,List(Leaf('keyword,"product of "), 
  			Branch('exp,List(Leaf('num,"1"))), 
  			Leaf('keyword," and "), 
  			Branch('exp,List(Leaf('num,"1"))))),
  			"")
  		)
  	)
  	
  	assert(parseNonterminal(exp, ae)("1 and 1") == 
  		Some(
  		(Branch('exp,List(Leaf('num,"1")))," and 1")) 
  	)
  	
  }
  
  
  
  
  "parseRHS" should "create a List of syntax tree nodes for all parts mentioned in the right hand side of a production rule" in {
  
  	assert(parseRHS(sumOf ~ exp ~ and ~ exp, ae)("sum of 1 and 1") ==
     Some((
       List(
         Leaf('keyword, "sum of "),           // keyword "sum of "
         Branch('exp, List(Leaf('num, "1"))), // left operand
         Leaf('keyword, " and ")    ,         // keyword " and "
         Branch('exp, List(Leaf('num, "1")))  // right operand
       ),
       ""
     )))
  
   val threeNums = num | (num ~ and ~ num) | (num ~ and ~ num ~ and ~ num)
  
   assert(parseRHS(threeNums, ae)("1") ==
     Some((
       List(Leaf('num, "1")),
       ""
     )))
  
   assert(parseRHS(threeNums, ae)("1 and 2") ==
     Some((
       List(
         Leaf('num, "1"),
         Leaf('keyword, " and "),
         Leaf('num, "2")
       ),
       ""
     )))
  
   assert(parseRHS(threeNums, ae)("1 and 2 and 3") ==
     Some((
       List(
         Leaf('num, "1"),
         Leaf('keyword, " and "),
         Leaf('num, "2"),
         Leaf('keyword, " and "),
         Leaf('num, "3")
       ),
       ""
     )))
  }
  
  
}//end class

