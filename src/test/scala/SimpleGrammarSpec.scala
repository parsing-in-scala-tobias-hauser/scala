import org.scalatest._
import sext._
import SimpleGrammar._

class SimpleGrammarSpec extends FlatSpec {
/*
  it should "parse and simplify arithmetic expressions" in {       
    assert(parse("1234") == Leaf('num, "1234"))

    assert(parse("2 + 2") ==
      Branch('add, List(
        Leaf('num, "2"),
        Leaf('num, "2"))))

		assert(parse("3 * 5") ==
      Branch('mul, List(
        Leaf('num, "3"),
        Leaf('num, "5"))))
        
   assert(parse("5 - 2 - 1") ==
      Branch('sub, List(
        Branch('sub, List(
          Leaf('num, "5"), 
          Leaf('num, "2"))), 
        Leaf('num, "1")))) 
        
  assert(parse("5 - 2 - 1 - 3") ==
    Branch('sub, List(
      Branch('sub, List(
        Branch('sub, List(
          Leaf('num, "5"),
          Leaf('num, "2"))),
      Leaf('num, "1"))),
    Leaf('num, "3"))))  
    
    assert(parse("5 - 2 - 1 - 3 + 4") ==
      Branch('add, List(
        Branch('sub, List(
          Branch('sub, List(
            Branch('sub, List(
              Leaf('num, "5"), 
              Leaf('num, "2"))), 
            Leaf('num, "1"))), 
          Leaf('num, "3"))), 
        Leaf('num,"4"))))              
                                                                         
    assert(parse("3 + 5 / 2") ==
      Branch('add, List(
        Leaf('num, "3"), 
        Branch('div, List(
          Leaf('num, "5"), 
          Leaf('num, "2"))))))
      	
    assert(parse("3 - 2 + 4 - 2") == 
      Branch('sub, List(
        Branch('add, List(
          Branch('sub, List(
            Leaf('num, "3"), 
            Leaf('num, "2"))), 
          Leaf('num, "4"))), 
         Leaf('num, "2"))))            
    
    assert(parse("1256 + 25 * 48 / 9") == 
      Branch('add, List(
        Leaf('num, "1256"), 
        Branch('div, List(
          Branch('mul, List(
            Leaf('num, "25"), 
            Leaf('num, "48"))), 
          Leaf('num, "9"))))))
          
    assert(parse("2 + 2 == 4") == 
      Branch('comp, List(
        Branch('add, List(
          Leaf('num, "2"), 
          Leaf('num, "2"))), 
        Leaf('num, "4"))))
         
    assert(parse("25 * 8 == 500 / 2 - 50") == 
      Branch('comp, List(
        Branch('mul, List(
          Leaf('num, "25"), 
          Leaf('num, "8"))), 
        Branch('sub, List(
          Branch('div, List(
            Leaf('num, "500"), 
            Leaf('num, "2"))), 
          Leaf('num, "50"))))))
          
    assert(parse("if 1 == 1 then 2 else 3") == 
      Branch('cond, List(
        Branch('comp,List(   //if
          Leaf('num, "1"), 
          Leaf('num, "1"))), 
        Leaf('num, "2"),    //then
        Leaf('num, "3"))))  //else
      
    assert(parse("if 2 + 2 == 5 then 1900 + 84 else 5 * 403") == 
      Branch('cond, List(
        Branch('comp, List(      //if
          Branch('add, List(
            Leaf('num, "2"), 
            Leaf('num, "2"))), 
          Leaf('num, "5"))), 
        Branch('add, List(      //then
          Leaf('num, "1900"), 
          Leaf('num, "84"))), 
        Branch('mul, List(      //else
          Leaf('num, "5"), 
          Leaf('num, "403")))))) 
          
    assert(parse("if 2 + 2 == 5 then if 1 == 2 then 0 else 1 else 5 * 403") == 
      Branch('cond,List(
        Branch('comp,List(    //if
          Branch('add,List(
            Leaf('num, "2"), 
            Leaf('num, "2"))), 
            Leaf('num, "5"))), 
        Branch('cond,List(    //then
          Branch('comp,List(      //if
            Leaf('num, "1"), 
            Leaf('num, "2"))), 
          Leaf('num, "0"),        //then
          Leaf('num, "1"))),      //else
        Branch('mul,List(     //else
          Leaf('num, "5"), 
          Leaf('num, "403"))))))            	      	     		       
  }//end
  
  
  
  "eval" should "evaluate a given tree" in {     
    val test0 = parse("1234")
             
    val test1 = parse("2 + 3")
    val test2 = parse("2 * 3")
    val test3 = parse("5 - 2")
    val test4 = parse("16 / 4")
    
    val test5 = parse("(2 + 3) * 4")
    val test6 = parse("2 + (3 * 4)")
    val test7 = parse("2 + (3 * 4) - 5")
    val test8 = parse("(2 + 3) * (4 - 5)") 
        
    val test9 = parse("2 + 3 * 4")
    val test10 = parse("2 * 3 + 4")    
    val test11 = parse("2 + 3 * 4 * 2 + 5")
    val test12 = parse("2 - 3 * 5 + 20 + 4 / 2") 
      
    val test13 = parse("5 - 2 - 1 - 3")
    val test14 = parse("5 - 2 - 1 - 3 - 4")   
    val test15 = parse("5 - 2 - 16 / 2 / 2 / 2") 
    val test16 = parse("1 + 16 / 2 / 2 / 2")
    val test17 = parse("16 / 2 / 2 / 2 + 1") 
        
               
    assert(eval(test0) == 1234)  
          
    assert(eval(test1) == 5) 
    assert(eval(test2) == 6) 
    assert(eval(test3) == 3)
    assert(eval(test4) == 4)
    
    assert(eval(test5) == 20)
    assert(eval(test6) == 14)
    assert(eval(test7) == 9)    
    assert(eval(test8) == -5)
     
    assert(eval(test9) == 14)
    assert(eval(test10) == 10)    
    assert(eval(test11) == 31)
    assert(eval(test12) == 9) 
     
    assert(eval(test13) == -1)
    assert(eval(test14) == -5)    
    assert(eval(test15) == 1)
    assert(eval(test16) == 3)
    assert(eval(test17) == 3)         
  }//end
  
  
  "`parse`" should "be the left-inverse of `unparse`" in {
    val test0 = parse("1234")
             
    val test1 = parse("2 + 3")
    val test2 = parse("2 * 3")
    val test3 = parse("5 - 2")
    val test4 = parse("16 / 4")
    val test5 = parse("2 == 2")
    
    val test6 = parse("2 + 3 * 4")
    val test7 = parse("2 * 3 + 4")    
    val test8 = parse("2 + 3 * 4 * 2 + 5")
    val test9 = parse("2 - 3 * 5 + 20 + 4 / 2")
    
    val test10 = parse("5 - 2 - 1 - 3")
    val test11 = parse("5 - 2 - 1 - 3 - 4")   
    val test12 = parse("5 - 2 - 16 / 2 / 2 / 2") 
    val test13 = parse("1 + 16 / 2 / 2 / 2")
    val test14 = parse("16 / 2 / 2 / 2 + 1") 
    
    val test15 = parse("25 * 8 == 500 / 2 - 50")
    val test16 = parse("if 1 == 1 then 2 else 3")    
    val test17 = parse("if 2 + 2 == 5 then 1900 + 84 else 5 * 403")
    val test18 = parse("if 2 + 2 == 5 then if 1 == 2 then 0 else 1 else 5 * 403")
    
    assert(parse(unparse(test0)) == test0)
    
    assert(parse(unparse(test1)) == test1)
    assert(parse(unparse(test2)) == test2)
    assert(parse(unparse(test3)) == test3)
    assert(parse(unparse(test4)) == test4)
    assert(parse(unparse(test5)) == test5)
    
    assert(parse(unparse(test6)) == test6)
    assert(parse(unparse(test7)) == test7)
    assert(parse(unparse(test8)) == test8)
    assert(parse(unparse(test9)) == test9)
    
    assert(parse(unparse(test10)) == test10)
    assert(parse(unparse(test11)) == test11)
    assert(parse(unparse(test12)) == test12)
    assert(parse(unparse(test13)) == test13)
    assert(parse(unparse(test14)) == test14)
    
    assert(parse(unparse(test15)) == test15)
    assert(parse(unparse(test16)) == test16)
    assert(parse(unparse(test17)) == test17)
    assert(parse(unparse(test18)) == test18)
  }//end
  */
  












  /*
  "`enumerate`" should "enumerate all possible ways to print a syntax tree" in {
    val test0 = Leaf('num, "1234")
    val test1 = Branch('add, List(Leaf('num, "1"), Leaf('num, "1")))
    val test2 = Branch('comp, List(Branch('add, List(Leaf('num, "1"), Leaf('num, "1"))), Leaf('num, "2")))
    
    assert(enumerate(test0) == List(List((0, "1234"))))    
    assert(enumerate(test1) == List(
                                List((0, "1 + 1")),
                                List((0, "1 +"), (2, "1"))))
                                
    assert(enumerate(test2) == List(
                                  List((0, "1 + 1 == 2")), 
                                  List((0, "1 + 1 =="), (2, "2")),
                                  List((0, "1 +"), (2, "1 == 2")),
                                  List((0, "1 +"), (2, "1 =="), (2, "2"))))                                                                                                                           
  }
  
  "`findBestLayout`" should "find the layout that has as few lines as possible without causing lines to overflow" in {
    val test0 = List(List((0, "1 + 1")), List((0, "1 + "), (2, "1")))
    val test1 = List(List(
                          (0, "if 1 + 1 == 2 then "), 
                          (2, "if 2 + 2 == 5 then 1111 + 222 + 33 + 4 else 4444 * 333 * 22 * 1"),
                          (0, "else"),
                          (2, "if 1 == 2 then 2 + 2 else 4 * 5")),
                    List(
                          (0, "if 1 + 1 == 2 then "),
                          (2, "if 2 + 2 == 5 then "),
                          (4, "1111 + 222 + 33 + 4"),
                          (2, "else"),
                          (4, "4444 * 333 * 22 * 1"),
                          (0, "else"),
                          (2, "if 1 == 2 then 2 + 2 else 4 * 5")))
                          
    
    assert(findBestLayout(test0, 5) == List((0, "1 + 1")))
    assert(findBestLayout(test0, 4) == List((0, "1 + "), (2, "1")))
    
    assert(findBestLayout(test1, 80) == List(
                                            (0, "if 1 + 1 == 2 then "), 
                                            (2, "if 2 + 2 == 5 then 1111 + 222 + 33 + 4 else 4444 * 333 * 22 * 1"),
                                            (0, "else"),
                                            (2, "if 1 == 2 then 2 + 2 else 4 * 5")))
                                            
    assert(findBestLayout(test1, 40) == List(
                                            (0, "if 1 + 1 == 2 then "),
                                            (2, "if 2 + 2 == 5 then "),
                                            (4, "1111 + 222 + 33 + 4"),
                                            (2, "else"),
                                            (4, "4444 * 333 * 22 * 1"),
                                            (0, "else"),
                                            (2, "if 1 == 2 then 2 + 2 else 4 * 5")))
  }
  
  "`render`" should "print a given layout" in {
    val test0 = List((0, "hello"), (2, "world"))   
    assert(render(test0) == 
"""
hello
  world
""")
  }
  
  "`pretty`" should "make the code pretty" in {
    val test = parse("if 1 + 1 == 2 then if 2 + 2 == 5 then 1111 + 222 + 33 + 4 else 4444 * 333 * 22 * 1 else if 1 == 2 then 2 + 2 else 4 * 5")
    
    assert(pretty(test, 80) == 
"""
if 1 + 1 == 2 then
  if 2 + 2 == 5 then 1111 + 222 + 33 + 4 else 4444 * 333 * 22 * 1
else
  if 1 == 2 then 2 + 2 else 4 * 5
""")

    assert(pretty(test, 40) == 
"""
if 1 + 1 == 2 then
  if 2 + 2 == 5 then
    1111 + 222 + 33 + 4
  else
    4444 * 333 * 22 * 1
else
  if 1 == 2 then 2 + 2 else 4 * 5
""")     

assert(pretty(test, 1) == 
"""
if 1 + 1 ==
  2 then
  if 2 +
    2 ==
    5 then
    1111 +
      222 +
      33 +
      4
  else
    4444 *
      333 *
      22 *
      1
else
  if 1 ==
    2 then
    2 + 2
  else
    4 * 5
""")     
  }
  */
}//eof



