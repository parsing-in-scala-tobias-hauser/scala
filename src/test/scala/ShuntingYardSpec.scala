import org.scalatest._
import ShuntingYard._
import scala.collection.mutable.Queue

class ShuntingYardSpec extends FlatSpec {  

  "eval" should "evaluate a given RPN" in {     
    val test0 = parse("1234")
             
    val test1 = parse("2 + 3")
    val test2 = parse("2 * 3")
    val test3 = parse("5 - 2")
    val test4 = parse("16 / 4")
    
    val test5 = parse("( 2 + 3 ) * 4")
    val test6 = parse("2 + ( 3 * 4 )")
    val test7 = parse("2 + ( 3 * 4 ) - 5")
    val test8 = parse("( 2 + 3 ) * ( 4 - 5 )") 
        
    val test9 = parse("2 + 3 * 4")
    val test10 = parse("2 * 3 + 4")    
    val test11 = parse("2 + 3 * 4 * 2 + 5")
    val test12 = parse("2 - 3 * 5 + 20 + 4 / 2") 
      
    val test13 = parse("5 - 2 - 1 - 3")
    val test14 = parse("5 - 2 - 1 - 3 - 4")   
    val test15 = parse("5 - 2 - 16 / 2 / 2 / 2") 
    val test16 = parse("1 + 16 / 2 / 2 / 2")
    val test17 = parse("16 / 2 / 2 / 2 + 1") 

    val test18 = parse("2 ^ 3 + 20")
    val test19 = parse("20 + 2 ^ 3")
    val test20 = parse("2 ^ 3 ^ 2")

    val test21 = parse("if 2 == 3 then 1 else 0")
    val test22 = parse("if 1 + 2 == 4 - 1 then 2 - 1 else 3 - 3")
    val test23 = parse("if if 0 then 4 else 0 then 2 + 3 else 3 - 4")
    val test24 = parse("if 1 == 1 then if 3 == 0 then 1 else 0 else 14")
        
               
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

    assert(eval(test18) == 28)
    assert(eval(test19) == 28)
    assert(eval(test20) == 512)   

    assert(eval(test21) == 0)
    assert(eval(test22) == 1)
    assert(eval(test23) == -1)
    assert(eval(test24) == 0)
  }//end
  
}//eof



