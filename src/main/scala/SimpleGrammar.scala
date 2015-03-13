/*
	Exercise 4: Left recursion
	
	
	Answers to Questions 5, 7 and 8:
	
	5.) 
	Question: What do you think is wrong with the simple grammar of arithmetic expressions with infix operators? How to fix it?
	Answer: 
	The grammar is left-recursive, fix it by changing it to right-recursive.
			
     left-recursive:
        exp	-> (num | add | mul),
        add -> (exp ~ plus ~ exp),       
        mul -> (exp ~ times ~ exp)  
      
      Problem: exp is calling exp again in add and mul.
      
      right-recursive:
        exp	-> (num | add | mul),
        add -> (num ~ plus ~ exp),       
        mul -> (num ~ times ~ exp)  
        
      Solution: exp is calling first num and plus (times) Parser, and after this exp again.
      
	7.) 
	Question: What syntax tree does it produce? How is the result of your parser different?
	Answer: 
	It is the same syntax tree, if you use treeString.
	There is no difference, see the test in SimpleGrammarSpec.scala.
	
	8.) 
	Question:	What does "2 + 3 * 4" evaluate to? What about "2 * 3 + 4"?
	Answer: 
	eval("2 + 3 * 4") evaluates to 14, which is correct.
	eval("2 * 3 + 4") evaluates to 14, which is not correct, it should evaluate to 10.
	
	Question: Is the result what you expect? If not, why?
	The result is not always correct.
	The grammar does not know that there is a ranking of operators, for example '*' should be evaluated before '+'.
	
	Solution:
	Tell the grammar what to do first and what to do second.
*/




/*
  Exercise 5: Operator precedence
  
  Question:  How did you implement operator precedence?
  Answer:
  
  The following Grammar does the work:
  
  exp	-> (term | add),
  add -> (term ~ plus ~ exp), 
  term-> (rest | mul),      
  mul -> (rest ~ times ~ term),
  rest -> (num | left ~ exp ~ right) 
  
  Multiplication is done before addition. To realize that, the grammar puts every multiplication in a 'package', and adds
  those 'packages'.
  
  First, the grammar checks whether the expression is an addition or a term.
  This is repeated recursively until all additions are found. (That's why 'add' calls 'exp' again as second operand)
  
  Second, when all additions are found, the grammar checks each term whether it is a multiplication or something else.
  Since we have already checked all additions recursively, we do not need to call 'exp' or 'add' again.

  Third, when all additions and multiplications are found, we check the remaining rest wether it is a number or an expression in
  brackets. When it is a number, we just parse the number. When it is a bracket term, we parse the expression inside the brackets
  starting again with 'exp'.


*/


import sext._


object SimpleGrammar extends util.Combinators {
  sealed trait Tree
  case class Leaf(symbol: Symbol, code: String) extends Tree
  case class Branch(symbol: Symbol, children: List[Tree]) extends Tree  

  def parse(code: String): Tree = makeLeftAssociative (parseGrammar(myGrammar)(code))  

  case class Nonterminal(symbol: Symbol) extends RuleRHS
  case class Number(parse: Parser[Tree]) extends RuleRHS
  case class Comment(parse: Parser[Tree]) extends RuleRHS
  case class Sequence(lhs: RuleRHS, rhs: RuleRHS) extends RuleRHS
  case class Select(lhs: RuleRHS, rhs: RuleRHS) extends RuleRHS

  sealed trait RuleRHS {
    def | (rhs: RuleRHS) = Select(this, rhs)
    def ~ (rhs: RuleRHS) = Sequence(this, rhs)
  }
  
  val aexp      = Nonterminal('aexp)
  val cond      = Nonterminal('cond)
  val exp       = Nonterminal('exp)
  val rest      = Nonterminal('rest)
  val start     = Nonterminal('start)
  val term      = Nonterminal('term)
   
  val add       = Nonterminal('add)
  val mul       = Nonterminal('mul)
  val sub       = Nonterminal('sub)
  val div       = Nonterminal('div)
  val comp      = Nonterminal('comp)

  val num       = Number(digitsParser('num)) 
  val plus      = Comment(keywordParser(" + "))
  val times     = Comment(keywordParser(" * ")) 
  val minus     = Comment(keywordParser(" - "))
  val slash     = Comment(keywordParser(" / ")) 
  val left      = Comment(keywordParser("(")) 
  val right     = Comment(keywordParser(")"))
  val equals    = Comment(keywordParser(" == ")) 
  val ifif      = Comment(keywordParser("if "))
  val thenthen  = Comment(keywordParser(" then "))
  val elseelse  = Comment(keywordParser(" else "))
 
  def digitsParser(symbol: Symbol): Parser[Tree] =
    parseRegex("[0-9]+") ^^ { x => Leaf(symbol, x) }

  def keywordParser(keyword: String): Parser[Tree] =
    parseString(keyword) ^^ { x => Leaf('keyword, keyword) }  
       
  val myGrammar: Grammar =
    Grammar(
      start = start,
      rules = Map(
        start -> (exp | cond),
        cond -> (ifif ~ start ~ thenthen ~ start ~ elseelse ~ start),        
        exp -> (aexp | comp),        
        comp -> (aexp ~ equals ~ start),
      	aexp	-> (term | add | sub),
        add -> (term ~ plus ~ aexp),
        sub -> (term ~ minus ~ aexp),        
        term-> (rest | mul | div),      
        mul -> (rest ~ times ~ term),
        div -> (rest ~ slash ~ term),
        rest -> (num | left ~ exp ~ right)     
      )
    )
    
    
  case class Grammar(start: Nonterminal, rules: Map[Nonterminal, RuleRHS]) {
    def lookup(nonterminal: Nonterminal): RuleRHS = rules(nonterminal)
  }
    
  def parseGrammar(grammar: Grammar): String => Tree = input => {
    parseNonterminal(grammar.start, grammar)(input) match {
      	case Some((tree, rest)) => tree
        case None => sys.error("not a tree: " + input)
      }
  }
    
  def parseNonterminal(nonterminal: Nonterminal, grammar: Grammar): Parser[Tree] =
  	grammar.lookup(nonterminal) match {
  		case Select(firstpart, secondpart) => parseRHS(grammar.lookup(nonterminal), grammar) ^^ {
      																				children => {      																				     																				  
      																				  children.head    																				  
      																				}
    																				}  	
  		case _ =>	parseRHS(grammar.lookup(nonterminal), grammar) ^^ {
      						children => Branch(nonterminal.symbol, children)
    						}
  }



  def parseRHS(ruleRHS: RuleRHS, grammar: Grammar): Parser[List[Tree]] =
    ruleRHS match {
    	case exp@Nonterminal(symbol) => parseNonterminal(exp, grammar) ^^ {
    		child => List(child)}
    	case Number(parse) => parse ^^ {
        child => List(child)
      } 
      case Comment(parse) => parse ^^ {                
        child => {
          //println(child)
          Nil        
        }
      }             		
    	case Sequence(firstPart, secondPart) => parseRHS(firstPart, grammar) ~ parseRHS(secondPart, grammar) ^^ {
   			parsedSeq => parsedSeq._1 ::: parsedSeq._2
   		}  	 		   	
    	case Select(firstTry, secondTry) => parseRHS(firstTry, grammar) | parseRHS(secondTry, grammar)
    	   	 		
   }
   
   
   
   
   
   
   
   
   
   
   /*   
   *  Exercise 7: Associativity
   */
   
   
   /*
    * Problem:
    * How to use right recursion and still keep left associativity?
    * "5 - 2 - 1" => "(5 - 2) - 1"
    * sub    = num ('-' num) *
    *
    * Now:    Branch('sub, List(Leaf('num, "5"), Branch('sub, List(Leaf('num, "2"), Leaf('num, "1")))))
    *
    * Better: Branch('sub, List(Branch('sub, List(Leaf('num, "5"), Leaf('num, "2"))), Leaf('num, "1")))
    *
    * Solution: left-rotate the tree
    *
    */
   
   /*
   makeLeftAssociative transforms a right-associative tree into a left-associative tree by 
   rotating the tree left wherever it is necessary. It does not rotate when it is not a good idea to rotate.
   For example to keep the operator precedence, it only rotates the parts of the tree that can be rotated without
   violating the rules.
   */
    def makeLeftAssociative(tree: Tree): Tree = 
    {
   		var copiedTree = rotateLeft(List(tree))
   		copiedTree match {
   			case List(tree) => tree
   		}  		
    }    
    
    /*
    rotateLeft decides whether a Tree is a Leaf, a Branch with two children or a Branch with three children.
    Arithmetic expressions and integer comparison have two chlildren.
    If-then-else expressions have three children.
    */
    def rotateLeft(tree: List[Tree]): List[Tree] = tree match {
      case List(Leaf(terminal, string)) => tree 
      case List(Branch(nt0, List(leftChild, rightChild))) => rotateTree(tree)
      case List(Branch(nt0, List(child1, child2, child3))) => 
                      List(Branch(nt0, List(makeLeftAssociative(child1), makeLeftAssociative(child2), makeLeftAssociative(child3))))             
    }
    
    /*
    rotateTree left-rotates a tree with two children.
    If the children are both Leafs, the tree is returned unchanged:
    Branch:
    - 'add              - 'add
    - List:             - List:
    | - Leaf:           | - Leaf:
    | | - 'num    =>    | | - 'num 
    | | - 2             | | - 2
    | - Leaf:           | - Leaf:
    | | - 'num          | | - 'num
    | | - 2             | | - 2
    
    If the left child is a Branch and the right child is a Leaf, only the left child is rotated left.
    
    Branch:                           Branch:
    - 'add                            - 'add
    - List:                           - List:
    | - Branch:                       | - Branch:
    | | - 'div                        | | - 'div
    | | - List:                       | | - List:
    | | | - Leaf:                     | | | - Branch:
    | | | | - 'num                    | | | | - 'div
    | | | | - 16                      | | | | - List:
    | | | - Branch:         =>        | | | | | - Leaf:
    | | | | - 'div                    | | | | | | - 'num
    | | | | - List:                   | | | | | | - 16
    | | | | | - Leaf:                 | | | | | - Leaf:
    | | | | | | - 'num                | | | | | | - 'num
    | | | | | | - 2                   | | | | | | - 2
    | | | | | - Leaf:                 | | | - Leaf:
    | | | | | | - 'num                | | | | - 'num
    | | | | | | - 2                   | | | | - 2
    | - Leaf:                         | - Leaf:
    | | - 'num                        | | - 'num
    | | - 1                           | | - 1

    
    If the left child is a Leaf and the right child is a Branch or both children are Branches,
    it is checked whether the operator of the root and the operator of the right child are both
    '+' and/or '-' or both '*' and/or '/' to keep the operator precedence.
    
    If the precedence is correct, the tree is rotated left.
    
    Branch:                       Branch:
    - 'sub                        - 'sub
    - List:                       - List:
    | - Leaf:                     | - Branch:
    | | - 'num                    | | - 'sub
    | | - 5                       | | - List:
    | - Branch:         =>        | | | - Leaf:
    | | - 'sub                    | | | | - 'num
    | | - List:                   | | | | - 5
    | | | - Leaf:                 | | | - Leaf:
    | | | | - 'num                | | | | - 'num
    | | | | - 2                   | | | | - 2
    | | | - Leaf:                 | - Leaf:
    | | | | - 'num                | | - 'num
    | | | | - 1                   | | - 1

    
    If the precedence is not correct, only the right child is rotated left.
    
    Branch:                           Branch:
    - 'add                            - 'add
    - List:                           - List:
    | - Leaf:                         | - Leaf:
    | | - 'num                        | | - 'num
    | | - 1                           | | - 1
    | - Branch:                       | - Branch:
    | | - 'div                        | | - 'div
    | | - List:                       | | - List:
    | | | - Leaf:                     | | | - Branch:
    | | | | - 'num          =>        | | | | - 'div
    | | | | - 16                      | | | | - List:
    | | | - Branch:                   | | | | | - Leaf:
    | | | | - 'div                    | | | | | | - 'num
    | | | | - List:                   | | | | | | - 16
    | | | | | - Leaf:                 | | | | | - Leaf:
    | | | | | | - 'num                | | | | | | - 'num
    | | | | | | - 2                   | | | | | | - 2
    | | | | | - Leaf:                 | | | - Leaf:
    | | | | | | - 'num                | | | | - 'num
    | | | | | | - 2                   | | | | - 2
    */    
    def rotateTree(tree: List[Tree]): List[Tree] = tree match {        
      case List(Branch(nonterminal, children)) => children match {
        case List(Leaf(lt, ls), Leaf(rt, rs)) => tree 
        case List(Branch(nt, c), Leaf(t, s)) => List(Branch(nonterminal, List(makeLeftAssociative(Branch(nt, c)), Leaf(t, s))))
        case List(leftChild, rightChild) => {
          if (sameOperatorType(tree))
            rotateTree(rotate(tree))
          else
            List(Branch(nonterminal, List(leftChild, makeLeftAssociative(rightChild))))
        }
      }           
    }
           
  /*
  rotate left-rotates the root and the right child.
  The other functions make sure that the tree's children are either 
  Leaf, Branch or 
  Branch, Branch.  
  */          
   def rotate(tree: List[Tree]): List[Tree] = tree match {       
    case List(Branch(nt0, c0)) => c0 match {               
      case List(Leaf(t1, s1), Branch(nt1, c1)) => c1 match {          
        case List(leftChild, rightChild) => List(Branch(nt1, List(Branch(nt0, List(Leaf(t1, s1), leftChild)), rightChild)))          
      }   
      case List(Branch(lnt1, lc1), Branch(rnt1, rc1)) => rc1 match {
        case List(leftChild, rightChild) => List(Branch(rnt1, List(Branch(nt0, List(Branch(lnt1, lc1), leftChild)), rightChild)))
       }        
     }     
    }
          
    /*
    sameOperatorType checks whether the Operator of the root and the Operator of the right child are both
    '+' and/or '-' or both '*' and/or '/'.
    */          
    def sameOperatorType(tree: List[Tree]): Boolean =  tree match {       
      case List(Branch(nt0, c0)) => c0 match {               
        case List(Leaf(t1, s1), Branch(nt1, c1)) => (compareOperatorType(nt0, nt1)) match {          
          case true => true
          case false => false
        }   
        case List(Branch(lnt1, lc1), Branch(rnt1, rc1)) => (compareOperatorType(nt0, rnt1)) match {
          case true => true
          case false => false 
        }       
      }     
    }
    
    /*
    compareOperatorType checks whether two operators are both '+' and/or '-' or both '*' and/or '/'.
    */
    def compareOperatorType(first: Symbol, second: Symbol): Boolean = {
      val addSub = List('add, 'sub)
      val mulDiv = List('mul, 'div)      
      if  (addSub.contains(first) && addSub.contains(second))
        true
      else if (mulDiv.contains(first) && mulDiv.contains(second))
        true
      else
        false
    }
     
   
  def eval(t: Tree): Int = t match {

    case Branch('sub, List(lhs, rhs)) =>
      eval(lhs) - eval(rhs)

    case Branch('div, List(lhs, rhs)) =>
      eval(lhs) / eval(rhs)

    case Branch('add, List(lhs, rhs)) =>
      eval(lhs) + eval(rhs)

    case Branch('mul, List(lhs, rhs)) =>
      eval(lhs) * eval(rhs)

    case Leaf('num, code) =>
      code.toInt
     
  }
  
  
  
  
  
  /*   
   *  Exercise 8: Pretty printing
   */
   
   
  def unparse(tree: Tree): String = tree match {
    case Leaf(t,s) => s
    case Branch(nonterminal, List(Leaf(t1, s1), Leaf(t2, s2))) => s1 + nonterminalToString(nonterminal) + s2
    case Branch(nonterminal, List(leftChild, rightChild)) => unparse(leftChild) + nonterminalToString(nonterminal) + unparse(rightChild)
    case Branch(nonterminal, List(child1, child2, child3)) => "if " + unparse(child1) + " then " + unparse(child2) + " else " + unparse(child3)
   }
   
  def nonterminalToString(nonterminal: Symbol): String = nonterminal match {
    case 'add => " + "
    case 'sub => " - "
    case 'mul => " * "
    case 'div => " / "
    case 'comp => " == "
   }
   
   def nonterminalToString2(nonterminal: Symbol): String = nonterminal match {
    case 'add => " +"
    case 'sub => " -"
    case 'mul => " *"
    case 'div => " /"
    case 'comp => " =="
   }
   
  def unparse2(tree: Tree, nonterminal: Symbol): String = unparse(tree) + nonterminalToString2(nonterminal)
   
   
  def pretty(tree: Tree, lineWidth: Int): String = render(findBestLayout(enumerate(tree), lineWidth))
   
   
  // intermediate representation of code to be printed
  type Layout = List[(Int, String)]
  
  // all possible layouts of a syntax tree
  type Doc = List[Layout]

//https://scalatutorial.wordpress.com/tag/for-comprehension/
//http://nerd.kelseyinnis.com/blog/2013/11/12/idiomatic-scala-the-for-comprehension/
  
  // step 1: enumerate all possible ways to print a syntax tree
  def enumerate(tree: Tree): Doc = tree match {
    case Leaf(t, s) => List(List((0, s)))
     
    case Branch(nonterminal, List(Leaf(t1, s1), Leaf(t2, s2))) =>
      val horizontal: Layout = List((0, unparse(tree)))
      val vertical: Layout = List((0, unparse2(Leaf(t1, s1), nonterminal)), (2, unparse(Leaf(t2, s2))))  
      for {      
        treeLayout <- List(horizontal, vertical)      
      } yield {                                         
          treeLayout                
      }
    
    case Branch(nonterminal, List(leftChild, rightChild)) => 
      val leftDoc = enumerate(leftChild)
      val rightDoc = enumerate(rightChild)
      for {              
        leftLayout <-  leftDoc //??? : Doc //alle möglichkeiten linkes kind        
        rightLayout <- rightDoc //??? : Doc //alle möglichkeiten rechtes kind
        break <- List(false, true)
      } yield {        
          //??? : Layout //der ganze baum, mit allen möglichkeiten linkes kind , rechtes kind                           
          //??? : Layout 
          //leftLayout + nonterminal + rightLayout  und 
          //               leftLayout + nonterminal + 
          //                 rightLayout                        
          makeLayout(leftLayout, rightLayout, nonterminal, break)
      }
      
      case Branch(nonterminal, List(child1, child2, child3)) => 
      val firstDoc = enumerate(child1)
      val secondDoc = enumerate(child2)
      val thirdDoc = enumerate(child3)
      for {              
        firstLayout <-  firstDoc       
        secondLayout <- secondDoc
        thirdLayout <- thirdDoc 
        break <- List(false, true)
      } yield {                                    
          makeLayout2(firstLayout, secondLayout, thirdLayout, break, tree)
      }
    }
  
  
  
  
  def makeLayout (leftLayout: Layout, rightLayout: Layout, nonterminal: Symbol, break: Boolean): Layout = {
    if (break) {
      verticalize(leftLayout, nonterminal, addIndent(rightLayout))
      //Idee:
      //wenn addIndent weiß, welcher Operator oben steht, kann man vielleicht die Einrückung besser spezialisieren
      //der indendation level von leftLayout könnte man auch noch übergeben
      //verticalize(leftLayout, nonterminal, addIndent(nonterminal, rightLayout))
    }
    else {      
      horizontalize(leftLayout, nonterminal, addIndent(rightLayout))      
    }
  }

/*  
List(
  List((0, "1 + 1 == 2")), 
  List((0, "1 + 1 =="), (2, "2")),
  List((0, "1 +"), (2, "1 == 2")),
  List((0, "1 +"), (2, "1 =="), (2/*4*/, "2"))))
 */
 

 
  def verticalize(leftLayout: Layout, nonterminal: Symbol, rightLayout: Layout): Layout = {     
    var newLayout: Layout = leftLayout
    leftLayout.last match {
      case (indent, string) => 
        newLayout = newLayout.dropRight(1) //remove last element
        newLayout = newLayout :+ (indent, string + nonterminalToString2(nonterminal))
        newLayout ::: rightLayout      
    }    
  }
  
  def horizontalize(leftLayout: Layout, nonterminal: Symbol, rightLayout: Layout): Layout = {
    var newLayout1: Layout = leftLayout    
    var newLayout2: Layout = rightLayout   
    leftLayout.last match {
      case (indentLeft, stringLeft) => rightLayout.head match {
        case (indentRight, stringRight) => 
          newLayout1 = newLayout1.dropRight(1) //remove last element
          newLayout2 = newLayout2.drop(1) //remove first element
          newLayout1 = newLayout1 :+ (indentLeft, stringLeft + nonterminalToString(nonterminal) + stringRight)
          newLayout1 ::: newLayout2        
      }
    }   
  }
  
  
  
  def addIndent(layout: Layout): Layout = {
    var newLayout : Layout = List[(Int, String)]()  
    layout.foreach { 
      _ match {
        case (indent, string) => newLayout = newLayout :+ (indent+2, string)          
      }  
    }
    newLayout
  }

 
  
  def makeLayout2 (firstLayout: Layout, secondLayout: Layout, thirdLayout: Layout, break: Boolean, tree: Tree): Layout = {
    if (break) {
      verticalize2(firstLayout, secondLayout, thirdLayout)
    }
    else {      
      horizontalize2(tree)  
    }
  }
                                  
                                  
  def verticalize2 (firstLayout: Layout, secondLayout: Layout, thirdLayout: Layout) = {
    var newLayout : Layout = firstLayout    
    if (newLayout.size == 1) {
      firstLayout.head match {
      case (indent, string) => 
        newLayout = newLayout.drop(1)
        newLayout = List((indent, "if " + string + " then"))
      }
    }//end if
    else {
    firstLayout.head match {
      case (indent, string) => 
        newLayout = newLayout.drop(1)
        newLayout = (indent, "if " + string) :: newLayout        
      }
    firstLayout.last match {
      case (indent, string) =>         
        newLayout = newLayout.dropRight(1)          
        newLayout = newLayout :+ (indent, string + " then")       
    }  
    }//end else 
    newLayout = newLayout ::: addIndent(secondLayout)
    firstLayout.head match {
      case (indent, string) =>         
        newLayout = newLayout :+ (indent, "else")
      }
    newLayout ::: addIndent(thirdLayout)
  }
  
  def horizontalize2 (tree: Tree) = List((0, unparse(tree)))
                                  
                                  
                                  
  
  //step 2: find the best layout according to some line width
  //idea: 
  //1.) delete everything longer than lineWidth
  //2.) if all Layouts are longer than lineWidth, find the one that comes close to it
  //3.) find the layout with the least number of lines
  def findBestLayout(doc: Doc, lineWidth: Int): Layout = {
    var resTmp = List[Layout] ()
    //1.)
    doc.foreach {
      _ match {
        case x:List[_] => if( checkLineWidth(x, lineWidth) ) resTmp ::= x
      }           
    }    
    //2.)
    if (resTmp.isEmpty) {
     var min: Int = findWidestLineWidth(doc.head)
     resTmp = List(doc.head)
       doc.foreach {
         _ match {
           case x:List[_] => if (findWidestLineWidth(x) < min) {
             resTmp = List(x)
             min = findWidestLineWidth(x)
           }
         }           
       }     
    }
    //3.)
     var min = countLines(resTmp.head)
     var res = resTmp.head
     resTmp.foreach {
       _ match {
         case x:List[_] => if( countLines(x) < min) {
           min = countLines(x)
           res = x
          }
        }           
      }   
    res    
  }
   
  def checkLineWidth(layout: Layout, lineWidth: Int): Boolean = {
    var res = true
    layout.foreach {
      _ match {
        case (indent, string) => if ((indent + string.length) > lineWidth) res = false
      }
    }
    res
  }
  
  def countLines(layout: Layout): Int = {
    var res = 0
    layout.foreach {
      _ match {
        case (indent, string) => res += 1
      }
    }
    res
  }
  
  def findWidestLineWidth(layout: Layout): Int = {
    var res = 0
    layout.foreach {
      _ match {
        case (indent, string) => if ((indent + string.length) > res) res = (indent + string.length)
      }
    }
    res
  }
  
  //step 3: render the layout as a string
  //idea
  //convert layouts to strings.
  //convert List((0, "hello"), (2, "world")) to:
  //
  //hello
  //  world
  def render(layout: Layout): String = {
    var res: String = "\n" 
    layout.foreach {
      _ match {
        case (indent, string) => res += printWhiteSpace(indent) + string + "\n"
      }
    }
    res
  }
   
  def printWhiteSpace(indent: Int): String = {
    if (indent == 0) return ""
    var res: String = ""
    for(a <- 1 to indent) {
      res += " "
    }
    res
  }    
}//end SimpleGrammar object
