/** Exercise 3.3: A more sophisticated interpreter of grammars
  *
  * In 3.2, you implemented the method `simplifyAE` so that
  * the result of parsing the grammar `ae` is easier to use.
  * We will now make the simplicification work for all grammars.
  *
  * Tasks:
  *
  * 1. Design a data structure for grammars and implement
  *    a grammar interpreter so that users need not write
  *    a simplifying method like `simplifyAE` for every grammar
  *    they define. Instead, the parser of the grammar should
  *    always produce simplified syntax trees.
  *
  * 2. Create a grammar object. You may choose to either write
  *    something equivalent to `NaiveGrammar.ae` in 3.2, or
  *    describe arithmetic expressions with arbitrary spacing
  *    between words (ex. 2.2).
  *
  * 3. Test that your grammar interpreter works as expected.
  *
  *
  * ===================== SPOILER BEGINS =====================
  * You may want to have grammar objects contain information
  * about how to simplify their syntax trees.
  *
  * 1. Instead of `Terminal`, keywords could have their own case
  *    class (say, `Comment`), and the grammar interpreter could
  *    discard all syntax tree nodes created from them.
  *
  * 2. Instead of `Choice`, exp could be defined in terms of a
  *    new case class (say, `Select`). The grammar interpreter
  *    never creates new syntax tree nodes from `Select`.
  * ====================== SPOILER ENDS ======================
  */

object SimpleGrammar extends util.Combinators {
  sealed trait Tree
  case class Leaf(symbol: Symbol, code: String) extends Tree
  case class Branch(symbol: Symbol, children: List[Tree]) extends Tree


  /** Parsing the grammar of your choice.
    * Always produce simplified syntax trees.
    * Should not be hard-coded for arithmetic expressions.
    */

  def parseAE(code: String): Tree = parseGrammar(ae)(code)




  case class Nonterminal(symbol: Symbol) extends RuleRHS
  case class Number(parse: Parser[Tree]) extends RuleRHS
  case class Comment(parse: Parser[Tree]) extends RuleRHS
  case class Sequence(lhs: RuleRHS, rhs: RuleRHS) extends RuleRHS
  case class Select(lhs: RuleRHS, rhs: RuleRHS) extends RuleRHS


  sealed trait RuleRHS {
    def | (rhs: RuleRHS) = Select(this, rhs)
    def ~ (rhs: RuleRHS) = Sequence(this, rhs)
  }
  
  
  val exp       = Nonterminal('exp)
  val add       = Nonterminal('add)
  val mul       = Nonterminal('mul)

  val num       = Number(digitsParser('num))
  val sumOf     = Comment(keywordParser("sum of "))
  val productOf = Comment(keywordParser("product of "))
  val and       = Comment(keywordParser(" and "))
 
  def digitsParser(symbol: Symbol): Parser[Tree] =
    parseRegex("[0-9]+") ^^ { x => Leaf(symbol, x) }

  def keywordParser(keyword: String): Parser[Tree] =
    parseString(keyword) ^^ { x => Leaf('keyword, keyword) }
    
  
  
   
  val ae: Grammar =
    Grammar(
      start = exp,
      rules = Map(
        exp -> (add | mul | num),
        add -> (sumOf ~ exp ~ and ~ exp),
        mul -> (productOf ~ exp ~ and ~ exp)
      )
    )
  
    
  case class Grammar(start: Nonterminal, rules: Map[Nonterminal, RuleRHS]) {
    def lookup(nonterminal: Nonterminal): RuleRHS = rules(nonterminal)
  }
  
  
  def parseGrammar(grammar: Grammar): String => Tree = input => {
    parseNonterminal(exp, grammar)(input) match {
      	case Some((tree, rest)) => tree
        case None => sys.error("not a tree: " + input)
      }
  }

    
  def parseNonterminal(nonterminal: Nonterminal, grammar: Grammar): Parser[Tree] =
  	grammar.lookup(nonterminal) match {
  		case Select(firstpart, secondpart) => parseRHS(grammar.lookup(nonterminal), grammar) ^^ {
      																				children => {
      																				  println(children)
      																				  //Branch('Hello, children)
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
        child => Nil
      } 
             		
    	case Sequence(firstPart, secondPart) => parseRHS(firstPart, grammar) ~ parseRHS(secondPart, grammar) ^^ {
   			parsedSeq => parsedSeq._1 ::: parsedSeq._2
   		}  	 		
    	
    	case Select(firstTry, secondTry) => parseRHS(firstTry, grammar) | parseRHS(secondTry, grammar)
    	   	 		
   }
   
   
}//end SimpleGrammar object
