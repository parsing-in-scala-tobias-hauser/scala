/*
	Final project

	- Dijkstra's shunting-yard algorithm -


	Tasks:
	- Implement Dijkstra's shunting-yard algorithm to support operators of any precedence and associativity. 
	- Extend the algorithm for operators with 3 or more operands (e. g., if-then-else).
	- Transform a RPN (Reverse Polish notation) expression into an AST (abstract syntax tree).
*/



/*
	Table of contents:
	0.) Introduction
	1.) Structures used in the program
	2.) Tokenizing an input string
	3.) Dijkstra's shunting-yard algorithm
	4.) Parsing infix and transform it to postfix
	5.) Evaluating arithmetic expressions in Reverse Polish notation
	6.) Transform a RPN (Reverse Polish notation) expression into an AST (abstract syntax tree)
*/



/*
	0.) Introduction:
*/



/*
	The way we write arithmetic expressions is infix notation.
	Operators have associativity and precedence, brackets override precedence.

	Many computer programs (scripting languages, spreadsheets, compilers and calculators)
	require the parsing and calculation of arithmetic expressions in infix notation.
	One very good way to do this is to convert the expression from infix notation to some 
	intermediate format. We will be using a very common and simple format called Reverse Polish
	notation. In Reverse Polish notation we write the operators after the operands, that
	is why we can call it also postfix notation.

	Dijkstra's shunting-yard algorithm converts an expression in infix notation to an
	expression in Reverse Polish notation (RPN), so the computer can calculate the result.

	Examples: 
	- shuntingYard(1 + 2) = 1, 2, +
	- shuntingYard(2 - 4 - 2) = 2, 4, -, 2, -
	- shuntingYard(9 + 24 / ( 7 - 3 )) = 9, 24, 7, 3, -, /, +

	As we can see, the shunting-yard algorithm keeps associativity, operator precedence, and
	bracket override. To evaluate the RPN, we read the output left to right, push the
	operands onto a stack and each time an operator comes up, we perform the operation
	on the stack. The last remaining value on the stack is the result:

	eval(1, 2, +) = 3
	eval(2, 4, -, 2, -) = -2, 2, - = -4
	eval(9, 24, 7, 3, -, /, +) = 9, 24, 4, /, + = 9, 6, + = 15

	Terms:
	To implement the shunting-yard algorithm, it is a good idea to
	define following basic terms:

	- token: number, operator, bracket, or other elements of an expression
	- operator: +, -, *, /, or any other function
	- n-ary operator: operators with three or more operands, e.g. if-then-else
	- stack: LIFO data structure, used to temporarily store things, 
					 e.g. operators on an operator stack
	- queue: opposite to a stack, FIFO data structure, used to store an RPN expression,
					 e.g. the output of the shunting-yard algorithm or the input of a RPN evaluator.

	List of references: 
	- http://en.wikipedia.org/wiki/Shunting-yard_algorithm
	- https://www.youtube.com/watch?v=QzVVjboyb0s
*/



import scala.collection.mutable.Queue
import scala.math.pow

object ShuntingYard {	
	type ArrayOfTokens = Array[Token]
	type ListOfTokens = List[Token]



	/*
	1.) Structures used in the program:
	*/



	/*
	An arithmetic expression consists of tokens. A token could be a number, a binary operator, a bracket
	or other elements of the expression. 9 + 24 / ( 7 - 3 ) has four number tokens, three binary operator tokens
	and two bracket tokens.

	Number tokens: only contain the integer value, nothing else needed.
	Operator tokens: represent binary operators and contain the operator as a string, e.g. "+", or "*".
	Bracket tokens: contain the bracket as a string, e.g. "(" and ")".
	N-Ary operator tokens: 	
		- more complex tokens for n-ary operators like "if-then-else".
		- contain the keyword, e.g. "if", "then" or "else".
		- contain the position, e.g. "if" would be on position 0, and "then" on position 1.
		- the token also tells us whether it is the last position or not, e.g. "else" is the last keyword of the "if-then-else" expression.
	*/
	sealed trait Token 
		case class Num(number: Int) extends Token
		case class Operator(operator: String) extends Token			
		case class Bracket(bracket: String) extends Token	
		case class NAryOperator(keyword: String, position: Int, last: Boolean) extends Token		

	/*
	A binary operator can be left-associative or right-associative. It also has a precedence.
	For example, the operator "+" and "*" are both left-associative. And "+" has less precedence
	than "*". 

	Let's introduce two case classes for left- and right-associativity, each containing
	it's precedence represented as an integer value.
	*/
	sealed trait Associativity	
		case class Left(precedence: Int) extends Associativity
		case class Right(precedence: Int) extends Associativity		

	//All supported n-ary operators:
	val nAryList = List("if")	

	//All supported binary operators:
	val operatorList = List("+", "-", "*", "/", "^", "==")

	//Precedence and associativity rules for all binary operators:
	val orderOfOperands = Map(				
		"==" -> Right(0),			
		"+" -> Left(2),
		"-" -> Left(2),
		"*" -> Left(3),
		"/" -> Left(3),
		"^" -> Right(4)
	)
	


	/*
	2.) Tokenizing an input string:
	*/
		


	/*
	Given a string, we want to know whether it's a number or not.
	*/	
	def isAllDigits(input: String): Boolean = {
		input forall Character.isDigit 
	}
 
	/*
	Given a string, we want to create a token.
	If the string is a number, bracket or operator, we want to return a suitable token, e.g.
	for "42", return Num(42),
	for "(", return Bracket("("), and
	for "+", return Operator("+").

	If there is an operator we don't know, e.g. "IAmAStrangeNewOperator" let's print a suitable error message and exit the program.

	Should return the created token on success.
	*/ 
	def createToken(input: String): Token = {		
  	isAllDigits(input) match {
  		case true => Num(input.toInt)
  		case false => input match {  			
  			case "(" => Bracket("(")
  			case ")" => Bracket(")") 
  			case "if" => NAryOperator("if", 0, false)
  			case "then" => NAryOperator("then", 1, false)
  			case "else" => NAryOperator("else", 2, true) 	  				
  			case _ => {
  				if(operatorList.contains(input)) 
  					Operator(input) 
  				else {
  					println("The operator " + input + " is not supported.")
  					sys.exit()
  				}  					
  			}
  		}
  	}
  }

  /*
	Given an input string, we create an array of strings by separating the input string by spaces.
	With this array of strings, we want to create an array of tokens, by calling the `createToken` function on
	each string of the array. 

	If there is a number following a number, or an operator following an operator, let's print a 
	suitable error message and exit the program.

	Should return an array of tokens on success.
  */
  def tokenize(input: String): ArrayOfTokens = {
  	var stringArray = input.split(" ").map(_.trim)
  	var tokenArray: ArrayOfTokens = new Array[Token] (stringArray.length) 

  	tokenArray(0) = createToken(stringArray(0))

  	for(i <- 1 until stringArray.length) { 
  		tokenArray(i) = tokenArray(i-1) match {
  			case Num(number1) => createToken(stringArray(i)) match {
  				case Num(number2) => {
  					println("Cannot parse a number following a number (" + number1 + " " + number2 + ")")  					
  					sys.exit()  						
  				}
  				case _ => createToken(stringArray(i))
  			}
  			case Operator(operator1) => createToken(stringArray(i)) match {
  				case Operator(operator2) => {
  					println("Cannot parse an operator following an operator (" + operator1 + " " + operator2 + ")")  					
  					sys.exit()  						
  				}
  				case _ => createToken(stringArray(i))
  			}
  			case _ => createToken(stringArray(i))
  		}  		
  	}
  	tokenArray
  }
    


  /*
  3.) Dijkstra's shunting-yard algorithm:
	*/



  /*
  Given two operator strings, we want to test whether
  - the first operand is left-associative and its precedence is less than or equal to that of the second operand, or
  - the first operand is right-associative, and has precedence less than that of the second operand.

  Should return true or false on successs.
  */ 
  def checkPrecedence(first: String, second: String): Boolean = {
  	orderOfOperands(first) match {
  		case Left(precedence1) => orderOfOperands(second) match {
  			case Left(precedence2) => if(precedence1 <= precedence2) true else false
  			case Right(precedence2) => if(precedence1 <= precedence2) true else false  			
  		}
  		case Right(precedence1) => orderOfOperands(second) match {
  			case Left(precedence2) => if(precedence1 < precedence2) true else false
  			case Right(precedence2) => if(precedence1 < precedence2) true else false  			
  		}  		
  	}
  }

  /*
	Given a `token, we want to return it's name.

	Prints an error message on failure.

	Should return a string on success.
  */
  def getString(token: Token): String = 
  	token match {
  		case Operator(operator) => operator
  		case Bracket(bracket) => bracket
  		case NAryOperator(keyword, position, last) => keyword  
  		case _ => {
  			println("getString() did not work.")   										
  			sys.exit()
  		}		
  	}

  /*
  In order to transform expressions written in infix notation to postfix notation, we can use the shunting-yard algorithm.
  The shuntig-yard algorithm transforms infix to postfix.
	The following data structures are needed:
	- an array of tokens is the input
	- a stack for the operators
	- a stack for the positions of n-ary operators
	- a boolean for n-ary operators that tells me whether a n-ary operator was closed before or not
	- a queue of tokens for the output

	- while there are tokens to read:
		- read a token
		- if the token is a number, add it to the queue		
    - if the token is an operator, o1, then:
      - while there is an operator token, o2, at the top of the stack, and either
        		o1 is left-associative and its precedence is less than or equal to that of o2, or
        		o1 is right associative, and has precedence less than that of o2,
      		pop o2 off the operator stack, onto the output queue
      - push o1 onto the operator stack
		- if it's a left bracket "(", push it onto the stack
		- if it's a right bracket ")":
			- while there's not a left bracket at the top of the stack:
					pop operators from the stack onto the output queue
			- pop the left bracket from the stack but discard it!
		- if it's the start of a n-ary operator (e.g. "if"):
			- push it onto the stack
			- push it's position (0) to the position stack
		- if it's not the start of a n-ary operator e.g. "then, else":
			- check with the help of the position stack, whether the operator is at a valid position
				- if the position is valid:
		 			- pop all operators from the stack until the start of the n-ary operator is at the top of the stack again
		 			- if it is the last operator keyword:
						- clear the position stack (remove n elements for n-ary operator)
						- set closed to true
					- if it is not the last operator keyword:
						- add the position to the position stack
						- if a n-ary operator was closed before:
							- pop the n-ary operator from the stack and set closed to false 
		 		if the position is not valid:
		 		- return error message
	- if the position stack is empty:
		- while there are operators on the stack, pop them to the queue
	- else, if the position stack is not empty:
		- return error message


	Given an array of tokens, we want to use the shunting-yard algorithm to create
	an output queue of tokens in RPN order.

	Prints an error message on failure.
	
	Should return a queue of tokens.
  */
  def shuntingYard(tokenArray: ArrayOfTokens): Queue[Token] = {
  	//the result will be stored in the output queue
  	var output = new Queue[Token] 

  	//for the shunting-yard we need an operator stack
  	var stack = List[Token] () 

  	//we need to store the keyword positions for n-ary operators on a position stack
  	var positionStack = List[Int] () 	

  	//is set to true if a n-ary keyword is following the last keyword of another n-ary operator, 
  	//e.g. ... else 4 then ... 
  	var closed = false 

  	for(i <- 0 until tokenArray.length) {
  		tokenArray(i) match {
  			case Num(number) => output.enqueue(Num(number))
  			case Operator(operator) => {
  				while(stack.isEmpty == false && operatorList.contains(getString(stack.head)) && checkPrecedence(operator, getString(stack.head))) {
  					output.enqueue(stack.head)
  					stack = stack.drop(1)
  				}
  				stack = Operator(operator) +: stack  				
  			}
  			case Bracket(bracket) => bracket match {
  				case "(" => stack = Bracket(bracket) +: stack
  				case ")" => {
  					while(getString(stack.head) != "(") {
  						output.enqueue(stack.head)
  						stack = stack.drop(1)
  					}
  					stack = stack.drop(1) //discard "("
  				}
  			}
  			case NAryOperator(keyword, position, last) => position match {  				  				
  				case 0 => {
  					stack = NAryOperator(keyword, 0, false) +: stack
  					positionStack = 0 +: positionStack
  				}
  				case _ => {
  					if(positionStack.isEmpty == false && position == positionStack.head + 1) { 

  						while(stack.isEmpty == false && nAryList.contains(getString(stack.head)) == false) {  						
  							output.enqueue(stack.head)
  							stack = stack.drop(1) 
  						} 
  						if(last) {
  							positionStack = positionStack.drop(position) //clear the position stack if it is the last keyword
  							closed = true 
  						}
  						else {
  							positionStack = position +: positionStack //add to the position stack if it is not the last keyword
  							if(closed) {  								
  								output.enqueue(stack.head)	//pop the n-ary operator from the stack and set closed to false
  								stack = stack.drop(1)
  								closed = false
  							}
  						}					 							  						  						  							   						
  					}
  					else {
  						println("An operator with 3 or more operands (e.g., if-then-else) could not be parsed.")   										
  						sys.exit() 
  					}
  				}
  			}  			
  		}
  	}
  	if(positionStack.isEmpty) { //all n-ary operators should be closed, and the rest of the operator stack is added to the output queue
  		output ++= stack 
  	}
  	else {
  		println("An operator with 3 or more operands (e. g., if-then-else) could not be parsed.")   										
  		sys.exit()
  	} 	
  } 



  /*
  4.) Parsing infix and transform it to postfix:
	*/



	/*
	Given an infix arithmetic expression string, we want to transform it to RPN (Reverse Polish notation).
	We tokenize the string, and then call the shunting-yard algorithm on it.
	
	Should return a queue containing the tokens in RPN order on success.
	*/
	def parse(input: String): Queue[Token] = {
		shuntingYard(tokenize(input))
	}



	/*
	5.) Evaluating arithmetic expressions in Reverse Polish notation:
	*/



	/*
	Given a token that should be Num(number), number is returned.

	Prints an error message on failure.
	
	Should return an integer.
  */
  def getInt(token: Token): Int = {
  	token match {
  		case Num(number) => number
  		case _ => {
  			println("getInt() did not work.")   										
  			sys.exit()
  		}
  	}
  }

  /*
	In order to evaluate an expression in postfix notation, we need 
	a stack for the number-tokens and a function which evaluates all operations 
	on the stack.

  The following algorithm evaluates Reverse Polish notation (= postfix notation):
  while there are input tokens left:  
	- read the next token from input.
	- if the token is a number, we push it to the stack.
	- if the token is an operator, we pop the required operands from
		the stack, perform the operation and push the result back to the stack.
	- we are finished when there are no more tokens to read.
	- the final number on the stack is the result.


	Given a queue of tokens in RPN order, we want to evaluate it's value.

	Prints an error message on failure.

	Should return the result as an integer value.
  */
  def eval(input: Queue[Token]): Int = {
  	//we need a stack for the number-tokens, so we can do operations on the stack
  	var stack = List[Token] ()

  	while(input.isEmpty == false) {
  		input.dequeue match {
  			case Num(number) => stack = Num(number) +: stack 
  			case Operator(operator) => operator match {
  				case "+" => {
  					var secondOperand = getInt(stack.head)
  					stack = stack.drop(1)
  					var firstOperand = getInt(stack.head)
  					stack = stack.drop(1)
  					stack = Num(firstOperand + secondOperand) +: stack
  				}
  				case "-" => {
  					var secondOperand = getInt(stack.head)
  					stack = stack.drop(1)
  					var firstOperand = getInt(stack.head)
  					stack = stack.drop(1)
  					stack = Num(firstOperand - secondOperand) +: stack
  				}
  				case "*" => {
  					var secondOperand = getInt(stack.head)
  					stack = stack.drop(1)
  					var firstOperand = getInt(stack.head)
  					stack = stack.drop(1)
  					stack = Num(firstOperand * secondOperand) +: stack
  				}
  				case "/" => {
  					var secondOperand = getInt(stack.head)
  					stack = stack.drop(1)
  					var firstOperand = getInt(stack.head)
  					stack = stack.drop(1)
  					stack = Num(firstOperand / secondOperand) +: stack
  				}
          case "^" => {
            var secondOperand = getInt(stack.head)
            stack = stack.drop(1)
            var firstOperand = getInt(stack.head)
            stack = stack.drop(1)
            stack = Num(pow(firstOperand , secondOperand).toInt) +: stack
          }
          case "==" => {
            var secondOperand = getInt(stack.head)
            stack = stack.drop(1)
            var firstOperand = getInt(stack.head)
            stack = stack.drop(1)
            if(firstOperand == secondOperand)            
            	stack = Num(1) +: stack
            else
            	stack = Num(0) +: stack
          }            
  			}
  			case NAryOperator(keyword, position, last) => keyword match {
  				case "if" => {
          	var thirdOperand = getInt(stack.head)
            stack = stack.drop(1)
          	var secondOperand = getInt(stack.head)
            stack = stack.drop(1)
            var firstOperand = getInt(stack.head)
            stack = stack.drop(1)
            if (firstOperand != 0) {            	
            	stack = Num(secondOperand) +: stack
            }
            else {            	
            	stack = Num(thirdOperand) +: stack
            }
          }
  			}
  			case _ => {
  				println("Something could not be evaluated.")   										
  				sys.exit()
  			}
  		}
  	}
  	getInt(stack.head)
  }
  


  /*
  6.) Transform a RPN (Reverse Polish notation) expression into an AST (abstract syntax tree):
  */


  /*
	We can easily transform the output of the shunting-yard algorithm into an abstract syntax tree.
	Let's create a simplified version of the tree structure in SimpleGrammar.scala:
	The leafs contain an integer value, and the branches contain the operator as a string and
	the children as a list of trees.
  */
  sealed trait Tree
  case class Leaf(number: Int) extends Tree
  case class Branch(operator: String, children: List[Tree]) extends Tree 

  //a stack of trees is needed to save the children for the next operators
  var stackOfTrees = List[Tree] ()

  /*
	Building a tree from a RPN expression is similar to evaluating it.
	If we encounter a number, we build the leaf and push it onto the stackOfTrees.
	If we encounter an operator, we build a branch and use the correspondent items on the stackOfTrees
	as it's children. Then we push the branch onto the stackOfTrees, so it can be used as a child
	for the next operation. If there are no elements to read, the remaining element on the stackOfTrees
	is our tree.

	Prints error message on failure.

	Should return a tree.
  */
  def createTree(input: Queue[Token]): Tree = input.dequeue match {
  	case Num(number) => {
  		stackOfTrees = Leaf(number) +: stackOfTrees   		
  		if(input.isEmpty == false)
  			createTree(input)
  		else
  			stackOfTrees.head
  	}  
  	case Operator(operator) => {
  		var second = stackOfTrees.head
  		stackOfTrees = stackOfTrees.drop(1)
  		var first = stackOfTrees.head
  		stackOfTrees = stackOfTrees.drop(1)
  		stackOfTrees = Branch(operator, List(first, second)) +: stackOfTrees
  		if(input.isEmpty == false)
  			createTree(input)
  		else
  			stackOfTrees.head		
  	} 
  	case NAryOperator(keyword, position, last) => {
  		var third = stackOfTrees.head
  		stackOfTrees = stackOfTrees.drop(1)
  		var second = stackOfTrees.head
  		stackOfTrees = stackOfTrees.drop(1)
  		var first = stackOfTrees.head
  		stackOfTrees = stackOfTrees.drop(1)
  		stackOfTrees = Branch(keyword, List(first, second, third)) +: stackOfTrees
  		if(input.isEmpty == false)
  			createTree(input)
  		else
  			stackOfTrees.head		
  	}   
  	case _ => {
  		print("Something went wrong.")
  		sys.exit()
  	}	
  }

  def helper(input: String): Tree = createTree(parse(input))



}//end






