import scalaz.zio._
import scalaz.zio.console._
import java.io.IOException

//Reimplement hangman game using state monad
object HangmanV2 extends App {

  import scalaz.State, State._

  lazy val words: List[String] = scala.io.Source.fromFile("words.txt").getLines.toList

  case class Hangman(name: String, guesses: Set[Char], word: String) {

    def playerWon: Boolean = word.toSet.diff(guesses).isEmpty

    def playerLost: Boolean = guesses.diff(word.toSet).size >= 10
  }

  //Create state monad
  type ->[A,B] = (A,B)
  type  HangmanState[A]= State[Hangman,A]

  def run(args: List[String]): IO[Nothing, ExitStatus] = {
    hangman.attempt.map(_.fold(_ => 1, _ => 0)).map(ExitStatus.ExitNow(_))
  }

  val hangman: IO[IOException, Unit] = for {
    _ <- putStrLn("Welcome to Hangman Game!! please enter your name")
    name <- getStrLn
    _ <- putStrLn("Hello " + name + ",Let's begin the game ")
    word <- findWord()
    _ <-putStrLn(word.toString)
    state = Hangman(name, Set(), word)
    _ <-showWord(state)
    _ <- gameLoop(state)
  }yield ()


  //Chooses one word to be guessed from internal file randomly
  def findWord(): IO[IOException, String] = for {
    rnd <- IO.sync(scala.util.Random.nextInt(words.length))
  } yield (words.lift(rnd).getOrElse("error"))

  //Updates the state of the player each time he enters a character
  def showWord(state: Hangman) = {
    val tempWord = state.word.toList.map(char => if (state.guesses.contains(char)) char else ("-"))
    putStrLn(tempWord.mkString(" ").toString())
  }

  //Checks the character that the player entered
  def chooseChar(): IO[IOException, Char] = for {
    _ <- putStrLn("Enter a character please !")
    choice <- getStrLn
    char <- choice.toLowerCase.trim.headOption match {
      case Some(c) => IO.point(c)
      case None => putStrLn("you did not enter a character") *> chooseChar()
    }
  } yield (char)

  //Play the game until it finished
  def gameLoop(state: Hangman): IO[IOException,Hangman] = for {
    guess <- chooseChar()
    newState <-IO.point(changeState(guess))
    tempState = newState.eval(state)
    _ <- showWord(newState.eval(tempState))
    loop <- if (tempState.playerWon) putStrLn(s"Congrats ${tempState.name} you WON").const(false)
    else if (tempState.playerLost) putStrLn(s"Hard luck :( you lost \n The word was ${tempState.word}").const(false)
    else if (tempState.word.contains(guess)) putStrLn("Go on you guessed correctly").const(true)
    else putStrLn("you are wrong ,keep trying you have a chance").const(true)
    _ <- if (loop) gameLoop(tempState) else IO.point(tempState)
  } yield tempState

  //Change the state for the game {change the set of characters represents the guesses }
  def changeState(guess:Char)=
    for {
      a <- init[Hangman]
      _ <- modify((s: Hangman) => s.copy(guesses = s.guesses + guess))
        newState <- get
    } yield newState
}
