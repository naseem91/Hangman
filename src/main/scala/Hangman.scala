import scalaz.zio._
import scalaz.zio.console._
import java.io.IOException

object Hangman extends App {

  //Read all words in the file to a list lazily
  lazy val words: List[String] = scala.io.Source.fromFile("words.txt").getLines.toList

  //Each player has a state changed during the game it contains guessing characters
  //that user entered and both the correct word and it's name .
  //In this game number of tries is 10
  case class State(name: String, guesses: Set[Char], word: String) {
    //Case of wining the game the function compares between the set of guesses and
    // the correct word each time
    def playerWon: Boolean = word.toSet.diff(guesses).isEmpty

    // Case of losing the game it compares the guesses with the word also with the
    // condition not acceding 10 times
    def playerLost: Boolean = guesses.diff(word.toSet).size >= 10
  }

  def run(args: List[String]): IO[Nothing, ExitStatus] = {
    hangman.attempt.map(_.fold(_ => 1, _ => 0)).map(ExitStatus.ExitNow(_))
  }

  val hangman: IO[IOException, Unit] = for {
    _ <- putStrLn("Welcome to Hangman Game!! please enter your name")
    name <- getStrLn
    _ <- putStrLn("Hello " + name + ",Let's begin the game ")
    word <- findWord()
    //    _ <- putStrLn(word.toString)
    state = State(name, Set(), word)
    _ <-showWord(state)
    _ <- gameLoop(state)

  } yield ()

  //Chooses one word to be guessed from internal file randomly
  def findWord(): IO[IOException, String] = for {
    rnd <- IO.sync(scala.util.Random.nextInt(words.length))
  } yield (words.lift(rnd).getOrElse("error"))

  //Updates the state of the player each time he enters a character
  def showWord(state: State) = {
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
  def gameLoop(state: State): IO[IOException, State] = for {
    guess <- chooseChar()
    state <- IO.point(state.copy(guesses = state.guesses + guess))
    _ <- showWord(state)
    loop <- if (state.playerWon) putStrLn(s"Congrats ${state.name} you WON").const(false)
    else if (state.playerLost) putStrLn(s"Hard luck :( you lost \n The word was ${state.word}").const(false)
    else if (state.word.contains(guess)) putStrLn("Go on you guessed correctly").const(true)
    else putStrLn("you are wrong ,keep trying you have a chance").const(true)
    _ <- if (loop) gameLoop(state) else IO.point(state)
  } yield state
}
