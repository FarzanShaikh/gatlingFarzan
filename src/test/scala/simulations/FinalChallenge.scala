package simulations

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import sun.security.util.Length
import scala.concurrent.duration._
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.Random

class FinalChallenge extends Simulation{

  val httpConf = http.baseUrl("http://localhost:8080/app/")
    .header("Accept", "application/json")
    .proxy(Proxy("localhost",8888))

  def userCount: Int = getProperty("USERS", "5").toInt
  def rampDuration: Int = getProperty("RAMP_DURATION", "10").toInt
  def testDuration: Int = getProperty("DURATION", "60").toInt

  val idNumber = (21 to 2000).iterator
  val rnd = new Random()
  val now = LocalDate.now()
  val pattern = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  private def getProperty (propertyName: String, defaultValue: String) ={
    Option(System.getenv(propertyName))
      .orElse(Option(System.getProperty(propertyName)))
      .getOrElse(defaultValue)
  }

  def randomString (length: Int) ={
    rnd.alphanumeric.filter(_.isLetter).take(length).mkString
  }

  def getRandonDate(startedData: LocalDate,random: Random): String ={
    startedData.minusDays(random.nextInt(30)).format(pattern)
  }

  def randomStrig (length: Int) ={
    rnd.alphanumeric.filter(_.isLetter).take(length).mkString
  }

  def randomDate (random: Random, staredtDate: LocalDate) ={
    staredtDate.minusDays(random.nextInt(30)).format(pattern)
  }

  val customFeeder = Iterator.continually(Map(
    "gameId" -> idNumber.next(),
    "name" -> ("Game-" + randomString(5)),
    "releaseDate"-> getRandonDate(now, rnd),
    "reviewScore" -> rnd.nextInt(100),
    "category" -> ("Category-" + randomString(6)),
    "rating" -> ("Rating-" + randomString(4))
  ))

  before {
    println(s"Running test with ${userCount} users")
    println(s"Ramping users over ${rampDuration} seconds")
    println(s"Total test duration: ${testDuration} seconds")
  }

  def getAllGames() ={
    exec(http("getAllGames")
      .get("videogames")
      .check(status.is(200)))
      .pause(5)
  }

  def creatGame()={
    feed(customFeeder)
      .exec(http("createNewGame")
        .post("videogames")
        .body(ElFileBody("JsonData/RequestBody.json")).asJson
        //.check(jsonPath("$.name")is("name"))
        .check(status.is(200)))
      .pause(1)
  }

  def getSpecificGames() ={
    exec(http("getSpecificGames")
      .get("videogames/${gameId}")
      .check(status.is(200)))
      .pause(5)
  }

  def deleteSpecificGame() = {
    exec(http("deleteSpecificGame")
    .delete("videogames/${gameId}")
    .check(status.is(200)))
      .pause(2)
  }

  val scn = scenario("Final Challenge")
    .forever(){
      exec(getAllGames())
        .exec(creatGame())
        .exec(getSpecificGames())
        .exec(deleteSpecificGame())
    }


  setUp(
    scn.inject(
      nothingFor(5.seconds),
      rampUsers(userCount) during (rampDuration.second)
    )
  ).protocols(httpConf)
    .maxDuration(testDuration.seconds)

  after{
    println("============TEST OVER============")
  }


}
