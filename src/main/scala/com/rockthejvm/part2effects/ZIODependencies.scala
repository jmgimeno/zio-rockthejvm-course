package com.rockthejvm.part2effects

import zio.ZIOAppDefault

import zio.*
import java.util.concurrent.TimeUnit

object ZIODependencies extends ZIOAppDefault:

  // app to subscribe users to newsletter
  case class User(name: String, email: String)

  case class Connection():
    def runQuery(query: String): Task[Unit] =
      ZIO.succeed(println(s"Executing query: $query"))

  // val subscriptionService = ZIO.succeed( // Dependency injection
  //   new UserSubscription(
  //     new EmailService,
  //     new UserDatabase(
  //       new ConnectionPool(10)
  //     )
  //   )
  // )

  val subscriptionService = ZIO.succeed( // Dependency injection
    UserSubscription.create(
      EmailService.create(),
      UserDatabase.create(
        ConnectionPool.create(10)
      )
    )
  )

  /*
    This *clean DI* has drawbacks:
      - does not scale to many services
      - DI can be 100x worse (nightmare for finding the dependencies) => needs discipline
        - pass dependencies partially
        - not having all deps in the same place
        - passing dependencies multiple times can cause problems
   */

  def subscribe(user: User): ZIO[Any, Throwable, Unit] =
    for
      sub <- subscriptionService // service instantiated at the point of call
      //                          => every subscription instantiates a service
      _ <- sub.subscribeUser(user)
    yield ()

  // risk leaking resources if you subscribe multiple users in the same program
  val program =
    for
      _ <- subscribe(User("Daniel", "daniel@rockthejvm.com"))
      _ <- subscribe(User("Bon Jovi", "jon@rockthejvm.com"))
    yield ()

  // alternative
  def subscribe_v2(user: User): ZIO[UserSubscription, Throwable, Unit] =
    for
      sub <- ZIO.service[UserSubscription] // ZIO[UserSubscription, Nothing, UserSubscription]
      _   <- sub.subscribeUser(user)
    yield ()

  val program_v2 =
    for
      _ <- subscribe_v2(User("Daniel", "daniel@rockthejvm.com"))
      _ <- subscribe_v2(User("Bon Jovi", "jon@rockthejvm.com"))
    yield ()

  /*
    - we don't need to care about dependencies until the end of the world
    - all ZIOs requiring the dependency will use the same instance
    - can use different instances of the same type for different needs (e.g. testing)
    - layers can be created and composed much like regular ZIOs with a rich API
   */

  /*
  def run = program_v2.provideLayer( // In Scala 3 use provideLayer (or else does not compile)
    ZLayer.succeed(
      UserSubscription.create(
        EmailService.create(),
        UserDatabase.create(
          ConnectionPool.create(10)
        )
      )
    )
  )
   */

  /*
   * ZLayers
   */

  // A layer with no dependency
  val connectionPoolLayer: ZLayer[Any, Nothing, ConnectionPool] =
    ZLayer.succeed(ConnectionPool.create(10))

  // A layer with a dependency
  val databaseLayer: ZLayer[ConnectionPool, Nothing, UserDatabase] =
    ZLayer.fromFunction(UserDatabase.create)

  val emailServiceLayer            = ZLayer.succeed(EmailService.create())
  val userSubscriptionServiceLayer = ZLayer.fromFunction(UserSubscription.create)

  // composing layers
  // vertical composition (>>>)
  val databaseLayerFull: ZLayer[Any, Nothing, UserDatabase] = connectionPoolLayer >>> databaseLayer

  // horizontal composition (++)
  val subscriptionRequirementsLayer: ZLayer[Any, Nothing, UserDatabase & EmailService] =
    databaseLayerFull ++ emailServiceLayer

  // mix & match
  val userSubscriptionLayer: ZLayer[Any, Nothing, UserSubscription] =
    subscriptionRequirementsLayer >>> userSubscriptionServiceLayer

  // def run = program_v2.provideLayer(userSubscriptionLayer)

  // best practice: write "factory" methods exposing layers in the companion objects of the services
  // (these are the live vals we have above)
  // we can do the same as above with the newly defined lives

  // magic
  def runnableProgram_v2 = program_v2.provide(
    UserSubscription.live,
    EmailService.live,
    UserDatabase.live,
    ConnectionPool.live(10),
    // ZIO will tell you if you're missing a layer
    // or if you have multiple layers of the same type
    // or tell you the dependency graph !!
    // ZLayer.Debug.tree,
    ZLayer.Debug.mermaid
  )

  // magic v2
  val userSubscriptionLayer_v2: ULayer[UserSubscription] =
    ZLayer.make[UserSubscription](
      UserSubscription.live,
      EmailService.live,
      UserDatabase.live,
      ConnectionPool.live(10)
    )

  // passthrough
  val dbWithPoolLayer: ZLayer[ConnectionPool, Nothing, ConnectionPool & UserDatabase] =
    UserDatabase.live.passthrough

  // service = take a dependency and expose it as a value to further layers
  val dbService: ZLayer[UserDatabase, Nothing, UserDatabase] = ZLayer.service[UserDatabase]

  // launch = a ZIO with same requirements which starts UserSubscription and enver finishes
  // user when your whole application is a layer (e.g. a server)
  val subscriptionLaunch: ZIO[EmailService & UserDatabase, Nothing, Nothing] =
    UserSubscription.live.launch

  // memoization
  // - by default all layers are memoized
  // - use fresh if you do not want to

  /*
    Already provided services by ZIOAppDefault: Clock, Random, System, Console
   */
  val getTime       = Clock.currentTime(TimeUnit.SECONDS)
  val randomValue   = Random.nextInt
  val sysVariable   = System.env("JAVA_HOME")
  val printlnEffect = Console.printLine("Hello world")

  def run = runnableProgram_v2
