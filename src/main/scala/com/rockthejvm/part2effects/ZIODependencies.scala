package com.rockthejvm.part2effects

import zio.ZIOAppDefault

import zio.*
import javax.sql.ConnectionPoolDataSource

object ZIODependencies extends ZIOAppDefault:

  // app to subscribe users to newsletter
  case class User(name: String, email: String)

  class UserSubscription(
      emailService: EmailService,
      userDatabase: UserDatabase
  ):
    def subscribeUser(user: User): Task[Unit] =
      for
        _ <- emailService.email(user)
        _ <- userDatabase.insert(user)
      yield ()

  object UserSubscription:
    def create(emailService: EmailService, userDatabase: UserDatabase) =
      new UserSubscription(emailService, userDatabase)

  class EmailService:
    def email(user: User): Task[Unit] =
      ZIO
        .succeed(
          println(s"You've been subscribe to Rock the JVM. Welcome, ${user.name}")
        )

  object EmailService:
    def create(): EmailService = new EmailService

  class UserDatabase(connectionPool: ConnectionPool):
    def insert(user: User): Task[Unit] =
      for
        conn <- connectionPool.get
        _ <- conn.runQuery(
          s"insert into subscribers(name, email) values (${user.name}, ${user.email}) "
        )
      yield ()

  object UserDatabase:
    def create(connectionPool: ConnectionPool): UserDatabase =
      new UserDatabase(connectionPool)

  class ConnectionPool(nConnections: Int):
    def get: Task[Connection] =
      ZIO.succeed(println("Acquired connection")) *> ZIO.succeed(new Connection())

  object ConnectionPool:
    def create(nConnections: Int): ConnectionPool =
      new ConnectionPool(nConnections)

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
