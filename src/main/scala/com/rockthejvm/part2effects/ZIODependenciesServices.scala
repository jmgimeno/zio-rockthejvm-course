package com.rockthejvm.part2effects

import ZIODependencies.{Connection, User}

import zio.*

// We have separated the services because the provide in ZIODependencies
// is a macro that in Scala3 causes problems if the services are defined
// in the same file

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
  val live: ZLayer[EmailService & UserDatabase, Nothing, UserSubscription] =
    ZLayer.fromFunction(create)

class EmailService:
  def email(user: User): Task[Unit] =
    ZIO
      .succeed(
        println(s"You've been subscribe to Rock the JVM. Welcome, ${user.name}")
      )

object EmailService:
  def create(): EmailService     = new EmailService
  val live: ULayer[EmailService] = ZLayer.succeed(create())

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
  val live: ZLayer[ConnectionPool, Nothing, UserDatabase] = ZLayer.fromFunction(create)

class ConnectionPool(nConnections: Int):
  def get: Task[Connection] =
    ZIO.succeed(println("Acquired connection")) *> ZIO.succeed(new Connection())

object ConnectionPool:
  def create(nConnections: Int): ConnectionPool =
    new ConnectionPool(nConnections)
  def live(nConnections: Int): ULayer[ConnectionPool] = ZLayer.succeed(create(nConnections))
