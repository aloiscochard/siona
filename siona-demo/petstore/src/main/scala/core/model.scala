package siona.demo.petstore
package core

import scalaz._
import Scalaz._

import siona._
//import siona.data._  will replace separated sub-module import

import siona.data.mapping._
import siona.data.model._
import siona.data.repository._
import siona.data.io._

// - Refactor querying support (per impl configuration)
// - Better matcher support with automatic add SizeConstrain
// - Schema support

package object model {

  case class Category(
    key: Category#Key,
    name: String
  ) extends Entity[UUID, Category]

  case class Item(
    key: Item#Key,
    name: String,
    category: Category#Key
  ) extends Entity[UUID, Item]

  object Category extends MappedDocument[Category] {
    val key = KeyField[UUID](keyLens)
    val name = MappedField[String]("name", nameValidator, nameLens)

    val nameValidator: Validator[String] = Validator(name => {
      if (name.isEmpty) "Name can't be empty".fail
      if (name.length > 20) "Name can't be longer than 20 character".fail
      if (name.length < 5) "Name can't be smaller than 5 character".fail
      name.success
    })

    val keyLens: Lens[UUID] = Lens(e => k => e.copy(key = Key(k)), _.key)
    val nameLens: Lens[String] = Lens(e => (n => e.copy(name = n)), _.name)

    // TODO Add Optional Field support

    def apply(name: String): Category = Category(
      Key(UUID.random),
      name
    )

    def in(implicit in: Input) = Category(key, name)
    def out(implicit out: Output) = /* key ::*/ name :: Nil
  }



  object Item {
    def apply(name: String, category: Category#Key): Item = Item(
      Key(java.util.UUID.randomUUID),
      name,
      category
    )
  }
}

package object store {
  object Categories extends Entities[model.Category.type, model.Category, UUID, model.Category](model.Category)
}

object test {
  val c = model.Category("food")
  val i = model.Item("HappyCat", c.key)

  // Type-safe equality on entity ID
  c === c

  import model.Category._
  import store.Categories._

  ///////////////////
  // Serialization //
  ///////////////////

  def serialization = {
    import siona.data.io.jackson._

    //unmarshall
    //c.marshall

    //unmarshall[JSON]
    println(new String(c.marshall[JSON].toBytes.toArray))
  }

  ////////////////////////
  // Repository support //
  ////////////////////////

  def repository = {
    // Per Entities (Mapped Only)
    entity.get(c.key)
    entity.get(c) // implicit convertion Entity => Entity#Key

    entity.set(c.key, c)
    entity.update(c.key, x => x.copy(name = x.name + "*"))
    entity.update(c, name)(_ + "*") // == update(c.key)(_)(_) => Then apply lens and return c
    entity.update(c)(name -> name) { (a, b) => (a, b) }

    // Per Entity (Mapped Only)
    c.save // == entity.set(c.key)(c)
    c.update(name)(_ + "*") // == entity.update(c)(_)(_)

    // Per Column
    name.get(c.key) // == get(c.key)(name)
    name.set(c.key, "yo") // == get(c.key)(name)
    name.update(c.key, _ + "*") // == update(c.key)(name)(_)

    // Per Document
    get(c.key, name)
    get(c.key)(key, name)

    set(c.key, name, "a")
    set(c.key)(key, name)(UUID.random, "b")

    update(c.key, name)(_ + "*")
    update(c.key)(key, name).to((a, b) => (a, b + "*"))

    // Monadic Operations
    entity.get(c.key).flatMap { x => entity.set(c.key, x.copy(name = x.name + "*")) }
    for { x <- entity.get(c.key) } yield entity.set(c.key, x.copy(name = x.name + "*"))


    get(c.key, name).flatMap { x => set(c.key, name, x + "*") }
    for { x <- get(c.key, name) } yield set(c.key, name, x + "*")

    // Query DSL
    entity.query(name === "food")
  }
}
