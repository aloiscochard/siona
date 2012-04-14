package siona.demo.petstore
package core

import scalaz._
import Scalaz._

import siona._
//import siona.data._  will replace separated sub-module import

import siona.data.mapping._
import siona.data.model._
import siona.data.repository._

// - Refactor querying support (per impl configuration)
// - Better matcher support with automatic add SizeConstrain
// - Schema support
// - Use shapeless HList instead of TupleX

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

  object Category extends Document with Mapper[Category] {
    val name = new Field[String]("name") with Mapped[String] {
      val lens = Lens(_.name, e => n => e.copy(name = n))

      val validation = Validation(name => {
        if (name.isEmpty) "Name can't be empty".fail
        if (name.length > 20) "Name can't be longer than 20 character".fail
        if (name.length < 5) "Name can't be smaller than 5 character".fail
        name.success
      })
    }

    // TODO Add Optional Field support

    def apply(name: String): Category = Category(
      Key(UUID.random),
      name
    )

    def in(implicit in: In) = Category(key, name)
    def out = name :: Nil
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

  ////////////////////////
  // Repository support //
  ////////////////////////
  import model.Category._
  import store.Categories._

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

  set(c.key, name)("a")
  set(c.key)(key, name)(UUID.random, "b")

  update(c.key, name)(_ + "*")
  update(c.key)(key, name) { (k, n) => (UUID.random, n + "-" + k) }

  // Monadic Operations
  entity.get(c.key).flatMap { x => entity.set(c.key, x.copy(name = x.name + "*")) }
  for { x <- entity.get(c.key) } yield entity.set(c.key, x.copy(name = x.name + "*"))


  get(c.key, name).flatMap { x => set(c.key, name)(x + "*") }
  for { x <- get(c.key, name) } yield set(c.key, name)(x + "*")

  // Query DSL
  entity.query(name === "food")
}
