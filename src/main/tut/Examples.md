# Getting Started

With qeduce, database interactions have three steps:

 1. form an SQL query or statement
 2. transform results to a data structure or aggregate
 3. execute against a connection, or connection pool

Everything needed is is the following package:

```tut:silent
import qeduce.api._
```

There is also a `qeduce.cql.api` for the Cassandra database. However, hese examples use a postgresql database containing the gnucash accounting schema.   Here is the setup:

```tut:silent
Class.forName("org.postgresql.Driver")
import Credentials.{host, user, pass}
val url = s"jdbc:postgresql://$host/gnucash?user=$user&password=$pass"
```

_Step 1_: create some SQL. Note the use of a parameter.

```tut:book
val param = "BANK"
val query = sql"select * from accounts where account_type = $param"
```

_Step 2_: add a transformation for the results.

```tut:book
val action = query map {row => row[String]('name) }
```

Here row, of type Row, has an apply operator for selecting fields by their type and name.

>>Note: `Row` is a wrapper around java.sql.ResultSet which is mutable.  A `Row` is only valid within the map function.

The map operation produces something called an `Action` which runs the query on a database connection.

_Step 3_: execute. Given an URL or a `java.sql.DataSource` the action obtains a connection, runs the query, and cleans up.

```tut:book
val result = action runWithUrl url
```

More details about each step follow.

# Forming SQL Statements

Statements have the type `SQL` and are produced by `sql" ... "` strings. (See scala string interpolation.)

```tut:book
sql"select name from accounts where account_type = ${param}"
```

Each interpolated parameter becomes a parameter of a `java.sql.PreparedStatement` which prevents accidental SQL injection.

A typeclass, `QueryType[A]` specifies the conversion of scala types, `A`, to SQL types. Instances of `QueryType` are provided for the common types and more can added.

Statements can also be assembled from pieces using the `~` operator.

```tut:book
val head = sql"select name from accounts"
val pred = sql"where account_type = ${param}"
head ~ pred
```

Taking this a step further, qeduce provides `val select = sql"select"` and similar definitions for the most common SQL keywords.  Using these and the `~` operator we can write:

```tut:book
select ~ sql"name" ~ from ~ sql"accounts" ~ where ~ sql"account_type = ${param}"
```

There are also implicit subclasses of SQL for symbols and values which allow this to further simplified:

```tut:book
select ~ 'name ~ from ~ 'accounts ~ where ~ 'account_type ~ sql"=" ~ param
```

In short, `~` concatenates SQL values, symbols, and values for which there is an QueryType instance.

Finally, methods `list()` and `nest()` are provided to make it easier to form comma-separated lists. The `list()` form creates a bare list while the `nest()` form creates a list enclosed in brackets.

```tut:book
val query = select ~ list('name, 'commodity_scu) ~ from ~ 'accounts ~ where ~ 'account_type ~ in ~ nest("BANK", "EXPENSE")
```

## Note

This is not a true SQL embedded DSL. The aim is to allow statements to be composed dynamically while preventing accidental SQL injection. If the form of a statement is fixed, a simple sql" .... " string works well.

# Terms

Symbols such as `'name` stand for SQL identifiers in statement construction as in `select ~ 'name ~ from ~ 'accounts` and result transformation as in `row[String]('name)`.

If a symbol is frequently used, a term can be defined which captures its type and name:

```tut:book
val name = term[String]('name)
val scu = term[Int]('commodity_scu)
```

A term can be used anywhere in place of a symbol. In a mapping function this is shorter because the type can be omitted:

```tut:book
val txform = { row: Row => (row(name), row(scu)) }
```

The row can be implicit to reduce repetition:

```tut:book
val txform = { implicit row: Row => (name(), scu()) }
```

This is used as in:

```tut:book
query map txform runWithUrl url
```

## Matching

More complex cases can use matching or partial functions involving terms:

```tut:book
object & { def unapply[T](t: T) = Some((t, t)) }
val txform: Row => (String, Int) = { case name(n) & scu(s) => (n, s) }
```

# Result Transformation

## Rationale

The aim is to convert a series of `Row`s into an application-specific data structure. For example:

- A sequence of objects, one per row.  (This is the obvious case, illustrated in the previous examples.)
- A graph data structure.
- An updated copy of an existing data structure.
- A fixed-space aggregate, summary or digest.
- An effect such as a Task or Process.
- A search, which may produce an answer without traversing the entire query result.

In all but the first case there is no need to materialise the query result as a sequence. And it is often not important to define a class that models a result row.

Another aim is to keep connection management and result set iteration out of the transformation code which can remain purely functional.

All in all, a _fold_ is the appropriate concept here.  But this is a fold that can exit early, which is sometimes called a _reducer_. Reducers are typically combined with _transducers_.

## Transducers and Reducers

Qeduce relies on a separate transducer library.  To use transducers explicitly:

```tut:silent
import transducers.api._
```

The previous example is equivalent to the following:

```tut:book
val txduce = map { implicit row: Row => (name(), scu()) }
val toResult = txduce(toVector)
query reduce toResult runWithUrl url
```

### Graph Example

The following example uses a simplified graph data structure, defined as:

```tut:silent
type Pair=(String, String)
type Pairs = Set[Pair]

case class Graph(edges: Pairs = Set(), labels: Pairs = Set()) {
  def label(p: Pair) = copy(labels=labels+p)
  def edge(p: Pair) = copy(edges=edges+p)
  override def toString = s"Graph(${edges.size} edges, ${labels.size} labels)"
}
```

Defining terms:

```tut:silent
val parent = term[Option[String]]('parent_guid)
val name = term[String]('name)
val guid = term[String]('guid)
```

A reducer that builds a `Graph` from rows.

```tut
val toGraph = reducer(Graph()) { (graph: Graph, row: Row) =>
    implicit def r = row
    val stage = parent() map (n => graph edge guid() -> n) getOrElse graph
    stage label guid() -> name()
}
```

Finally, build and run the query:

```tut
select ~ list(guid, name, parent) ~ from ~ 'accounts reduce toGraph runWithUrl url
```

# Errors

What happens if the type I expect does not agree with the column in the `ResultSet`?

```tut:nofail
val wrong = term[Int]('name)
query map { implicit row => wrong() } runWithUrl url
```

Or, using the `Row` `get` method:

```tut:nofail
query map { row => row get wrong } runWithUrl url
```
