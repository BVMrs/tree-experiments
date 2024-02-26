package persistence

import com.apple.foundationdb._

object FoundationDBExample {

  def main(args: Array[String]): Unit = {
    // Connect to the FoundationDB cluster
    FDB.selectAPIVersion(710)
    val cluster = FDB.instance()
    val database = cluster.open()

    // Key-value pair
    val key = Tuple.from("example_key").pack()
    val value = Tuple.from("example_value").pack()

    // Upsert (insert or update) the record
    upsertRecord(database, key, value)

    // Read the record
    val retrievedValue = readRecord(database, key)
    println(s"Retrieved value: ${Tuple.fromBytes(retrievedValue).getString(0)}")

    // Close the connection
    database.close()
    cluster.close()
  }

  def upsertRecord(database: Database, key: Array[Byte], value: Array[Byte]): Unit = {
    database.run((tr: Transaction) => {
      tr.set(key, value)
      tr.commit()
    })
  }

  def readRecord(database: Database, key: Array[Byte]): Array[Byte] = {
    database.read((tr: ReadTransaction) => tr.get(key))
  }
}