// Xiang Zhu
// DS4300 HW4 Part B: 4

import scala.collection.mutable.HashMap

/*
Implement a Scala- based key-value store as a single class.
The class should have the following redis-like methods:

        get(key: String): String
        set(key: String, value: String)
        lpush(key: String, value: String)
        rpush(key: String, value: String)
        lpop(key: String): String
        rpop(key: String): String
        lrange(key: String, start: Int, stop: Int): List[String]
        llen(key: String)
        flushall()
 */
class Redis {

  var keyValue = HashMap[String, List[String]]()

  // Get the value of key.
  def get(key: String): List[String] = {
    val value = keyValue get key
    value match {
      case Some(x) => x
      case None => Nil
    }
  }

  // Set key to hold the string value.
  def set(key: String, value: String): Unit = {
    if (!(keyValue contains key))
      (keyValue += (key -> List(value)))
    else
      (keyValue += (key -> (value +: findValue(key))))
  }

  // Insert all the specified values at the head of the list stored at key.
  // RETURN the length of the list
  def lpush(key: String, value: String): Int = {
    (keyValue += (key -> (value +: findValue(key))))
    llen(key)
  }

  // Insert all the specified values at the tail of the list stored at key.
  // RETURN the length of the list
  def rpush(key: String, value: String): Int = {
    (keyValue += (key -> (findValue(key) :+ value)))
    llen(key)
  }

  // Removes and returns the first element of the list stored at key.
  def lpop(key: String): String = {
    val losValue = findValue(key)
    (keyValue += (key -> losValue.drop(1)))
    losValue.head
  }

  // Removes and returns the last element of the list stored at key.
  def rpop(key: String): String = {
    val losValue = findValue(key)
    (keyValue += (key -> losValue.dropRight(1)))
    losValue.last
  }

  // Returns the specified elements of the list stored at key.
  // The offsets start and stop are zero-based indexes, with 0 being the first element of
  // the list (the head of the list), 1 being the next element and so on.
  def lrange(key: String, start: Int, stop: Int): List[String] = {
    val losValue = findValue(key)
    var specifiedElements = List[String]()
    if ((start > stop) || (stop > (losValue length)))
      throw new Exception("The start index should be less than the end index.")
    else
      for(i <- start until stop) {
        specifiedElements = (specifiedElements :+ losValue(i))
      }
    return specifiedElements
  }

  // Returns the length of the list stored at key
  def llen(key: String): Int = {
    findValue(key) length
  }
        
  // reset the class
  def flushall() = {
    keyValue = HashMap[String, List[String]]()
  }

  // Returns the value of the key, if not, return empty set.
  def findValue(k:String): List[String] = keyValue getOrElse(k, List.empty)
}
