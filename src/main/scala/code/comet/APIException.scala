package code.comet

class APIException(msg: String, cause:Throwable = null) extends RuntimeException(msg, cause)
