 def getFileFromClasspath(classPath: String): Option[String] = {
    val resource = getClass.getResource(classPath)
    Option(resource) match {
      case None => None
      case Some(x) => Some(x.getPath.toString())
    }

  }
