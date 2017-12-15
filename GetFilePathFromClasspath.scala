 def getFileFromClasspath(fileAbsPath: String): Option[String] = {
    val sourceCSVFileResource = getClass.getResource(fileAbsPath)
    Option(sourceCSVFileResource) match {
      case None => None
      case Some(x) => Some(x.getPath.toString())
    }

  }
