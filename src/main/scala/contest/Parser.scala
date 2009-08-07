package contest

import scala.util.parsing.combinator._
import scala.io.Source

import java.io.InputStream
import java.util.Date

import java.text.{DateFormat,SimpleDateFormat}

abstract class MyParser[T<:Entite] extends JavaTokenParsers{
  def identRepo: Parser[String] =
    """[A-Za-z-_\.0-9]*\/[A-Za-z-_\.0-9]*""".r
  def aDate:Parser[String]=
    """[0-9]{4}-[0-9]{2}-[0-9]{2}""".r
  def readFile(file:String):Stream[T] = {
    readFile(getClass.getClassLoader.getResourceAsStream(file))
  }
  def readFile(file:InputStream):Stream[T] = {
    val iter=    Source.fromInputStream(file).getLines
    def inner(iter:Iterator[String]):Stream[T]={
      if (iter.hasNext) Stream.cons(parseData(iter.next),inner(iter))
        else Stream.empty
    }
    inner(iter)
  } 

  def getMap(stream:Stream[T]):Map[Int,T]={
    stream.foldLeft(Map[Int,T]())((a,b:T)=>a++Map(b.id->b))
  }

  def getMap(file:String):Map[Int,T]={
    readFile(file).foldLeft(Map[Int,T]())((a,b:T)=>a++Map(b.id->b))
  }

  def parseData(input:String):T= {
    parseAll(line,input).get
  }
  def line : Parser[T]
}

object DataParser extends MyParser[Data]{
  def line : Parser[Data] =
    floatingPointNumber~":"~floatingPointNumber ^^{
    case user~":"~repo => new Data(user.toInt,repo.toInt)
  }
}

object RepoParser extends MyParser[Repo]{
  import contest.converter._

  def line :Parser[Repo] = 
    wholeNumber~":"~identRepo~","~aDate~opt(",")~opt(wholeNumber)^^ {
  case repoId~":"~repoName~","~date~ga~parentId=> {
    parentId match {
      case Some(s)=> new Repo(repoId.toInt, repoName, date,s.toInt)
      case None => new Repo(repoId.toInt, repoName, date)
    }
    //    case repoId~":"~repoName~","~date~","~parentId => new Repo(repoId.toInt, repoName, date,parentId)
  }
}
                                                                                                                                                    }

                                                                                                                                                    object LangParser extends MyParser[Lang]{
                                                                                                                                                      def langWithLine:Parser[(String,Int)]=
                                                                                                                                                        ident~";"~wholeNumber ^^ {
                                                                                                                                                        case langId~";"~numLine => (langId,numLine.toInt)
                                                                                                                                                      }

                                                                                                                                                      def languageMap : Parser[Map[String,Int]]=
                                                                                                                                                        repsep(langWithLine,",") ^^ (Map() ++ _) 

                                                                                                                                                      def line: Parser[Lang]=
                                                                                                                                                        wholeNumber~":"~languageMap ^^ {
                                                                                                                                                        case repoId~":"~mapLang => new Lang(repoId.toInt,mapLang)
                                                                                                                                                      }
                                                                                                                                                    }

