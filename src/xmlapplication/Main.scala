/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package xmlapplication

import scala.xml._
import java.io.File

object Main {

  case class Heritage(val address: String, val name: String, val primaryUse: Option[String], val date: Option[String])
  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    val workingDirectory: File = new File("/home/detriusxiiuser/NetBeansProjects/XMLApplication")
    val heritageXmlFile : File = new File(workingDirectory, "Heritage Properties.xml")
    
    val xmlChildren = XML.loadFile(heritageXmlFile).child
    
    val tagNames : Seq[String] = xmlChildren map ((u) => u.label)
    
    val table = xmlChildren.filter(_ match {
        case <Worksheet>{t @ _*}</Worksheet> => true
        case _ => false
      }).head.child.filter(_ match {
        case <Table>{_*}</Table> => true
        case _ => false
      }).head.child.filter(_ match {
        case <Row>{_*}</Row> => true
        case _ => false
      }).tail
    
    val cells = table.map(_ match {
        case <Row>{q @ _*}</Row> => {
            val t = q.filter(_ match {
                case <Cell><Data>{_*}</Data></Cell> => true
                case _ => false
              })
            val address =  if (t.length > 0) {
              t.apply(0) match {
                case <Cell><Data>{t @ _*}</Data></Cell> => t.apply(0).toString
                case u => u.toString
              }
            } else {
              ""
            }
            val name = if (t.length > 1) {
              t.apply(1) match {
                case <Cell><Data>{t @ _*}</Data></Cell> => t.apply(0).toString
                case _ => ""
              }
            } else {
              ""
            }
            val primaryUse = if (t.length > 2) {
              Some(t.apply(2) match {
                case <Cell><Data>{t @ _*}</Data></Cell> => t.apply(0).toString
                case _ => ""
              })
            } else {
              None
            }
            val date = if (t.length > 3) {
              Some(t.apply(3) match {
                case <Cell><Data>{t @ _*}</Data></Cell> => t.apply(0).toString
                case _ => ""
              })
            } else {
              None
            }
            
            Heritage(address, name, primaryUse, date)
        }
        case _ => Heritage("", "", None, None)
      })
    
    
    
    println(table)
    println(cells)
  }

}
