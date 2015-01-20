import java.io.{FileReader, File, FileWriter}
import org.scalair.ir.llvm.LLVMParser
import org.scalair.ir.{ModuleConstPropPass, ModuleDominancePass, ImIr2Llvm, Llvm2ImIr}
import org.scalair.util.Logging
import org.slf4j.LoggerFactory
import util.parsing.input.{StreamReader, CharArrayReader}
import org.scalair.hoopl.TypeDefines.OrderedMap
/**
 * User: wangn
 * Date: 3/9/11
 */

object Main extends Logging {
  def writer(name:String, suffix:String, content:Option[Any]) {
    content.map { x =>
      val ssagOut = new FileWriter(name+suffix)
      ssagOut.write(content.toString)
      ssagOut.close()
    }
  }

  def main(args:Array[String]):Unit = {
    if (args.length == 1) {
      logger.info("input={}", args(0))
      val f = new File(args(0))
      val reader =
        if (f.exists)
          StreamReader(new FileReader(f))
        else
          new CharArrayReader(args(0).toCharArray())
      val result = new LLVMParser().parse(reader)
      writer(args(0), ".1.parser", result)

      val result2 = result.map { x =>
        Llvm2ImIr(x._1, x._2).clModule()
      }

      writer(args(0), ".2.ssag", result2)

      result2 match {
        case Some(r) => r.functionDefs.foreach(
          x => {
            val name = x.funProto.name.pureImg
            val gOut = new FileWriter(args(0)+".2.ssag." + name)
            gOut.write(x.graph.toDot(name))
            gOut.close()
          })
        case None => ()
      }

      val result3_0 = result2.map { x =>
        val (r1, r2) = ModuleDominancePass.run(x)
        r1
      }
      val result3_1 = result3_0.map { x =>
        val (r1, r2) = ModuleConstPropPass.run(x)
        r1
      }
      writer(args(0), ".3.const", result3_1)

      val result3 = result3_1

      val result4 = result3.map(ImIr2Llvm(_).clModule())
      writer(args(0), ".4.safe", result4)
    }
  }
}