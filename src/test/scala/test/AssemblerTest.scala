package org.kr.cpu
package test

import assembler._
import org.scalactic.anyvals.PosZInt
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class AssemblerTest extends AnyFeatureSpec with ScalaCheckPropertyChecks {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50, maxDiscardedFactor = 30.0, minSize = PosZInt(100))

  Feature("Parse label line"):
    Scenario("Parse correct label line (with trailing colon)"):
      forAll(TestUtils.textGen):
        t =>
          val labelText = t+":"
          assert(AssemblerParser().process(labelText).contains(LabelLine(Label(labelText))))

    Scenario("Parse incorrect label line (w/o trailing colon)"):
      assert(AssemblerParser().process("adbc").isLeft)
      assert(AssemblerParser().process("12:g5").isLeft)
      assert(AssemblerParser().process(":r63").isLeft)

    Scenario("Parse incorrect label line (with text after or before)"):
      assert(AssemblerParser().process("adbc: some_other_text").isLeft)
      assert(AssemblerParser().process("WWW adbc:").isLeft)

  Feature("Parse symbol line"):
    Scenario("Parse correct symbol name and value"):
      forAll(TestUtils.textGen, TestUtils.textGen):
        (s, v) =>
          assert(AssemblerParser().process(f".SYMBOL $s $v").contains(SymbolLine(Operand(s),Operand(v))))

    Scenario("Parse incorrect symbol line (wrong keyword"):
      assert(AssemblerParser().process(f".SYMBOLxxx AAA BBB").isLeft)
      assert(AssemblerParser().process(f"SYMBOL AAA BBB").isLeft)

    Scenario("Parse incorrect symbol line (wrong number of operands"):
      assert(AssemblerParser().process(f".SYMBOL AAA BBB CCC").isLeft)
      assert(AssemblerParser().process(f".SYMBOL AAA").isLeft)
      assert(AssemblerParser().process(f"SYMBOL").isLeft)

  Feature("Parse origin line"):
    Scenario("Parse correct origin line with address or symbol"):
      forAll(TestUtils.textGen):
        o =>
          assert(AssemblerParser().process(f".ORG $o").contains(OrgLine(Operand(o))))

    Scenario("Parse incorrect origin line (wrong keyword"):
      assert(AssemblerParser().process(f".ORIG 0x1234").isLeft)
      assert(AssemblerParser().process(f"ORG 0x3245").isLeft)

    Scenario("Parse incorrect origin line (wrong number of operands"):
      assert(AssemblerParser().process(f".ORG 1111 2222").isLeft)
      assert(AssemblerParser().process(f".ORG").isLeft)

  Feature("Parse data line"):
    Scenario("Parse correct data line with 1-3 values"):
      forAll(TestUtils.textGen,TestUtils.textGen,TestUtils.textGen):
        (d1, d2, d3) =>
          assert(AssemblerParser().process(f".DATA $d1").contains(DataLine(Vector(Operand(d1)))))
          assert(AssemblerParser().process(f".DATA $d1 $d2").contains(DataLine(Vector(Operand(d1),Operand(d2)))))
          assert(AssemblerParser().process(f".DATA $d1 $d2 $d3").contains(DataLine(Vector(Operand(d1),Operand(d2),Operand(d3)))))

    Scenario("Parse incorrect data line (wrong keyword"):
      assert(AssemblerParser().process(f".DATAx 0x1234").isLeft)
      assert(AssemblerParser().process(f"DATA 0x3245").isLeft)

    Scenario("Parse incorrect data line (no operand"):
      assert(AssemblerParser().process(f".DATA").isLeft)

}
