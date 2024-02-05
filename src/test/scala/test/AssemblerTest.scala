package org.kr.cpu
package test

import assembler.{LabelLine, *}
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

    Scenario("Parse incorrect symbol line (one or none operand given"):
      assert(AssemblerParser().process(f".SYMBOL AAA").isLeft)
      assert(AssemblerParser().process(f"SYMBOL").isLeft)
}
