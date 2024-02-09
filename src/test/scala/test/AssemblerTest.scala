package org.kr.cpu
package test

import assembler.*

import org.scalactic.anyvals.PosZInt
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class AssemblerTest extends AnyFeatureSpec with ScalaCheckPropertyChecks with GivenWhenThen:
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
          assert(AssemblerParser().process(f".SYMBOL $s=$v").contains(SymbolLine(Operand(s),Operand(v))))

    Scenario("Parse incorrect symbol line (wrong keyword or equals sign"):
      assert(AssemblerParser().process(f".SYMBOLxxx AAA = BBB").isLeft)
      assert(AssemblerParser().process(f".SYMBOL AAA BBB").isLeft)
      assert(AssemblerParser().process(f"SYMBOL AAA=BBB").isLeft)

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
          assert(AssemblerParser().process(f".DATA $d1, $d2").contains(DataLine(Vector(Operand(d1),Operand(d2)))))
          assert(AssemblerParser().process(f".DATA $d1, $d2 , $d3").contains(DataLine(Vector(Operand(d1),Operand(d2),Operand(d3)))))

    Scenario("Parse incorrect data line (wrong keyword"):
      assert(AssemblerParser().process(f".DATAx 0x1234").isLeft)
      assert(AssemblerParser().process(f"DATA 0x3245").isLeft)

    Scenario("Parse incorrect data line (no operand"):
      assert(AssemblerParser().process(f".DATA").isLeft)

  Feature("Parse 0-operand instruction"):
    Scenario("Parse correct 0-operand instruction"):
      TokenParser.mnemonic0list.foreach(m =>
        assert(AssemblerParser().process(f"$m").contains(Instruction0Line(Mnemonic0(m)))))

  Scenario("Parse incorrect 0-operand instruction with operand"):
    TokenParser.mnemonic0list.foreach(m =>
      assert(AssemblerParser().process(f"$m AAA").isLeft))

  Feature("Parse 1-operand instruction"):
    Scenario("Parse correct 1-operand instruction"):
      forAll(TestUtils.textGen):
        op =>
          TokenParser.mnemonic1list.foreach(m =>
            assert(AssemblerParser().process(f"$m $op").contains(Instruction1Line(Mnemonic1(m),Operand(op)))))

    Scenario("Parse incorrect 1-operand instruction without operand"):
      TokenParser.mnemonic1list.foreach(m =>
        assert(AssemblerParser().process(f"$m").isLeft))

    Scenario("Parse incorrect 1-operand instruction with 2 operands"):
      TokenParser.mnemonic1list.foreach(m =>
        assert(AssemblerParser().process(f"$m AAA BBB").isLeft))

  Feature("Parse 2-operand instruction"):
    Scenario("Parse 2-operand instruction"):
      forAll(TestUtils.textGen, TestUtils.textGen):
        (op1, op2) =>
          TokenParser.mnemonic2list.foreach(m =>
            assert(AssemblerParser().process(f"$m $op1, $op2").contains(Instruction2Line(Mnemonic2(m),Operand(op1),Operand(op2)))))

    Scenario("Parse incorrect 2-operand instruction without operand"):
      TokenParser.mnemonic2list.foreach(m =>
        assert(AssemblerParser().process(f"$m").isLeft))

    Scenario("Parse incorrect 2-operand instruction with 1 operand"):
      TokenParser.mnemonic2list.foreach(m =>
        assert(AssemblerParser().process(f"$m AAA").isLeft))

    Scenario("Parse incorrect 2-operand instruction with 3 operands"):
      TokenParser.mnemonic2list.foreach(m =>
        assert(AssemblerParser().process(f"$m AAA BBB CCC").isLeft))

  Feature("ignore comments after instructions and commands"):
    Scenario("ignore comment after label"):
      val labelText = "someLabel:"
      val expected = LabelLine(Label(labelText))
      assert(AssemblerParser().process(f"$labelText # comment1 comment2").contains(expected))
      assert(AssemblerParser().process(f"$labelText#comment1 comment2").contains(expected))

    Scenario("ignore comment after symbol"):
      val symbol = "someSymbol"
      val value = "someValue"
      val expected = SymbolLine(Operand(symbol),Operand(value))
      assert(AssemblerParser().process(f".SYMBOL $symbol=$value # comment1 comment2").contains(expected))
      assert(AssemblerParser().process(f".SYMBOL $symbol=$value#comment1 comment2").contains(expected))

    Scenario("ignore comment after origin"):
      val address = "someAddress"
      val expected = OrgLine(Operand(address))
      assert(AssemblerParser().process(f".ORG $address # comment1 comment2").contains(expected))
      assert(AssemblerParser().process(f".ORG $address#comment1 comment2").contains(expected))

    Scenario("ignore comment after data"):
      val data = "someData"
      val expected = DataLine(Vector(Operand(data),Operand(data)))
      assert(AssemblerParser().process(f".DATA $data, $data # comment1 comment2").contains(expected))
      assert(AssemblerParser().process(f".DATA $data, $data#comment1 comment2").contains(expected))

    Scenario("ignore comment in empty line"):
      val expected = EmptyLine()
      assert(AssemblerParser().process(f" # comment1 comment2").contains(expected))
      assert(AssemblerParser().process(f"#comment1 comment2").contains(expected))

    Scenario("Ignore comment after 0-operand instruction"):
      TokenParser.mnemonic0list.foreach(m =>
        val expected = Instruction0Line(Mnemonic0(m))
        assert(AssemblerParser().process(f"$m # comment1 comment2").contains(expected))
        assert(AssemblerParser().process(f"$m#comment1 comment2").contains(expected)))

    Scenario("Ignore comment after 1-operand instruction"):
      TokenParser.mnemonic1list.foreach(m =>
        val expected = Instruction1Line(Mnemonic1(m),Operand("AAA"))
        assert(AssemblerParser().process(f"$m AAA # comment1 comment2").contains(expected))
        assert(AssemblerParser().process(f"$m AAA#comment1 comment2").contains(expected)))

    Scenario("Ignore comment after 2-operand instruction"):
      TokenParser.mnemonic2list.foreach(m =>
        val expected = Instruction2Line(Mnemonic2(m), Operand("AAA"), Operand("BBB"))
        assert(AssemblerParser().process(f"$m AAA, BBB # comment1 comment2").contains(expected))
        assert(AssemblerParser().process(f"$m AAA, BBB#comment1 comment2").contains(expected)))

  private val program =
    """
      |.ORG 0x0010
      |.SYMBOL V1=0x000F
      |.SYMBOL V2=0x0010
      |.SYMBOL V3=V2 # not used
      |START:
      |LDR R5,V1
      |LDR R6,V3
      |CMP R5,R6
      |JMPIZ END
      |LDA 0x0000
      |LD R3,P0
      |END:
      |LDA 0x0001
      |LD R3,P1
      |""".stripMargin 
  
  Feature("Parse a program"):
    Scenario("Parse a program"):
      Given("a program")
      When("parsed")
      val progParsed = program.split('\n').toVector.map(AssemblerParser().process)
      Then("each line is parsed")
      assert(progParsed.forall(_.isRight))

  private val symbolProgram =
    """
      |.SYMBOL V1=0x0001
      |.SYMBOL V2=V1
      |.SYMBOL V3=V2
      |.SYMBOL V4=V3
      |.SYMBOL V5=V4
      |.SYMBOL V6=V5
      |.SYMBOL V7=V6
      |.SYMBOL V8=V7
      |.SYMBOL V9=V8
      |.SYMBOL VA=V9
      |""".stripMargin

  Feature("Replace symbols with their values"):
    Scenario("Replace symbols"):
      Given("a program")
      When("processed")
      val assembler = Assembler(program)
      Then("the progam is valid")
      assert(assembler.isValid)
      assert(assembler.symbols.get(Operand("V1")).contains(Operand("0x000F")))
      assert(assembler.symbols.get(Operand("V2")).contains(Operand("0x0010")))
      assert(assembler.symbols.get(Operand("V3")).contains(Operand("V2")))
      val replaced = assembler.withSymbolsReplaced.getOrElse(Vector())
      assert(replaced(3)==SymbolLine(Operand("V3"),Operand("0x0010")))
      assert(replaced(5)==Instruction2Line(Mnemonic2("LDR"),Operand("R5"),Operand("0x000F")))
      assert(replaced(6)==Instruction2Line(Mnemonic2("LDR"),Operand("R6"),Operand("0x0010")))
    
    Scenario("Replace nested symbols"):
      Given("a program with many nested levels of symbols")
      When("processed")
      val assembler = Assembler(symbolProgram)
      Then("all symbols are correctly replaced")
      val replaced = assembler.withSymbolsReplaced.getOrElse(Vector())
      assert(replaced(9)==SymbolLine(Operand("VA"),Operand("0x0001")))

    Scenario("Do not replace when there are too many levels of nested symbols"):
      Given("a program with too many nested levels of symbols")
      When("processed")
      // add another level
      val assembler = Assembler(symbolProgram+"\n.SYMBOL VB=VA")
      Then("all symbols are correctly replaced")
      assert(!assembler.isValid)
      assert(assembler.withSymbolsReplaced.isLeft)

    Scenario("Do not replace when there is circular reference"):
      Given("a program with circular reference")
      val circularSymbolProgram = ".SYMBOL V1 = V2\n.SYMBOL V2 = V1"
      When("processed")
      val assembler = Assembler(circularSymbolProgram)
      Then("all symbols are correctly replaced")
      assert(!assembler.isValid)
      assert(assembler.withSymbolsReplaced.isLeft)
      
  Feature("calculate addresses for lines"):
    Scenario("calculate addresses lines"):
      Given("a program")
      When("processed")
      val assembler = Assembler(program)
      Then("addresses are calculated properly")
      assert(assembler.withAddress.isRight)
      val addressed = assembler.withAddress.getOrElse(Vector())
      // NOTE: all lines with assembler keywords (except for .DATA) or labels do not affect address
      assert(addressed.map(_.address)==Vector(16,16,16,16,16,16,19,22,23,26,28,29,29,31))