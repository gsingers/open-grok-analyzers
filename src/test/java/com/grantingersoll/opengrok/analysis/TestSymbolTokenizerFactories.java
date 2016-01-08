/*
 * CDDL HEADER START
 *
 * The contents of this file are subject to the terms of the
 * Common Development and Distribution License (the "License").
 * You may not use this file except in compliance with the License.
 *
 * See LICENSE.txt included in this distribution for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL HEADER in each
 * file and include the License file at LICENSE.txt.
 * If applicable, add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your own identifying
 * information: Portions Copyright [yyyy] [name of copyright owner]
 *
 * CDDL HEADER END
 */

package com.grantingersoll.opengrok.analysis;

import java.io.Reader;
import java.io.StringReader;

import org.apache.lucene.analysis.Tokenizer;
import org.junit.Test;

/**
 * Simple tests to ensure the symbol tokenizer factories are working.
 */
public class TestSymbolTokenizerFactories extends BaseTokenStreamFactoryTestCase {
  @Test public void testCSymbolTokenizerFactory() throws Exception {
    assertTokenization("CSymbol",
        "const char *msg = \"this is } sample { string\";",
        new String[]{"msg"});
  }

  @Test public void testClojureSymbolTokenizerFactory() throws Exception {
    assertTokenization("ClojureSymbol",
        "(with-out-str (with-in-str in-str (main/repl)))",
        new String[]{"in-str", "main/repl"});
  }

  @Test public void testCSharpSymbolTokenizerFactory() throws Exception {
    assertTokenization("CSharpSymbol",
        "[Tag] private T M4<T>(T p) where T : new()",
        new String[]{"Tag", "T", "M4", "T", "T", "p", "T"});
  }

  @Test public void testCxxSymbolTokenizerFactory() throws Exception {
    assertTokenization("CxxSymbol",
        "int MemberFunc(int a, int b) const { return a + b; }",
        new String[]{"MemberFunc", "a", "b", "a", "b"});
  }

  @Test public void testErlangSymbolTokenizerFactory() throws Exception {
    assertTokenization("ErlangSymbol",
        "return_port_data(Port) ->\n" +
        " receive {Port, {data, Data}} ->\n" +
        " binary_to_term(Data)\n" +
        " end.",
        new String[]{"return_port_data", "Port", "Port", "data", "Data", "binary_to_term", "Data"});
  }

  @Test public void testFortranSymbolTokenizerFactory() throws Exception {
    assertTokenization("FortranSymbol",
        "SUBROUTINE say_hello(who)\n" +
        "  CHARACTER(LEN=*), INTENT(in) :: who\n" +
        "  PRINT *, 'Hello ', who\n" +
        "END SUBROUTINE say_hello",
        new String[]{"say_hello", "who", "in", "who", "who", "say_hello"});
  }

  @Test public void testGolangSymbolTokenizerFactory() throws Exception {
    assertTokenization("GolangSymbol",
        "func (t T) Less(i, j int) bool { return t[i] < t[j] }",
        new String[]{"t", "T", "Less", "i", "j", "int", "bool", "t", "i", "t", "j"});
  }

  @Test public void testHaskellSymbolTokenizerFactory() throws Exception {
    assertTokenization("HaskellSymbol",
        "| otherwise   = do\n" +
        "    sem <- newMVar (initial, [], [])\n" +
        "    return (QSemN sem)",
        new String[]{"otherwise", "sem", "newMVar", "initial", "return", "QSemN", "sem"});
  }

  @Test public void testJavaSymbolTokenizerFactory() throws Exception {
    assertTokenization("JavaSymbol", "static private String MY_MEMBER = \"value\";",
        new String[]{"String", "MY_MEMBER"});
  }

  @Test public void testJavaScriptSymbolTokenizerFactory() throws Exception {
    assertTokenization("JavaScriptSymbol",
        "var sameobjectasndate = new Date(96, 11, 25); // false !",
        new String[]{"sameobjectasndate"});
  }

  @Test public void testLispSymbolTokenizerFactory() throws Exception {
    assertTokenization("LispSymbol",
        "(defun foo-bar () ( setq str_variable \"string value\" ))",
        new String[]{"foo-bar", "setq", "str_variable"});
  }

  @Test public void testPerlSymbolTokenizerFactory() throws Exception {
    assertTokenization("PerlSymbol",
        "   while (($firstnme, $lastname) = $sth->fetchrow()) {\n" +
        "       print \"$firstnme: $lastname\\n\";\n" +
        "   }\n",
        new String[]{"firstnme", "lastname", "sth", "fetchrow"});
  }

  @Test public void testPhpSymbolTokenizerFactory() throws Exception {
    assertTokenization("PhpSymbol",
        "<?php\n" +
        "protected $p_manager = 'myproduct::ConnectivityAssistant';\n" +
        "?>",
        new String[]{"p_manager"});
  }

  @Test public void testPlainSymbolTokenizerFactory() throws Exception {
    assertTokenization("PlainSymbol",
        "What's all this then?",
        new String[]{"What", "all", "this", "then"});
  }

  @Test public void testPythonSymbolTokenizerFactory() throws Exception {
    assertTokenization("PythonSymbol",
        "def main():\n" +
        "    # go ahead\n" +
        "    print \"hello world\"" +
        "if  __name__ == \"__main__\":\n" +
        "    main()",
        new String[]{"main", "__name__", "main"});
  }

  @Test public void testScalaSymbolTokenizerFactory() throws Exception {
    assertTokenization("ScalaSymbol",
        "if (i == 3) throw new IllegalStateException(\"processor %d: Drat!\" format i)",
        new String[]{"i", "IllegalStateException", "format", "i"});
  }

  @Test public void testShSymbolTokenizerFactory() throws Exception {
    assertTokenization("ShSymbol",
        "LC_ALL=\"C\"\n" +
        "export PYTHONPATH LC_ALL\n" +
        "${HOME}/bin/bazaar/bin/bzr \"$@\"\n" +
        "exit $?",
        new String[]{"LC_ALL", "PYTHONPATH", "LC_ALL", "HOME", "bin", "bazaar", "bin", "bzr"});
  }

  @Test public void testTclSymbolTokenizerFactory() throws Exception {
    assertTokenization("TclSymbol",
        "set wtn [concat ${wt}: $f]",
        new String[]{"wtn", "wt", "f"});
  }

  @Test public void testVBSymbolTokenizerFactory() throws Exception {
    assertTokenization("VBSymbol",
        "Public Function getPngQuantVersion() As String\n" +
        "\n" +
        "    If Not isPngQuantAvailable Then\n" +
        "        getPngQuantVersion = \"\"\n" +
        "        Exit Function",
        new String[]{"getPngQuantVersion", "isPngQuantAvailable", "getPngQuantVersion"});
  }

  @Test public void testBogusArguments() throws Exception {
    String[] tokenizerSPINames = {
        "CSymbol",       "ClojureSymbol", "CSharpSymbol",  "CxxSymbol",   "ErlangSymbol",
        "FortranSymbol", "GolangSymbol",  "HaskellSymbol", "JavaSymbol",  "JavaScriptSymbol",
        "LispSymbol",    "PerlSymbol",    "PhpSymbol",     "PlainSymbol", "PythonSymbol",
        "ScalaSymbol",   "ShSymbol",      "TclSymbol",     "VBSymbol",
    };
    for (String tokenizerSPIName : tokenizerSPINames) {
      instantiateWithBogusArguments(tokenizerSPIName);
    }
  }

  private void assertTokenization(String tokenizerSPIName, String input, String[] output) throws Exception {
    Reader reader = new StringReader(input);
    Tokenizer stream = tokenizerFactory(tokenizerSPIName).create(newAttributeFactory());
    stream.setReader(reader);
    assertTokenStreamContents(stream, output);
  }

  private void instantiateWithBogusArguments(String tokenizerSPIName) throws Exception {
    try {
      tokenizerFactory(tokenizerSPIName, "bogusArg", "bogusValue");
      fail();
    } catch (IllegalArgumentException expected) {
      assertTrue(expected.getMessage().contains("Unknown parameters"));
    }
  }
}
