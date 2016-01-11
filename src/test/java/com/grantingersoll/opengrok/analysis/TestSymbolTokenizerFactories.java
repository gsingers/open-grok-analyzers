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
    SymbolTokenizerFactory factory = (SymbolTokenizerFactory)tokenizerFactory("CSymbol");
    assertTokenization(factory,
        "const char *msg = \"this is } sample { string\";",
        new String[]{"msg"});

    assertEquals("C", factory.getSourceCodeLanguage());
    assertEquals("text/x-csrc", factory.getMimeType());
  }

  @Test public void testClojureSymbolTokenizerFactory() throws Exception {
    SymbolTokenizerFactory factory = (SymbolTokenizerFactory)tokenizerFactory("ClojureSymbol");
    assertTokenization(factory,
        "(with-out-str (with-in-str in-str (main/repl)))",
        new String[]{"in-str", "main/repl"});

    assertEquals("Clojure", factory.getSourceCodeLanguage());
    assertEquals("text/x-clojure", factory.getMimeType());
  }

  @Test public void testCSharpSymbolTokenizerFactory() throws Exception {
    SymbolTokenizerFactory factory = (SymbolTokenizerFactory)tokenizerFactory("CSharpSymbol");
    assertTokenization(factory,
        "[Tag] private T M4<T>(T p) where T : new()",
        new String[]{"Tag", "T", "M4", "T", "T", "p", "T"});

    assertEquals("C#", factory.getSourceCodeLanguage());
    assertEquals("text/x-csharp", factory.getMimeType());
  }

  @Test public void testCxxSymbolTokenizerFactory() throws Exception {
    SymbolTokenizerFactory factory = (SymbolTokenizerFactory)tokenizerFactory("CxxSymbol");
    assertTokenization(factory,
        "int MemberFunc(int a, int b) const { return a + b; }",
        new String[]{"MemberFunc", "a", "b", "a", "b"});

    assertEquals("C++", factory.getSourceCodeLanguage());
    assertEquals("text/x-c++src", factory.getMimeType());
  }

  @Test public void testErlangSymbolTokenizerFactory() throws Exception {
    SymbolTokenizerFactory factory = (SymbolTokenizerFactory)tokenizerFactory("ErlangSymbol");
    assertTokenization(factory,
        "return_port_data(Port) ->\n" +
        " receive {Port, {data, Data}} ->\n" +
        " binary_to_term(Data)\n" +
        " end.",
        new String[]{"return_port_data", "Port", "Port", "data", "Data", "binary_to_term", "Data"});

    assertEquals("Erlang", factory.getSourceCodeLanguage());
    assertEquals("text/x-erlang", factory.getMimeType());
  }

  @Test public void testFortranSymbolTokenizerFactory() throws Exception {
    SymbolTokenizerFactory factory = (SymbolTokenizerFactory)tokenizerFactory("FortranSymbol");
    assertTokenization(factory,
        "SUBROUTINE say_hello(who)\n" +
        "  CHARACTER(LEN=*), INTENT(in) :: who\n" +
        "  PRINT *, 'Hello ', who\n" +
        "END SUBROUTINE say_hello",
        new String[]{"say_hello", "who", "in", "who", "who", "say_hello"});

    assertEquals("Fortran", factory.getSourceCodeLanguage());
    assertEquals("text/x-fortran", factory.getMimeType());
  }

  @Test public void testGolangSymbolTokenizerFactory() throws Exception {
    SymbolTokenizerFactory factory = (SymbolTokenizerFactory)tokenizerFactory("GolangSymbol");
    assertTokenization(factory,
        "func (t T) Less(i, j int) bool { return t[i] < t[j] }",
        new String[]{"t", "T", "Less", "i", "j", "int", "bool", "t", "i", "t", "j"});

    assertEquals("Golang", factory.getSourceCodeLanguage());
    assertEquals("text/x-go", factory.getMimeType());
  }

  @Test public void testHaskellSymbolTokenizerFactory() throws Exception {
    SymbolTokenizerFactory factory = (SymbolTokenizerFactory)tokenizerFactory("HaskellSymbol");
    assertTokenization(factory,
        "| otherwise   = do\n" +
        "    sem <- newMVar (initial, [], [])\n" +
        "    return (QSemN sem)",
        new String[]{"otherwise", "sem", "newMVar", "initial", "return", "QSemN", "sem"});

    assertEquals("Haskell", factory.getSourceCodeLanguage());
    assertEquals("text/x-haskell", factory.getMimeType());
  }

  @Test public void testJavaSymbolTokenizerFactory() throws Exception {
    SymbolTokenizerFactory factory = (SymbolTokenizerFactory)tokenizerFactory("JavaSymbol");
    assertTokenization(factory,
        "static private String MY_MEMBER = \"value\";",
        new String[]{"String", "MY_MEMBER"});

    assertEquals("Java", factory.getSourceCodeLanguage());
    assertEquals("text/x-java-source", factory.getMimeType());
  }

  @Test public void testJavaScriptSymbolTokenizerFactory() throws Exception {
    SymbolTokenizerFactory factory = (SymbolTokenizerFactory)tokenizerFactory("JavaScriptSymbol");
    assertTokenization(factory,
        "var sameobjectasndate = new Date(96, 11, 25); // false !",
        new String[]{"sameobjectasndate"});

    assertEquals("JavaScript", factory.getSourceCodeLanguage());
    assertEquals("application/javascript", factory.getMimeType());
  }

  @Test public void testLispSymbolTokenizerFactory() throws Exception {
    SymbolTokenizerFactory factory = (SymbolTokenizerFactory)tokenizerFactory("LispSymbol");
    assertTokenization(factory,
        "(defun foo-bar () ( setq str_variable \"string value\" ))",
        new String[]{"foo-bar", "setq", "str_variable"});

    assertEquals("Lisp", factory.getSourceCodeLanguage());
    assertEquals("application/x-lisp", factory.getMimeType());
  }

  @Test public void testPerlSymbolTokenizerFactory() throws Exception {
    SymbolTokenizerFactory factory = (SymbolTokenizerFactory)tokenizerFactory("PerlSymbol");
    assertTokenization(factory,
        "   while (($firstnme, $lastname) = $sth->fetchrow()) {\n" +
        "       print \"$firstnme: $lastname\\n\";\n" +
        "   }\n",
        new String[]{"firstnme", "lastname", "sth", "fetchrow"});

    assertEquals("Perl", factory.getSourceCodeLanguage());
    assertEquals("text/x-perl", factory.getMimeType());
  }

  @Test public void testPhpSymbolTokenizerFactory() throws Exception {
    SymbolTokenizerFactory factory = (SymbolTokenizerFactory)tokenizerFactory("PhpSymbol");
    assertTokenization(factory,
        "<?php\n" +
        "protected $p_manager = 'myproduct::ConnectivityAssistant';\n" +
        "?>",
        new String[]{"p_manager"});

    assertEquals("PHP", factory.getSourceCodeLanguage());
    assertEquals("text/x-php", factory.getMimeType());
  }

  @Test public void testPlainSymbolTokenizerFactory() throws Exception {
    SymbolTokenizerFactory factory = (SymbolTokenizerFactory)tokenizerFactory("PlainSymbol");
    assertTokenization(factory,
        "What's all this then?",
        new String[]{"What", "all", "this", "then"});

    assertEquals("Plain", factory.getSourceCodeLanguage());
    assertEquals("text/plain", factory.getMimeType());
  }

  @Test public void testPythonSymbolTokenizerFactory() throws Exception {
    SymbolTokenizerFactory factory = (SymbolTokenizerFactory)tokenizerFactory("PythonSymbol");
    assertTokenization(factory,
        "def main():\n" +
        "    # go ahead\n" +
        "    print \"hello world\"" +
        "if  __name__ == \"__main__\":\n" +
        "    main()",
        new String[]{"main", "__name__", "main"});

    assertEquals("Python", factory.getSourceCodeLanguage());
    assertEquals("text/x-python", factory.getMimeType());
  }

  @Test public void testScalaSymbolTokenizerFactory() throws Exception {
    SymbolTokenizerFactory factory = (SymbolTokenizerFactory)tokenizerFactory("ScalaSymbol");
    assertTokenization(factory,
        "if (i == 3) throw new IllegalStateException(\"processor %d: Drat!\" format i)",
        new String[]{"i", "IllegalStateException", "format", "i"});

    assertEquals("Scala", factory.getSourceCodeLanguage());
    assertEquals("text/x-scala", factory.getMimeType());
  }

  @Test public void testShSymbolTokenizerFactory() throws Exception {
    SymbolTokenizerFactory factory = (SymbolTokenizerFactory)tokenizerFactory("ShSymbol");
    assertTokenization(factory,
        "LC_ALL=\"C\"\n" +
        "export PYTHONPATH LC_ALL\n" +
        "${HOME}/bin/bazaar/bin/bzr \"$@\"\n" +
        "exit $?",
        new String[]{"LC_ALL", "PYTHONPATH", "LC_ALL", "HOME", "bin", "bazaar", "bin", "bzr"});

    assertEquals("Shell Script", factory.getSourceCodeLanguage());
    assertEquals("application/x-sh", factory.getMimeType());
  }

  @Test public void testTclSymbolTokenizerFactory() throws Exception {
    SymbolTokenizerFactory factory = (SymbolTokenizerFactory)tokenizerFactory("TclSymbol");
    assertTokenization(factory,
        "set wtn [concat ${wt}: $f]",
        new String[]{"wtn", "wt", "f"});

    assertEquals("Tcl", factory.getSourceCodeLanguage());
    assertEquals("text/x-tcl", factory.getMimeType());
  }

  @Test public void testVBSymbolTokenizerFactory() throws Exception {
    SymbolTokenizerFactory factory = (SymbolTokenizerFactory)tokenizerFactory("VBSymbol");
    assertTokenization(factory,
        "Public Function getPngQuantVersion() As String\n" +
        "\n" +
        "    If Not isPngQuantAvailable Then\n" +
        "        getPngQuantVersion = \"\"\n" +
        "        Exit Function",
        new String[]{"getPngQuantVersion", "isPngQuantAvailable", "getPngQuantVersion"});

    assertEquals("Visual Basic", factory.getSourceCodeLanguage());
    assertEquals("text/x-vbasic", factory.getMimeType());
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

  private void assertTokenization(SymbolTokenizerFactory factory, String input, String[] output) throws Exception {
    Reader reader = new StringReader(input);
    Tokenizer stream = factory.create(newAttributeFactory());
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
