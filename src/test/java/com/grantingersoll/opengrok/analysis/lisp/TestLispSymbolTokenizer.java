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

package com.grantingersoll.opengrok.analysis.lisp;


import com.grantingersoll.opengrok.analysis.SymbolTokenizer;
import org.apache.commons.io.IOUtils;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.BaseTokenStreamTestCase;
import org.apache.lucene.analysis.Tokenizer;
import org.junit.Test;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;

public class TestLispSymbolTokenizer extends BaseTokenStreamTestCase {
  private Analyzer analyzer;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    analyzer = new Analyzer() {
      @Override
      protected TokenStreamComponents createComponents(String fieldName) {
        Tokenizer tokenizer = new LispSymbolTokenizer(newAttributeFactory());
        return new TokenStreamComponents(tokenizer);
      }
    };
  }

  @Override
  public void tearDown() throws Exception {
    analyzer.close();
    super.tearDown();
  }

  @Test
  public void testNumericLiterals() throws Exception {
    String input = "#xFFFF #o777";
    String[] output = new String[] {}; // zero output tokens
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void test() throws Exception {
    String input;
    try (InputStream stream = TestLispSymbolTokenizer.class.getResourceAsStream("test.el");
         Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      input = IOUtils.toString(in);
    }
    String[] output = new String[] {
                                        //// ;
                                        //// ; CDDL HEADER START
                                        //// ;
                                        //// ; The contents of this file are subject to the terms of the
                                        //// ; Common Development and Distribution License (the "License").
                                        //// ; You may not use this file except in compliance with the License.
                                        //// ;
                                        //// ; See LICENSE.txt included in this distribution for the specific
                                        //// ; language governing permissions and limitations under the License.
                                        //// ;
                                        //// ; When distributing Covered Code, include this CDDL HEADER in each
                                        //// ; file and include the License file at LICENSE.txt.
                                        //// ; If applicable, add the following below this CDDL HEADER, with the
                                        //// ; fields enclosed by brackets "[]" replaced with your own identifying
                                        //// ; information: Portions Copyright [yyyy] [name of copyright owner]
                                        //// ;
                                        //// ; CDDL HEADER END
                                        //// ;
    "foo-bar", "setq", "variable",      //// (defun foo-bar () ( setq variable 5 ))
    "foo-bar", "setq", "str_variable",  //// (defun foo-bar () ( setq str_variable "string value" ))
    "foo-bar",                          //// (foo-bar)
                                        //// ; Multi line comment, with embedded strange characters: < > &,
                                        //// ; email address: testuser@example.com and even an URL:
                                        //// ; http://www.example.com/index.html and a file name and a path:
                                        //// ; <example.cpp> and </usr/local/example.h>.
                                        //// ; Ending with an email address: username@example.com
                                        ////
    };
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void testMimeType() {
    SymbolTokenizer tokenizer = new LispSymbolTokenizer(newAttributeFactory());
    assertEquals("application/x-lisp", tokenizer.getMimeType());
  }

  @Test
  public void testSourceCodeLanguage() {
    SymbolTokenizer tokenizer = new LispSymbolTokenizer(newAttributeFactory());
    assertEquals("Lisp", tokenizer.getSourceCodeLanguage());
  }
}
