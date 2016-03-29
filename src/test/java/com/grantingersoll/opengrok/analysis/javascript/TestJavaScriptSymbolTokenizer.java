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

package com.grantingersoll.opengrok.analysis.javascript;


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

public class TestJavaScriptSymbolTokenizer extends BaseTokenStreamTestCase {
  private Analyzer analyzer;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    analyzer = new Analyzer() {
      @Override
      protected TokenStreamComponents createComponents(String fieldName) {
        Tokenizer tokenizer = new JavaScriptSymbolTokenizer(newAttributeFactory());
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
  public void testHexLiteral() throws Exception {
    String input = "0xFFFF";
    String[] output = new String[] {}; // zero output tokens
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void test() throws Exception {
    String input;
    try (InputStream stream = TestJavaScriptSymbolTokenizer.class.getResourceAsStream("function.js");
         Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      input = IOUtils.toString(in);
    }
    String[] output = new String[] {
                                       //// /*
                                       ////  * Sample js program
                                       ////  */
                                       ////
        "date",                        //// var date = new Date(96, 11, 25);
                                       //// //reference
        "ref", "date",                 //// var ref = date;
                                       ////
        "ref", "setDate",              //// ref.setDate(21);
                                       ////
        "date", "getDate",             //// date.getDate(); //21
                                       ////
                                       //// // The same is true when objects and arrays are passed to functions.
                                       //// // The following function adds a value to each element of an array.
                                       //// // A reference to the array is passed to the function, not a copy of the array.
                                       //// // Therefore, the function can change the contents of the array through
                                       //// // the reference, and those changes will be visible when the function returns.
        "main", "totals", "x",         //// function main(totals, x)
                                       //// {
        "totals", "totals", "x",       ////     totals[0] = totals[0] + x;
        "totals", "totals", "x",       ////     totals[1] = totals[1] + x;
        "totals", "totals", "x",       ////     totals[2] = totals[2] + x;
                                       //// }
                                       ////
        "numberliteral",               //// var numberliteral = 0x4f;
                                       ////
        "date", "ref",                 //// (date == ref)           // Evaluates to true
                                       ////
        "ndate",                       //// var ndate= new Date(96, 11, 25);
        "sameobjectasndate",           //// var sameobjectasndate = new Date(96, 11, 25);
                                       ////
        "ndate", "sameobjectasndate",  //// (ndate != sameobjectasndate)    // true !
                                       ////
                                       ////
    };
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void testMimeType() {
    SymbolTokenizer tokenizer = new JavaScriptSymbolTokenizer(newAttributeFactory());
    assertEquals("application/javascript", tokenizer.getMimeType());
  }

  @Test
  public void testSourceCodeLanguage() {
    SymbolTokenizer tokenizer = new JavaScriptSymbolTokenizer(newAttributeFactory());
    assertEquals("JavaScript", tokenizer.getSourceCodeLanguage());
  }
}
