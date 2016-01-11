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

package com.grantingersoll.opengrok.analysis.csharp;


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

public class TestCSharpSymbolTokenizer extends BaseTokenStreamTestCase {
  private Analyzer analyzer;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    analyzer = new Analyzer() {
      @Override
      protected TokenStreamComponents createComponents(String fieldName) {
        Tokenizer tokenizer = new CSharpSymbolTokenizer(newAttributeFactory());
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
    try (InputStream stream = TestCSharpSymbolTokenizer.class.getResourceAsStream("Sample.cs");
         Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      input = IOUtils.toString(in);
    }
    String[] output = new String[] {
        "System",                       //// using System;
                                        ////
        "MyNamespace",                  //// namespace MyNamespace
                                        //// {
                                        ////     /// <summary>
                                        ////     /// summary
                                        ////     /// </summary>
        "Tag",                          ////     [Tag]
        "TopClass",                     ////     class TopClass {
        "M1",                           ////         public int M1() { }
                                        ////
        "M2",                           ////         public static void M2() {
                                        ////             //public static int MERR() {}
                                        ////         }
                                        ////
                                        ////         /// <summary>
                                        ////         /// sum
                                        ////         /// </summary>
        "M3",                           ////         public void M3()
                                        ////         {
                                        ////             /* hi
                                        ////                 public static int MERR() {
                                        ////                 }
                                        ////             */
                                        ////         }
                                        ////
        "Tag",                          ////         [Tag]
        "T", "M4", "T", "T", "p", "T",  ////         private T M4<T>(T p) where T : new()
                                        ////         {
                                        ////         }
                                        ////
        "InnerClass",                   ////         public class InnerClass
                                        ////         {
        "M5", "x",                      ////             private string M5(int x) {
                                        ////                 return null;
                                        ////             }
                                        ////         }
                                        ////
                                        ////     {
                                        ////
                                        //// }
                                        ////
    };
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void testMimeType() {
    SymbolTokenizer tokenizer = new CSharpSymbolTokenizer(newAttributeFactory());
    assertEquals("text/x-csharp", tokenizer.getMimeType());
  }

  @Test
  public void testSourceCodeLanguage() {
    SymbolTokenizer tokenizer = new CSharpSymbolTokenizer(newAttributeFactory());
    assertEquals("C#", tokenizer.getSourceCodeLanguage());
  }
}
