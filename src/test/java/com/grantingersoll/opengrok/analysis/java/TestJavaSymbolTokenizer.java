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

package com.grantingersoll.opengrok.analysis.java;


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

public class TestJavaSymbolTokenizer extends BaseTokenStreamTestCase {
  private Analyzer analyzer;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    analyzer = new Analyzer() {
      @Override
      protected TokenStreamComponents createComponents(String fieldName) {
        Tokenizer tokenizer = new JavaSymbolTokenizer(newAttributeFactory());
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
    try (InputStream stream = TestJavaSymbolTokenizer.class.getResourceAsStream("Sample.java");
         Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      input = IOUtils.toString(in);
    }
    String[] output = new String[] {
                                                               //// /*
                                                               ////  * CDDL HEADER START
                                                               ////  *
                                                               ////  * The contents of this file are subject to the terms of the
                                                               ////  * Common Development and Distribution License (the "License").
                                                               ////  * You may not use this file except in compliance with the License.
                                                               ////  *
                                                               ////  * See LICENSE.txt included in this distribution for the specific
                                                               ////  * language governing permissions and limitations under the License.
                                                               ////  *
                                                               ////  * When distributing Covered Code, include this CDDL HEADER in each
                                                               ////  * file and include the License file at LICENSE.txt.
                                                               ////  * If applicable, add the following below this CDDL HEADER, with the
                                                               ////  * fields enclosed by brackets "[]" replaced with your own identifying
                                                               ////  * information: Portions Copyright [yyyy] [name of copyright owner]
                                                               ////  *
                                                               ////  * CDDL HEADER END
                                                               ////  */
                                                               ////
                                                               //// /*
                                                               ////  * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved.
                                                               ////  */
        "org", "opensolaris", "opengrok", "analysis", "java",  //// package org.opensolaris.opengrok.analysis.java;
                                                               ////
        "Sample",                                              //// public class Sample {
                                                               ////
        "String", "MY_MEMBER",                                 ////   static private String MY_MEMBER = "value";
                                                               ////
        "Sample",                                              ////   public Sample() {
                                                               ////
                                                               ////   }
                                                               ////
        "Method", "arg",                                       ////   public int Method(int arg) {
        "res",                                                 ////     int res = 5;
                                                               ////
        "res", "arg",                                          ////     res += arg;
                                                               ////
        "InnerClass", "i", "InnerClass",                       ////     InnerClass i = new InnerClass();
                                                               ////
        "i", "InnerMethod", "length", "res",                   ////     return i.InnerMethod().length() * res;
                                                               ////   }
                                                               ////
        "InnerClass",                                          ////   private class InnerClass {
                                                               ////
        "String", "InnerMethod",                               ////     public String InnerMethod() {
                                                               ////       // somthing } */
                                                               ////             /* }}}
                                                               ////                 multi-line comment }{}
                                                               ////             */
                                                               ////
        "System", "out", "print",                              ////       System.out.print("I'm so useless");
                                                               ////
                                                               ////       return "Why do robots need to drink?";
                                                               ////     }
                                                               ////
                                                               ////   }
                                                               ////
                                                               //// }
                                                               ////
    };
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void testMimeType() {
    SymbolTokenizer tokenizer = new JavaSymbolTokenizer(newAttributeFactory());
    assertEquals("text/x-java-source", tokenizer.getMimeType());
  }

  @Test
  public void testSourceCodeLanguage() {
    SymbolTokenizer tokenizer = new JavaSymbolTokenizer(newAttributeFactory());
    assertEquals("Java", tokenizer.getSourceCodeLanguage());
  }
}
