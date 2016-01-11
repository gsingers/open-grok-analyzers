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

package com.grantingersoll.opengrok.analysis.c;

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

public class TestCSymbolTokenizer extends BaseTokenStreamTestCase {
  private Analyzer analyzer;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    analyzer = new Analyzer() {
      @Override
      protected TokenStreamComponents createComponents(String fieldName) {
        Tokenizer tokenizer = new CSymbolTokenizer(newAttributeFactory());
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
    try (InputStream stream = TestCSymbolTokenizer.class.getResourceAsStream("sample.c");
         Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      input = IOUtils.toString(in);
    }
    String[] output = new String[] {
                                       //// // compile me with gcc
                                       //// /* this is sample comment } */
        "stdio", "h",                  ////     #include <stdio.h>
        "string", "h",                 ////     #include <string.h>
                                       ////
        "TEST", "x", "x",              ////     #define TEST(x) (x)
                                       ////
        "foo", "a", "b",               ////     int foo(int a, int b) {
                                       ////     /* blah blah
                                       ////         }
                                       ////     */
                                       ////
        "c",                           ////       int c;
        "msg",                         ////       const char *msg = "this is } sample { string";
        "a", "b",                      ////       if (a < b) {
        "strlen", "msg",               ////         return strlen(msg);
                                       ////       } else {
                                       ////         // }}}}} something to return
        "c", "TEST", "a", "TEST", "b", ////         c = TEST(a) + TEST(b);
                                       ////       }
        "c",                           ////       return c;
                                       ////     }
                                       ////
        "bar", "x",                    ////     int bar(int x /* } */)
                                       ////     {
                                       ////       // another function
        "d",                           ////       int d;
        "f",                           ////       int f;
        "printf", "TEST",              ////       printf(TEST("test { message|$#@$!!#"));
        "d", "foo",                    ////       d = foo(2, 4);
        "f", "foo", "x", "d",          ////       f = foo(x, d);
                                       ////
                                       ////     /* return
                                       ////         some
                                       ////          rubish
                                       ////     */
        "d", "f",                      ////       return d+f;
                                       ////     }
                                       ////
                                       //// // main function
        "main", "argc", "argv",        ////     int main(int argc, char *argv[]) {
        "res",                         ////       int res;
        "printf",                      ////       printf("this is just a {sample}}");
                                       ////
        "res", "bar",                  ////       res = bar(20);
        "printf", "res",               ////       printf("result = {%d}\n", res);
                                       ////
                                       ////       return 0; }
                                       ////
    };
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void testMimeType() {
    SymbolTokenizer tokenizer = new CSymbolTokenizer(newAttributeFactory());
    assertEquals("text/x-csrc", tokenizer.getMimeType());
  }

  @Test
  public void testSourceCodeLanguage() {
    SymbolTokenizer tokenizer = new CSymbolTokenizer(newAttributeFactory());
    assertEquals("C", tokenizer.getSourceCodeLanguage());
  }
}
