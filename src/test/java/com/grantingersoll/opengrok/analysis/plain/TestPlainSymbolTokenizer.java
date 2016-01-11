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

package com.grantingersoll.opengrok.analysis.plain;


import com.grantingersoll.opengrok.analysis.JFlexTokenizer;
import org.apache.commons.io.IOUtils;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.BaseTokenStreamTestCase;
import org.apache.lucene.analysis.Tokenizer;
import org.junit.Test;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;

public class TestPlainSymbolTokenizer extends BaseTokenStreamTestCase {
  private Analyzer analyzer;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    analyzer = new Analyzer() {
      @Override
      protected TokenStreamComponents createComponents(String fieldName) {
        Tokenizer tokenizer = new PlainSymbolTokenizer(newAttributeFactory());
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
  public void test() throws Exception {
    String input;
    try (InputStream stream = TestPlainSymbolTokenizer.class.getResourceAsStream("sample");
         Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      input = IOUtils.toString(in);
    }
    String[] output = new String[] {
        "Sample", "plain", "text",                                  //// Sample plain text
                                                                    //// -----------------
                                                                    ////
        "Some", "words", "in", "here", "Two", "single", "letters",  //// Some words in here.  Two single letters: a, b.
                                                                    ////
        "More", "than", "tokens", "fewer", "than",                  //// More than 3 tokens, fewer than 10,000.
                                                                    ////
                                                                    ////
    };
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void testMimeType() {
    JFlexTokenizer tokenizer = new PlainSymbolTokenizer(newAttributeFactory());
    assertEquals("text/plain", tokenizer.getMimeType());
  }

  @Test
  public void testSourceCodeLanguage() {
    JFlexTokenizer tokenizer = new PlainSymbolTokenizer(newAttributeFactory());
    assertEquals("Plain", tokenizer.getSourceCodeLanguage());
  }
}
