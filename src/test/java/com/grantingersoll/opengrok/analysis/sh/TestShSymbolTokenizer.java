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

package com.grantingersoll.opengrok.analysis.sh;


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

public class TestShSymbolTokenizer extends BaseTokenStreamTestCase {
  private Analyzer analyzer;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    analyzer = new Analyzer() {
      @Override
      protected TokenStreamComponents createComponents(String fieldName) {
        Tokenizer tokenizer = new ShSymbolTokenizer(newAttributeFactory());
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
    try (InputStream stream = TestShSymbolTokenizer.class.getResourceAsStream("bazaar.sh");
         Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      input = IOUtils.toString(in);
    }
    String[] output = new String[] {
                                                                                      //// #! /bin/ksh
                                                                                      //// #
                                                                                      //// # CDDL HEADER START
                                                                                      //// #
                                                                                      //// # The contents of this file are subject to the terms of the
                                                                                      //// # Common Development and Distribution License (the "License").
                                                                                      //// # You may not use this file except in compliance with the License.
                                                                                      //// #
                                                                                      //// # See LICENSE.txt included in this distribution for the specific
                                                                                      //// # language governing permissions and limitations under the License.
                                                                                      //// #
                                                                                      //// # When distributing Covered Code, include this CDDL HEADER in each
                                                                                      //// # file and include the License file at LICENSE.txt.
                                                                                      //// # If applicable, add the following below this CDDL HEADER, with the
                                                                                      //// # fields enclosed by brackets "[]" replaced with your own identifying
                                                                                      //// # information: Portions Copyright [yyyy] [name of copyright owner]
                                                                                      //// #
                                                                                      //// # CDDL HEADER END
                                                                                      //// #
                                                                                      //// # Dummy wrapper-script to set up the PYTHONPATH and start bzr..
        "PYTHONPATH", "HOME", "bin", "bazaar", "lib", "python2", "site", "packages",  //// PYTHONPATH=${HOME}/bin/bazaar/lib/python2.5/site-packages
        "LC_ALL",                                                                     //// LC_ALL="C"
        "PYTHONPATH", "LC_ALL",                                                       //// export PYTHONPATH LC_ALL
        "HOME", "bin", "bazaar", "bin", "bzr",                                        //// ${HOME}/bin/bazaar/bin/bzr "$@"
                                                                                      //// exit $?
                                                                                      ////
    };
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void testMimeType() {
    SymbolTokenizer tokenizer = new ShSymbolTokenizer(newAttributeFactory());
    assertEquals("application/x-sh", tokenizer.getMimeType());
  }

  @Test
  public void testSourceCodeLanguage() {
    SymbolTokenizer tokenizer = new ShSymbolTokenizer(newAttributeFactory());
    assertEquals("Shell Script", tokenizer.getSourceCodeLanguage());
  }
}
