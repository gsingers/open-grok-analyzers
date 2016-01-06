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

package com.grantingersoll.opengrok.analysis.fortran;


import org.apache.commons.io.IOUtils;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.BaseTokenStreamTestCase;
import org.apache.lucene.analysis.Tokenizer;
import org.junit.Test;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;

public class TestFortranSymbolTokenizer extends BaseTokenStreamTestCase {
  private Analyzer analyzer;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    analyzer = new Analyzer() {
      @Override
      protected TokenStreamComponents createComponents(String fieldName) {
        Tokenizer tokenizer = new FortranSymbolTokenizer(newAttributeFactory());
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
    try (InputStream stream = TestFortranSymbolTokenizer.class.getResourceAsStream("hello.f90");
         Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      input = IOUtils.toString(in);
    }
    String[] output = new String[] {
                             //// !
                             //// ! CDDL HEADER START
                             //// !
                             //// ! The contents of this file are subject to the terms of the
                             //// ! Common Development and Distribution License (the "License").
                             //// ! You may not use this file except in compliance with the License.
                             //// !
                             //// ! See LICENSE.txt included in this distribution for the specific
                             //// ! language governing permissions and limitations under the License.
                             //// !
                             //// ! When distributing Covered Code, include this CDDL HEADER in each
                             //// ! file and include the License file at LICENSE.txt.
                             //// ! If applicable, add the following below this CDDL HEADER, with the
                             //// ! fields enclosed by brackets "[]" replaced with your own identifying
                             //// ! information: Portions Copyright [yyyy] [name of copyright owner]
                             //// !
                             //// ! CDDL HEADER END
                             //// !
        "hello",             //// PROGRAM hello
                             //// !
                             //// ! This is a comment
                             //// !
                             ////
        "start",             ////   REAL, PARAMETER :: start = 0.0
                             ////   REAL, PARAMETER :: end = 12.0
        "incr",              ////   REAL count, incr ! In line comment
                             ////
        "start",             ////   count = start
        "incr",              ////   incr = 2.0
                             ////   DO WHILE  (count .lt. end)
        "say_hello",         ////     CALL say_hello( 'World' )
                             ////     IF (count /= 8) THEN
        "fmt",               ////       WRITE(*, fmt="(F8.0)") count
                             ////     END IF
        "incr",              ////     count = count + incr
                             ////   END DO
                             ////
                             ////   CONTAINS
                             ////
        "say_hello", "who",  ////   SUBROUTINE say_hello(who)
        "in", "who",         ////     CHARACTER(LEN=*), INTENT(in) :: who
                             ////
        "who",               ////     PRINT *, 'Hello ', who
        "say_hello",         ////   END SUBROUTINE say_hello
                             ////
        "hello",             //// END PROGRAM hello
                             ////
    };
    assertAnalyzesTo(analyzer, input, output);
  }
}
