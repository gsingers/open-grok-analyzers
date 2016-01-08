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

package com.grantingersoll.opengrok.analysis.python;


import org.apache.commons.io.IOUtils;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.BaseTokenStreamTestCase;
import org.apache.lucene.analysis.Tokenizer;
import org.junit.Test;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;

public class TestPythonSymbolTokenizer extends BaseTokenStreamTestCase {
  private Analyzer analyzer;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    analyzer = new Analyzer() {
      @Override
      protected TokenStreamComponents createComponents(String fieldName) {
        Tokenizer tokenizer = new PythonSymbolTokenizer(newAttributeFactory());
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
    try (InputStream stream = TestPythonSymbolTokenizer.class.getResourceAsStream("main.py");
         Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      input = IOUtils.toString(in);
    }
    String[] output = new String[] {
                                                                 //// #!/usr/bin/env python
                                                                 //// #
                                                                 ////
                                                                 //// # testing comment
                                                                 ////
        "getopt",                                                //// import getopt
        "os",                                                    //// import os
                                                                 //// #import stat
        "string",                                                //// import string
        "sys",                                                   //// import sys
                                                                 ////
        "datetime",                                              //// import datetime
        "time",                                                  //// import time
        "math",                                                  //// import math
        "random",                                                //// import random
                                                                 ////
        "SCRIPTHOME", "os", "path", "dirname", "sys", "argv",    //// SCRIPTHOME=os.path.dirname(sys.argv[0])
                                                                 ////
        "version_string",                                        //// version_string = "0.4"
                                                                 ////
        "banner",                                                //// def banner():
                                                                 ////     print """This is a multi
                                                                 ////  string with mu
                                                                 ////  ltiple lines
                                                                 //// """,
                                                                 ////
                                                                 ////
        "version",                                               //// def version():
        "version_string",                                        ////     print "version %s " % version_string,
                                                                 ////
        "MyClass",                                               //// class MyClass:
                                                                 ////     """A simple example class"""
        "i",                                                     ////     i = 12345
        "x",                                                     ////     x = 0xdeadbeef
        "l", "L",  /* TODO: support all numeric formats! */      ////     l= 456L
        "z",                                                     ////     z=14*8
        "f",                                                     ////     f=12.38
        "i", "e", "j", /* TODO: support all numeric formats! */  ////     i=3.14e-10j
        "ef", "e", /* TODO: support all numeric formats! */      ////     ef=14e-13
        "f", "self",                                             ////     def f(self):
                                                                 ////         return 'hello world'
                                                                 ////
        "main",                                                  //// def main():
                                                                 ////
                                                                 ////     # supress RuntimeWarning for tempnam being insecure
                                                                 ////     #    warnings.filterwarnings( "ignore" )
                                                                 ////
                                                                 ////     # go ahead
        "x", "MyClass",                                          ////     x = MyClass()
                                                                 ////     print "hello world",
                                                                 ////
        "version",                                               ////     version()
                                                                 ////
                                                                 //// # -----------------------------------------------------
        "__name__",                                              //// if  __name__ == "__main__":
        "main",                                                  ////     main()
                                                                 ////
                                                                 ////
                                                                 ////
    };
    assertAnalyzesTo(analyzer, input, output);
  }
}
