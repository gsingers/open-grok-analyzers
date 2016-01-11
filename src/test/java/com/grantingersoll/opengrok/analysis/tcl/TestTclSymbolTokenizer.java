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

package com.grantingersoll.opengrok.analysis.tcl;


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

public class TestTclSymbolTokenizer extends BaseTokenStreamTestCase {
  private Analyzer analyzer;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    analyzer = new Analyzer() {
      @Override
      protected TokenStreamComponents createComponents(String fieldName) {
        Tokenizer tokenizer = new TclSymbolTokenizer(newAttributeFactory());
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
    try (InputStream stream = TestTclSymbolTokenizer.class.getResourceAsStream("test.tcl");
         Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      input = IOUtils.toString(in);
    }
    String[] output = new String[] {
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
                                                                            ////
        "printHelloWorld",                                                  //// proc printHelloWorld {} {
                                                                            ////    puts "Hello world"
                                                                            //// }
                                                                            ////
        "viewSource", "f",                                                  //// proc viewSource { f } {
        "filesVisited", "EB",                                               ////    global filesVisited EB
        "EB", "curFile", "f",                                               ////    set EB(curFile) $f
        "filesVisited", "f",                                                ////    lappend filesVisited $f
                                                                            ////
                                                                            ////    # change window title to show the current file
        "wt", "title", "eb",                                                ////    set wt [wm title .eb]
        "first", "wt",                                                      ////    if { [string first : $wt] != -1 } {
        "idx", "first", "wt",                                               //// 	set idx [string first : $wt]
        "base", "range", "wt", "idx",                                       //// 	set base [string range $wt 0 $idx]
        "wtn", "base", "f",                                                 //// 	set wtn [concat $base $f]
                                                                            //// 	 } else {
        "wtn", "wt", "f",                                                   //// 		  set wtn [concat ${wt}: $f]
                                                                            //// 	 }
        "title", "eb", "wtn",                                               ////     wm title .eb $wtn
        "eb", "f", "t", "config", "state", "normal",                        ////     .eb.f.t config -state normal
        "eb", "f", "t", "delete", "end",                                    ////     .eb.f.t delete 1.0 end
        "f", "in",                                                          ////     if [catch {open $f} in] {
        "eb", "f", "t", "insert", "end", "in",                              //// 	.eb.f.t insert end $in
                                                                            ////      } else {
        "eb", "f", "t", "insert", "end", "in",                              //// 	.eb.f.t insert end [read $in]
        "in",                                                               //// 	close $in
                                                                            ////      }
        "eb", "f", "t", "config", "state", "normal",                        ////     .eb.f.t config -state normal
        "eb", "buttons", "config", "command", "applySource", "f",           ////     .eb.buttons.apply config -command [list applySource $f]
                                                                            //// }
    };
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void testMimeType() {
    JFlexTokenizer tokenizer = new TclSymbolTokenizer(newAttributeFactory());
    assertEquals("text/x-tcl", tokenizer.getMimeType());
  }

  @Test
  public void testSourceCodeLanguage() {
    JFlexTokenizer tokenizer = new TclSymbolTokenizer(newAttributeFactory());
    assertEquals("Tcl", tokenizer.getSourceCodeLanguage());
  }
}
