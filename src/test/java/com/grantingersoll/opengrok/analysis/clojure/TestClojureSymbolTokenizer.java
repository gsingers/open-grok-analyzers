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

package com.grantingersoll.opengrok.analysis.clojure;


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

public class TestClojureSymbolTokenizer extends BaseTokenStreamTestCase {
  private Analyzer analyzer;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    analyzer = new Analyzer() {
      @Override
      protected TokenStreamComponents createComponents(String fieldName) {
        Tokenizer tokenizer = new ClojureSymbolTokenizer(newAttributeFactory());
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
    try (InputStream stream = TestClojureSymbolTokenizer.class.getResourceAsStream("main.clj");
         Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      input = IOUtils.toString(in);
    }
    String[] output = new String[] {
                                                             //// ;;;; From https://github.com/clojure/clojure/blob/master/test/clojure/test_clojure/main.clj
                                                             ////
                                                             //// ;   Copyright (c) Rich Hickey. All rights reserved.
                                                             //// ;   The use and distribution terms for this software are covered by the
                                                             //// ;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
                                                             //// ;   which can be found in the file epl-v10.html at the root of this distribution.
                                                             //// ;   By using this software in any fashion, you are agreeing to be bound by
                                                             //// ;   the terms of this license.
                                                             //// ;   You must not remove this notice, or any other, from this software.
                                                             ////
                                                             //// ; Author: Stuart Halloway
                                                             ////
                                                             ////
        "clojure.test-clojure.main",                         //// (ns clojure.test-clojure.main
        ":use", "clojure.test",                              ////   (:use clojure.test
        "clojure.test-helper", ":only", "platform-newlines", ////         [clojure.test-helper :only [platform-newlines]])
        ":require", "clojure.main", ":as", "main",           ////   (:require [clojure.main :as main]))
                                                             ////
        "deftest", "eval-opt",                               //// (deftest eval-opt
        "testing",                                           ////   (testing "evals and prints forms"
        "is", "platform-newlines", "clojure.main/eval-opt",  ////     (is (= (platform-newlines "2\n4\n") (with-out-str (#'clojure.main/eval-opt "(+ 1 1) (+ 2 2)")))))
                                                             ////
        "testing",                                           ////   (testing "skips printing nils"
        "is", "platform-newlines", "clojure.main/eval-opt",  ////     (is (= (platform-newlines ":a\n:c\n") (with-out-str (#'clojure.main/eval-opt ":a nil :c")))))
                                                             ////
        "testing",                                           ////   (testing "does not block access to *in* (#299)"
                                                             ////     (with-in-str "(+ 1 1)"
        "is", "platform-newlines", "clojure.main/eval-opt",  ////       (is (= (platform-newlines "(+ 1 1)\n") (with-out-str (#'clojure.main/eval-opt "(read)")))))))
                                                             ////
        "with-err-str",                                      //// (defmacro with-err-str
                                                             ////   "Evaluates exprs in a context in which *err* is bound to a fresh
                                                             ////   StringWriter.  Returns the string created by any nested printing
                                                             ////   calls."
        "&", "body",                                         ////   [& body]
        "s", "java.io.StringWriter",                         ////   `(let [s# (new java.io.StringWriter)
        "p", "java.io.PrintWriter", "s",                     ////          p# (new java.io.PrintWriter s#)]
        "p",                                                 ////      (binding [*err* p#]
        "@body",                                             ////        ~@body
        "s",                                                 ////        (str s#))))
                                                             ////
        "run-repl-and-return-err",                           //// (defn run-repl-and-return-err
                                                             ////   "Run repl, swallowing stdout and returing stderr."
        "in-str",                                            ////   [in-str]
        "with-err-str",                                      ////   (with-err-str
                                                             ////     (with-out-str
        "in-str",                                            ////       (with-in-str in-str
        "main/repl",                                         ////         (main/repl)))))
                                                             ////
                                                             //// ;argh - test fragility, please fix
        "_", "deftest", "repl-exception-safety",             //// #_(deftest repl-exception-safety
        "testing",                                           ////   (testing "catches and prints exception on bad equals"
        "is",                                                ////     (is (re-matches #"java\.lang\.NullPointerException\r?\n"
        "run-repl-and-return-err",                           ////            (run-repl-and-return-err
                                                             ////             "(proxy [Object] [] (equals [o] (.toString nil)))")))))
    };
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void testMimeType() {
    JFlexTokenizer tokenizer = new ClojureSymbolTokenizer(newAttributeFactory());
    assertEquals("text/x-clojure", tokenizer.getMimeType());
  }

  @Test
  public void testSourceCodeLanguage() {
    JFlexTokenizer tokenizer = new ClojureSymbolTokenizer(newAttributeFactory());
    assertEquals("Clojure", tokenizer.getSourceCodeLanguage());
  }
}
