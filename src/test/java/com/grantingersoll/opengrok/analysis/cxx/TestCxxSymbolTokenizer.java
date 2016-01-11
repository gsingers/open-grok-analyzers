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

package com.grantingersoll.opengrok.analysis.cxx;


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

public class TestCxxSymbolTokenizer extends BaseTokenStreamTestCase {
  private Analyzer analyzer;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    analyzer = new Analyzer() {
      @Override
      protected TokenStreamComponents createComponents(String fieldName) {
        Tokenizer tokenizer = new CxxSymbolTokenizer(newAttributeFactory());
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
    try (InputStream stream = TestCxxSymbolTokenizer.class.getResourceAsStream("sample.cxx");
         Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      input = IOUtils.toString(in);
    }
    String[] output = new String[] {
                                                                //// // compile me with g++
                                                                //// /* this is sample comment } */
        "string",                                               //// #include <string>
        "vector",                                               //// #include <vector>
        "iostream",                                             //// #include <iostream>
                                                                ////
        "TEST", "x", "x",                                       //// #define TEST(x) (x)
                                                                ////
        "SomeClass",                                            //// class SomeClass {
                                                                //// public:
        "SomeClass",                                            ////     SomeClass() /* I'm constructor */
        "attr_",                                                ////         : attr_(0)
                                                                ////     {
        "std","cout", "std", "endl",                            ////         std::cout << "Hello" << std::endl;
                                                                ////     }
                                                                ////
        "SomeClass",                                            ////     ~SomeClass() // destructor
                                                                ////     {
        "std", "cout", "std", "endl",                           ////         std::cout << "Bye" << std::endl;
                                                                ////     }
                                                                ////
        "MemberFunc", "a", "b",                                 ////     int MemberFunc(int a, int b) const {
                                                                ////         // some member function
        "a", "b",                                               ////         return a + b;
                                                                ////     }
                                                                ////
                                                                ////     int operator++(int) {
        "attr_",                                                ////         return attr_++;
                                                                ////     }
                                                                ////
        "T",                                                    ////     template<typename T>
        "size_t", "TemplateMember", "std", "vector", "T", "v",  ////     size_t TemplateMember(std::vector<T>& v) {
        "v", "size",                                            ////         return v.size();
                                                                ////     }
                                                                ////
                                                                //// private:
        "attr_",                                                ////     int attr_;
                                                                //// };
                                                                ////
        "ns1",                                                  //// namespace ns1 {
                                                                ////
        "NamespacedClass",                                      ////     class NamespacedClass {
                                                                ////     public:
        "SomeFunc", "std", "string", "arg",                     ////         static void SomeFunc(const std::string& arg) {
        "std", "cout", "arg",                                   ////             std::cout << arg;
                                                                ////         }
                                                                ////     };
                                                                ////
        "ns2",                                                  ////     namespace ns2 {
                                                                ////
        "foo", "a", "b",                                        ////         int foo(int a, int b) {
        "SomeClass", "t",                                       ////             SomeClass t;
        "t", "MemberFunc", "TEST", "a", "TEST", "b",            ////             return t.MemberFunc(TEST(a), TEST(b));
                                                                ////         }
                                                                ////
                                                                ////     }
                                                                //// }
                                                                ////
        "bar", "x",                                             //// int bar(int x /* } */)
                                                                //// {
                                                                ////     // another function
        "d",                                                    ////     int d;
        "f",                                                    ////     int f;
        "std", "cout", "TEST", "std", "endl",                   ////     std::cout << TEST("test { message|$#@$!!#") << std::endl;
        "d", "foo",                                             ////     d = foo(2, 4);
        "f", "foo", "x", "d",                                   ////     f = foo(x, d);
                                                                ////
                                                                ////     /* return
                                                                ////         some
                                                                ////          rubish
                                                                ////     */
        "d", "f",                                               ////     return d+f;
                                                                //// }
                                                                ////
                                                                //// // main function
        "main", "argc", "argv",                                 //// int main(int argc, char *argv[]) {
        "SomeClass", "c",                                       ////     SomeClass c;
        "res",                                                  ////     int res;
        "std", "cout", "std", "endl",                           ////     std::cout << "this is just a {sample}}" << std::endl;
                                                                ////
        "res", "bar",                                           ////     res = bar(20);
        "std", "cout", "res", "std", "endl",                    ////     std::cout << "result = {" << res << "}" << std::endl;
                                                                ////
        "std", "cout", "c", "MemberFunc", "std", "endl",        ////     std::cout << c.MemberFunc(1, 2) << std::endl;
        "std", "cout", "c", "std", "endl",                      ////     std::cout << c++ << std::endl;
                                                                ////
                                                                ////     return 0; }
                                                                ////
    };
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void testMimeType() {
    SymbolTokenizer tokenizer = new CxxSymbolTokenizer(newAttributeFactory());
    assertEquals("text/x-c++src", tokenizer.getMimeType());
  }

  @Test
  public void testSourceCodeLanguage() {
    SymbolTokenizer tokenizer = new CxxSymbolTokenizer(newAttributeFactory());
    assertEquals("C++", tokenizer.getSourceCodeLanguage());
  }
}
