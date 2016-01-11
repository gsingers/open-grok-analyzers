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

package com.grantingersoll.opengrok.analysis.php;


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

public class TestPhpSymbolTokenizer extends BaseTokenStreamTestCase {
  private Analyzer analyzer;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    analyzer = new Analyzer() {
      @Override
      protected TokenStreamComponents createComponents(String fieldName) {
        Tokenizer tokenizer = new PhpSymbolTokenizer(newAttributeFactory());
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
    try (InputStream stream = TestPhpSymbolTokenizer.class.getResourceAsStream("my_php.inc");
         Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      input = IOUtils.toString(in);
    }
    String[] output = new String[] {
                                                                                                                                //// <?php
                                                                                                                                //// /**
                                                                                                                                ////  *
                                                                                                                                ////  * @file
                                                                                                                                ////  * @brief
                                                                                                                                ////  *
                                                                                                                                ////  * @responsible
                                                                                                                                ////  * @author
                                                                                                                                ////  * @version
                                                                                                                                ////  * @copyright
                                                                                                                                ////  */
                                                                                                                                ////
        "ConnectivityAssistant", "ManagerRpc",                                                                                  //// class ConnectivityAssistant extends ManagerRpc {
        "p_result",                                                                                                             //// 	public $p_result = '';
        "p_parameters",                                                                                                         //// 	protected $p_parameters = array(
                                                                                                                                //// 				'set' => array(
                                                                                                                                //// 					'config'  => 'array',
                                                                                                                                //// 					'revertTimeout' => 'integer'
                                                                                                                                //// 				)
                                                                                                                                //// 	);
                                                                                                                                ////
        "p_manager",                                                                                                            ////   protected $p_manager = 'myproduct::ConnectivityAssistant';
                                                                                                                                ////
        "set", "p_config", "p_revertTimeout",                                                                                   //// 	public function set($p_config, $p_revertTimeout) {
        "this", "p_handleResponse", "p_errors", "this", "p_call", "set", "p_result", "p_config", "p_revertTimeout", "p_result"  //// 		return $this->p_handleResponse(&$p_errors, $this->p_call()->set(&$p_result, $p_config, $p_revertTimeout), $p_result);
                                                                                                                                //// 	}
                                                                                                                                //// }; // Configuration
                                                                                                                                //// ?>
                                                                                                                                ////
    };
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void testMimeType() {
    SymbolTokenizer tokenizer = new PhpSymbolTokenizer(newAttributeFactory());
    assertEquals("text/x-php", tokenizer.getMimeType());
  }

  @Test
  public void testSourceCodeLanguage() {
    SymbolTokenizer tokenizer = new PhpSymbolTokenizer(newAttributeFactory());
    assertEquals("PHP", tokenizer.getSourceCodeLanguage());
  }
}
