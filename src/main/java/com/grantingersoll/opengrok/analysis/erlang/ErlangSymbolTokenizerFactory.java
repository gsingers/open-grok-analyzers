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

package com.grantingersoll.opengrok.analysis.erlang;

import com.grantingersoll.opengrok.analysis.SymbolTokenizerFactory;
import org.apache.lucene.util.AttributeFactory;

import java.util.Map;

/**
 * Factory for {@link ErlangSymbolTokenizer}.
 * <pre class="prettyprint">
 * &lt;fieldType name="Erlang_lang" class="solr.TextField" positionIncrementGap="100"&gt;
 *   &lt;analyzer&gt;
 *     &lt;tokenizer class="solr.ErlangSymbolTokenizerFactory"/&gt;
 *   &lt;/analyzer&gt;
 * &lt;/fieldType&gt;</pre>
 */
public class ErlangSymbolTokenizerFactory extends SymbolTokenizerFactory {

  /** Creates a new ErlangSymbolTokenizerFactory */
  public ErlangSymbolTokenizerFactory(Map<String,String> args) {
    super(args);
    if (!args.isEmpty()) {
      throw new IllegalArgumentException("Unknown parameters: " + args);
    }
  }

  @Override
  public ErlangSymbolTokenizer create(AttributeFactory factory) {
    return new ErlangSymbolTokenizer(factory);
  }

  @Override
  public String getMimeType() {
    return Consts.MIME_TYPE;
  }

  @Override
  public String getSourceCodeLanguage() {
    return Consts.SOURCE_CODE_LANGUAGE;
  }
}
