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

package com.grantingersoll.opengrok.analysis;


import org.apache.lucene.analysis.util.TokenizerFactory;

import java.util.Map;

public abstract class SymbolTokenizerFactory extends TokenizerFactory {

  /** Initialize this factory via a set of key-value pairs. */
  protected SymbolTokenizerFactory(Map<String, String> args) {
    super(args);
  }

  /** Returns MIME type for the source code tokenized by tokenizers created with this factory. */
  abstract public String getMimeType();

  /** Returns the name of the language of the source code tokenized by tokenizers created with this factory. */
  abstract public String getSourceCodeLanguage();
}
