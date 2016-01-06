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

/*
 * Copyright 2010 Sun Microsystems, Inc.  All rights reserved.
 * Use is subject to license terms.
 */

package com.grantingersoll.opengrok.analysis;

import com.grantingersoll.opengrok.analysis.cxx.CxxSymbolTokenizerFactory;
import com.grantingersoll.opengrok.analysis.php.PhpSymbolTokenizer;
import com.grantingersoll.opengrok.analysis.plain.PlainSymbolTokenizer;
import com.grantingersoll.opengrok.analysis.sh.ShSymbolTokenizer;
import com.grantingersoll.opengrok.analysis.sh.ShSymbolTokenizerFactory;
import org.apache.lucene.analysis.Tokenizer;
import org.apache.lucene.analysis.util.TokenizerFactory;
import org.junit.Test;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;

public class TokenizerGuruTest {

  /**
   * Test that we get the correct analyzer if the file name exactly matches a
   * known extension.
   */
  @Test
  public void testFileNameSameAsExtension() throws Exception {
    Reader in = new StringReader("#!/bin/sh\nexec /usr/bin/zip \"$@\"\n");
    String filename = "/dummy/path/to/source/zip";
    Tokenizer tokenizer = TokenizerGuru.getTokenizer(in, filename);
    assertSame(ShSymbolTokenizer.class, tokenizer.getClass());
  }

  @Test
  public void testUTF8ByteOrderMark() throws Exception {
    byte[] xml = {(byte) 0xEF, (byte) 0xBB, (byte) 0xBF, // UTF-8 BOM
        '#', '!', '/', 'b', 'i', 'n', '/', 'p','h','p'};
    ByteArrayInputStream instream = new ByteArrayInputStream(xml);
    Reader in = new BufferedReader(new InputStreamReader(instream, StandardCharsets.UTF_8));
    Tokenizer tokenizer = TokenizerGuru.getTokenizer(in, "/dummy/file");
    assertSame(PhpSymbolTokenizer.class, tokenizer.getClass());
  }

  @Test
  public void testUTF8ByteOrderMarkPlainFile() throws Exception {
    byte[] bytes = {(byte) 0xEF, (byte) 0xBB, (byte) 0xBF, // UTF-8 BOM
        'h', 'e', 'l', 'l', 'o', ' ',
        'w', 'o', 'r', 'l', 'd'};

    ByteArrayInputStream instream = new ByteArrayInputStream(bytes);
    Reader in = new InputStreamReader(instream, StandardCharsets.UTF_8);
    Tokenizer tokenizer = TokenizerGuru.getTokenizer(in, "/dummy/file");
    assertSame(PlainSymbolTokenizer.class, tokenizer.getClass());
  }

  @Test
  public void testPlainText() throws IOException {
    Reader in = new StringReader("This is a plain text file.");
    assertSame(PlainSymbolTokenizer.class, TokenizerGuru.getTokenizer(in, "dummy").getClass());
  }

  @Test
  public void testHxxExtension() {
    TokenizerFactory factory = TokenizerGuru.find("foo.hxx");
    assertNotNull(factory);
    assertSame(CxxSymbolTokenizerFactory.class, factory.getClass());
  }

  @Test
  public void testOneExtensionIsThePrefixOfAnother() {
    TokenizerFactory factory1 = TokenizerGuru.find("main.c");
    assertNotNull(factory1);

    TokenizerFactory factory2 = TokenizerGuru.find("main.cc");
    assertNotNull(factory2);

    assertNotSame(factory1.getClass(), factory2.getClass());
  }

  @Test
  public void matchesFullName() {
    TokenizerFactory factory = TokenizerGuru.find("/path/to/Makefile");
    assertSame(ShSymbolTokenizerFactory.class, factory.getClass());

    factory = TokenizerGuru.find("GNUMakefile");
    assertSame(ShSymbolTokenizerFactory.class, factory.getClass());
  }
}
