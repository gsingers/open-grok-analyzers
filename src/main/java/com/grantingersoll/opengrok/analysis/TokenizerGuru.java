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
 * Copyright (c) 2005, 2015, Oracle and/or its affiliates. All rights reserved.
 */
package com.grantingersoll.opengrok.analysis;

import com.grantingersoll.opengrok.analysis.plain.PlainSymbolTokenizerFactory;
import org.apache.lucene.analysis.Tokenizer;
import org.apache.lucene.analysis.util.ClasspathResourceLoader;
import org.apache.lucene.analysis.util.ResourceLoaderAware;
import org.apache.lucene.analysis.util.TokenizerFactory;

import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Finds code symbol tokenizers appropriate for given file names / prefixes / extensions / magics (content prefixes).
 */
public class TokenizerGuru {
  /** Tokenizer factory to use when there is no file name/prefix/extension/magics match. */
  private static final TokenizerFactory DEFAULT_TOKENIZER_FACTORY
      = new PlainSymbolTokenizerFactory(Collections.<String,String>emptyMap());

  /** Used in regex alternations for prefix matching */
  private static final Comparator<String> LONGEST_FIRST_SORT = new Comparator<String>() {
    @Override public int compare(String o1, String o2) {
      return new Integer(o2.length()).compareTo(o1.length());
    }};

  /** Maps full filenames to tokenizer factories. */
  private static final Map<String,TokenizerFactory> FILE_NAMES = new HashMap<>();

  /** Maps filename extensions to tokenizer factories. */
  private static final Map<String,TokenizerFactory> FILE_EXTENSIONS = new HashMap<>();

  /** Maps filename prefixes, sorted longest first, to tokenizer factories. */
  private static final SortedMap<String,TokenizerFactory> FILE_PREFIXES
      = new TreeMap<>(LONGEST_FIRST_SORT);

  /** Maps magic strings (content prefixes), sorted longest first, to tokenizer factories. */
  private static final SortedMap<String,TokenizerFactory> FILE_MAGICS
      = new TreeMap<>(LONGEST_FIRST_SORT);

  /** Regex alternation of all filename prefixes, longest first */
  private static final Pattern FILE_PREFIX_PATTERN;

  /** Regex alternation of all file magics (content prefixes), longest first */
  private static final Pattern FILE_MAGIC_PATTERN;

  /** Maximum length of file magic (content prefix) */
  private static final int MAX_FILE_MAGIC_LENGTH;

  /** Minimum length of file magic (content prefix) */
  private static final int MIN_FILE_MAGIC_LENGTH;

  /** Char length of a UTF-8 encoded BOM */
  private static final char UTF8_BOM = '\uFEFF';

  static {
    try {
      populateTokenizerFactoryMap(FILE_NAMES, "TokenizerFileNames.properties");
      populateTokenizerFactoryMap(FILE_EXTENSIONS, "TokenizerFileExtensions.properties");

      String filePrefixRegex = populateTokenizerFactoryMap(FILE_PREFIXES, "TokenizerFilePrefixes.properties");
      FILE_PREFIX_PATTERN = Pattern.compile(filePrefixRegex, Pattern.CASE_INSENSITIVE);

      String fileMagicsRegex = populateTokenizerFactoryMap(FILE_MAGICS, "TokenizerFileMagics.properties");
      FILE_MAGIC_PATTERN = Pattern.compile(fileMagicsRegex);

      MAX_FILE_MAGIC_LENGTH = FILE_MAGICS.firstKey().length(); // First is longest
      MIN_FILE_MAGIC_LENGTH = FILE_MAGICS.lastKey().length(); // Last is shortest
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  private static ClasspathResourceLoader CLASSPATH_RESOURCE_LOADER
      = new ClasspathResourceLoader(TokenizerGuru.class.getClassLoader());
  private static Map<String,TokenizerFactory> FACTORY_SINGLETONS = new HashMap<>();

  /**
   * Reads in the given properties file, where keys are tokenizer SPI names and values are
   * space-and-or-comma delimited, and adds each list value as a key, with an instantiated
   * tokenizer for the given SPI name as its value.
   *
   * If the map is sorted, returns a regex alternation of all map keys, otherwise null.
   */
  private static String populateTokenizerFactoryMap
  (Map<String,TokenizerFactory> map, String propertiesFile) throws IOException {
    for (String key : map.keySet()) {
      Properties values = new Properties();
      values.load(TokenizerGuru.class.getResourceAsStream(propertiesFile));
      for (Map.Entry<Object,Object> entry : values.entrySet()) {
        String tokenizerSPIname = (String) entry.getKey();
        String valueList = (String) entry.getValue();
        TokenizerFactory factory = FACTORY_SINGLETONS.get(tokenizerSPIname);
        if (factory == null) {
          factory = TokenizerFactory.forName
              (tokenizerSPIname, Collections.<String, String>emptyMap());
          ((ResourceLoaderAware) factory).inform(CLASSPATH_RESOURCE_LOADER);
          FACTORY_SINGLETONS.put(tokenizerSPIname, factory);
        }
        // Ignore backslash-escaped value list separators
        for (String value : valueList.trim().split("(?<!\\\\)(?:\\s*,\\s*|\\s+)")) {
          value = value.replaceAll("\\A\\s+|(?<!\\\\)\\s+\\z", ""); // trim non-escaped prefix/suffix whitespace
          value = value.replaceAll("\\\\(.)", "$1"); // unescape backslash-escaped chars
          map.put(value, factory);
        }
      }
    }
    // The regex is only needed for prefix matching, in which case the map keys are sorted longest-first
    String regex = null;
    if (map instanceof SortedMap) {
      StringBuilder regexBuilder = new StringBuilder();
      for (String key : map.keySet()) {
        if (regexBuilder.length() > 0) {
          regexBuilder.append("|");
        }
        regexBuilder.append(Pattern.quote(key));
      }
      regex = regexBuilder.toString();
    }
    return regex;
  }

  /**
   * Get a tokenizer suited to analyze a file
   *
   * @param in Reader containing data to be analyzed
   * @param filename Name of the file to be analyzed
   * @return A tokenizer suited for that file content
   */
  public static Tokenizer getTokenizer(Reader in, String filename) throws IOException {
    TokenizerFactory factory = find(in, filename);
    if (factory == null) {
      factory = DEFAULT_TOKENIZER_FACTORY;
    }
    return factory.create();
  }

  /**
   * Finds a suitable tokenizer class for filename. If the tokenizer cannot be
   * determined by the file name/prefix/extension, try to look at the data in the
   * Reader to find a suitable tokenizer.
   *
   * Use if you just want to find file type.
   *
   * @param in The Reader containing the data
   * @param filename The filename to get the tokenizer factory for
   * @return the tokenizer factory to use
   */
  public static TokenizerFactory find(Reader in, String filename) throws IOException {
    TokenizerFactory factory = find(filename);
    // TODO above is not that great, since if 2 analyzers share one extension
    // then only the first one registered will own it
    // it would be cool if above could return more analyzers and below would
    // then decide between them ...
    if (factory != null) {
      return factory;
    }
    return find(in);
  }

  /**
   * Finds a suitable tokenizer factory for filename.
   *
   * @param filename The file name to get the tokenizer factory for
   * @return the tokenizer factory to use
   */
  public static TokenizerFactory find(String filename) {
    TokenizerFactory factory = null;
    String uppercaseBasename = new File(filename).getName().toUpperCase(Locale.ROOT);
    int lastDotPos = uppercaseBasename.lastIndexOf('.');

    // Try matching the prefix.
    Matcher matcher = FILE_PREFIX_PATTERN.matcher(uppercaseBasename);
    if (matcher.lookingAt()) {
      factory = FILE_PREFIXES.get(uppercaseBasename);
    }

    if (factory == null) {
      if (lastDotPos != -1) { // if the filename has an extension
        // Now try matching the extension. We kind of consider this order (first
        // prefix then extension) to be workable although for sure there can be
        // cases when this does not work.
        factory = FILE_EXTENSIONS.get(uppercaseBasename.substring(lastDotPos + 1));
      }
      if (factory == null) { // filename doesn't have any of the prefix or extensions we know
        // try full match
        factory = FILE_NAMES.get(uppercaseBasename);
      }
    }
    return factory;
  }

  /**
   * Finds a suitable tokenizer factory for the data in this reader, which will be reset back
   * to its initial position after checking for known file magics (content prefixes)
   *
   * @param in The reader containing the data to tokenize
   * @return the tokenizer factory to use
   * @throws java.io.IOException if an error occurs while reading data from the reader
   */
  public static TokenizerFactory find(Reader in) throws IOException {
    TokenizerFactory factory = null;

    in.mark(MAX_FILE_MAGIC_LENGTH + 1); // Add one for UTF-8 BOM
    char[] content = new char[MAX_FILE_MAGIC_LENGTH + 1]; // Add one for UTF-8 BOM
    int len = in.read(content);
    in.reset();

    // Ignore the UTF-8 BOM, if any
    int bomSkipOffset = content[0] == UTF8_BOM ? 1 : 0;
    String contentPrefix = new String(content, bomSkipOffset, len - bomSkipOffset);
    if (contentPrefix.length() >= MIN_FILE_MAGIC_LENGTH) {
      factory = findMagic(contentPrefix);
    }
    return factory;
  }

  /**
   * Finds a suitable tokenizer for the given content prefix
   *
   * @param contentPrefix known magic signatures will be matched against the content prefix
   * @return the tokenizer factory to use
   */
  private static TokenizerFactory findMagic(String contentPrefix) {
    TokenizerFactory factory = null;
    Matcher matcher = FILE_MAGIC_PATTERN.matcher(contentPrefix);
    if (matcher.lookingAt()) {
      factory = FILE_MAGICS.get(matcher.group());
    }
    return factory;
  }
}
