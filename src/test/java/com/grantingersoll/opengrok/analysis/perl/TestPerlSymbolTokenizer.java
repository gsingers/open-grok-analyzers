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

package com.grantingersoll.opengrok.analysis.perl;


import org.apache.commons.io.IOUtils;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.BaseTokenStreamTestCase;
import org.apache.lucene.analysis.Tokenizer;
import org.junit.Test;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;

public class TestPerlSymbolTokenizer extends BaseTokenStreamTestCase {
  private Analyzer analyzer;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    analyzer = new Analyzer() {
      @Override
      protected TokenStreamComponents createComponents(String fieldName) {
        Tokenizer tokenizer = new PerlSymbolTokenizer(newAttributeFactory());
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
    try (InputStream stream = TestPerlSymbolTokenizer.class.getResourceAsStream("main.pl");
         Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      input = IOUtils.toString(in);
    }
    String[] output = new String[] {
                                                                                             //// #!/usr/bin/perl
        "DBI",                                                                               ////    use DBI;
                                                                                             ////
        "database",                                                                          ////    my $database='dbi:DB2:sample';
        "user",                                                                              ////    my $user='';
        "password",                                                                          ////    my $password='';
                                                                                             ////
        "dbh", "DBI", "database", "user", "password",                                        ////    my $dbh = DBI->connect($database, $user, $password)
                                                                                             ////       or die "Can't connect to $database: $DBI::errstr";
                                                                                             ////
        "sth", "dbh", "prepare",                                                             ////    my $sth = $dbh->prepare(
        "SELECT", "firstnme", "lastname",                                                    ////       q{ SELECT firstnme, lastname
        "FROM", "employee",                                                                  ////          FROM employee }
                                                                                             ////       )
                                                                                             ////       or die "Can't prepare statement: $DBI::errstr";
                                                                                             ////
        "rc", "sth", "execute",                                                              ////    my $rc = $sth->execute
                                                                                             ////       or die "Can't execute statement: $DBI::errstr";
                                                                                             ////
                                                                                             ////    print "Query will return $sth->{NUM_OF_FIELDS} fields.\n\n";
                                                                                             ////    print "$sth->{NAME}->[0]: $sth->{NAME}->[1]\n";
                                                                                             ////
        "firstnme", "lastname", "sth", "fetchrow",                                           ////    while (($firstnme, $lastname) = $sth->fetchrow()) {
                                                                                             ////       print "$firstnme: $lastname\n";
                                                                                             ////    }
                                                                                             ////
                                                                                             ////    # check for problems which may have terminated the fetch early
        "DBI", "errstr", "DBI",                                                              ////    warn $DBI::errstr if $DBI::err;
                                                                                             ////
        "sth", "finish",                                                                     ////    $sth->finish;
        "dbh", "disconnect",                                                                 ////    $dbh->disconnect;
                                                                                             ////
        "item", "snazzle", /* TODO: handle POD formatted commentary! */                      //// =item snazzle($)
                                                                                             ////
        "The", "snazzle", "function", "will", "behave", "in", "the", "most", "spectacular",  //// The snazzle() function will behave in the most spectacular
        "form", "that", "you", "can", "possibly", "imagine", "even", "excepting",            //// form that you can possibly imagine, not even excepting
        "cybernetic", "pyrotechnics",                                                        //// cybernetic pyrotechnics.
                                                                                             ////
        "cut", "back", "to", "the", "compiler", "nuff", "of", "this", "pod", "stuff",        //// =cut back to the compiler, nuff of this pod stuff!
                                                                                             ////
        "snazzle",                                                                           //// sub snazzle($) {
        "thingie",                                                                           //// my $thingie = shift;
                                                                                             //// }
                                                                                             ////
        "x",                                                                                 //// my x;
        "x",                                                                                 //// $x=12345              # integer
        "x",                                                                                 //// $x=-54321             # negative integer
        "x",                                                                                 //// $x=12345.67            # floating point
        "x", "E23", /* TODO: handle full numeric syntax! */                                  //// $x=6.02E23             # scientific notation
        "x",                                                                                 //// $x=0xffff              # hexadecimal
        "x",                                                                                 //// $x=0377                # octal
        "x", "_294_967_296" /* TODO: handle full numeric syntax! */                          //// $x=4_294_967_296       # underline for legibility
                                                                                             ////
                                                                                             ////
    };
    assertAnalyzesTo(analyzer, input, output);
  }
}
