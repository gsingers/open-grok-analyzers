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

package com.grantingersoll.opengrok.analysis.vb;


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

public class TestVBSymbolTokenizer extends BaseTokenStreamTestCase {
  private Analyzer analyzer;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    analyzer = new Analyzer() {
      @Override
      protected TokenStreamComponents createComponents(String fieldName) {
        Tokenizer tokenizer = new VBSymbolTokenizer(newAttributeFactory());
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
  public void testNumericLiterals() throws Exception {
    String input = "&HFFFF + &o777";
    String[] output = new String[] {}; // zero output tokens
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void test() throws Exception {
    String input;
    try (InputStream stream = TestVBSymbolTokenizer.class.getResourceAsStream("VBP_pngnqInterface.bas");
         Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      input = IOUtils.toString(in);
    }
    String[] output = new String[] {
                                                                                             //// ''''From https://github.com/tannerhelland/PhotoDemon/blob/master/Modules/VBP_pngnqInterface.bas
                                                                                             ////
        "Attribute", "VB_Name",                                                              //// Attribute VB_Name = "Plugin_PNGQuant_Interface"
                                                                                             //// '***************************************************************************
                                                                                             //// 'PNGQuant Interface (formerly pngnq-s9 interface)
                                                                                             //// 'Copyright 2012-2015 by Tanner Helland
                                                                                             //// 'Created: 19/December/12
                                                                                             //// 'Last updated: 02/July/14
                                                                                             //// 'Last update: migrate all plugin support to the official pngquant library.  Work on pngnq-s9 has pretty much
                                                                                             //// '              evaporated since late 2012, so pngquant is the new workhorse for PD's specialized PNG needs.
                                                                                             //// '
                                                                                             //// 'Module for handling all PNGQuant interfacing.  This module is pointless without the accompanying
                                                                                             //// ' PNGQuant plugin, which will be in the App/PhotoDemon/Plugins subdirectory as "pngquant.exe"
                                                                                             //// '
                                                                                             //// 'PNGQuant is a free, open-source lossy PNG compression library.  You can learn more about it here:
                                                                                             //// '
                                                                                             //// ' http://pngquant.org/
                                                                                             //// '
                                                                                             //// 'PhotoDemon has been designed against v2.1.1 (02 July '14).  It may not work with other versions.
                                                                                             //// ' Additional documentation regarding the use of PNGQuant is available as part of the official PNGQuant library,
                                                                                             //// ' downloadable from http://pngquant.org/.
                                                                                             //// '
                                                                                             //// 'PNGQuant is available under a BSD license.  Please see the App/PhotoDemon/Plugins/pngquant-README.txt file
                                                                                             //// ' for questions regarding copyright or licensing.
                                                                                             //// '
                                                                                             //// 'All source code in this file is licensed under a modified BSD license.  This means you may use the code in your own
                                                                                             //// ' projects IF you provide attribution.  For more information, please visit http://photodemon.org/about/license/
                                                                                             //// '
                                                                                             //// '***************************************************************************
                                                                                             ////
        "Explicit",                                                                          //// Option Explicit
                                                                                             ////
                                                                                             //// 'Is PNGQuant.exe available on this PC?
        "isPngQuantAvailable",                                                               //// Public Function isPngQuantAvailable() As Boolean
                                                                                             ////
        "cFile", "pdFSO",                                                                    ////     Dim cFile As pdFSO
        "cFile", "pdFSO",                                                                    ////     Set cFile = New pdFSO
                                                                                             ////
        "cFile", "FileExist", "g_PluginPath", "isPngQuantAvailable", "isPngQuantAvailable",  ////     If cFile.FileExist(g_PluginPath & "pngquant.exe") Then isPngQuantAvailable = True Else isPngQuantAvailable = False
                                                                                             ////
                                                                                             //// End Function
                                                                                             ////
                                                                                             //// 'Retrieve the PNGQuant plugin version.  Shelling the executable with the "--version" tag will cause it to return
                                                                                             //// ' the current version (and compile date) over stdout.
        "getPngQuantVersion",                                                                //// Public Function getPngQuantVersion() As String
                                                                                             ////
        "isPngQuantAvailable",                                                               ////     If Not isPngQuantAvailable Then
        "getPngQuantVersion",                                                                ////         getPngQuantVersion = ""
                                                                                             ////         Exit Function
                                                                                             ////
                                                                                             ////     Else
                                                                                             ////
        "pngqPath",                                                                          ////         Dim pngqPath As String
        "pngqPath", "g_PluginPath",                                                          ////         pngqPath = g_PluginPath & "pngquant.exe"
                                                                                             ////
        "outputString",                                                                      ////         Dim outputString As String
        "ShellExecuteCapture", "pngqPath", "outputString",                                   ////         If ShellExecuteCapture(pngqPath, "pngquant.exe --version", outputString) Then
                                                                                             ////
                                                                                             ////             'The output string will be a simple version number and release date, e.g. "2.1.1 (February 2014)".
                                                                                             ////             ' Split the output by spaces, then retrieve the first entry.
        "outputString", "Trim", "outputString",                                              ////             outputString = Trim$(outputString)
                                                                                             ////
        "versionParts",                                                                      ////             Dim versionParts() As String
        "versionParts", "Split", "outputString",                                             ////             versionParts = Split(outputString, " ")
        "getPngQuantVersion", "versionParts",                                                ////             getPngQuantVersion = versionParts(0) & ".0"
                                                                                             ////
                                                                                             ////         Else
        "getPngQuantVersion",                                                                ////             getPngQuantVersion = ""
                                                                                             ////         End If
                                                                                             ////
                                                                                             ////     End If
                                                                                             ////
                                                                                             //// End Function
                                                                                             ////
    };
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void testMimeType() {
    SymbolTokenizer tokenizer = new VBSymbolTokenizer(newAttributeFactory());
    assertEquals("text/x-vbasic", tokenizer.getMimeType());
  }

  @Test
  public void testSourceCodeLanguage() {
    SymbolTokenizer tokenizer = new VBSymbolTokenizer(newAttributeFactory());
    assertEquals("Visual Basic", tokenizer.getSourceCodeLanguage());
  }
}
