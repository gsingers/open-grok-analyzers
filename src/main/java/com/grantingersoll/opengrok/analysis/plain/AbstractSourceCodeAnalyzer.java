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
 * Copyright (c) 2012, 2015 Oracle and/or its affiliates. All rights reserved.
 */
package com.grantingersoll.opengrok.analysis.plain;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

import com.grantingersoll.opengrok.analysis.Definitions;
import com.grantingersoll.opengrok.analysis.JFlexTokenizer;
import com.grantingersoll.opengrok.analysis.JFlexXref;
import com.grantingersoll.opengrok.analysis.StreamSource;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.document.Document;
import com.grantingersoll.opengrok.analysis.FileAnalyzerFactory;
import com.grantingersoll.opengrok.configuration.Project;
import com.grantingersoll.opengrok.history.Annotation;

/**
 *
 * @author Lubos Kosco
 */
public abstract class AbstractSourceCodeAnalyzer extends PlainAnalyzer {

    /**
     * Creates a new instance of abstract analyzer
     * @param factory for which analyzer to create this
     */
    protected AbstractSourceCodeAnalyzer(FileAnalyzerFactory factory) {
        super(factory);
    }
    
    /**
     * Create a symbol tokenizer for the language supported by this analyzer.
     * @param reader the data to tokenize
     * @return a symbol tokenizer
     */
    protected abstract JFlexTokenizer newSymbolTokenizer(Reader reader);

    /**
     * Create an xref for the language supported by this analyzer.
     * @param reader the data to produce xref for
     * @return an xref instance
     */
    @Override
    protected abstract JFlexXref newXref(Reader reader);

    @Override
    public void analyze(Document doc, StreamSource src, Writer xrefOut) throws IOException {
        super.analyze(doc, src, xrefOut);
    }

    Reader dummy=null;
    @Override
    public Analyzer.TokenStreamComponents createComponents(String fieldName) {        
        if ("refs".equals(fieldName)) {
            return new TokenStreamComponents(newSymbolTokenizer(dummy));
        }
        return super.createComponents(fieldName);
    }

    /**
     * Write a cross referenced HTML file reads the source from in
     *
     * @param lxref xrefer to be used
     * @param in Input source
     * @param out Output xref writer
     * @param defs definitions for the file (could be null)
     * @param annotation annotation for the file (could be null)
     * @param project project where this xref belongs to
     * @throws IOException when any I/O error occurs
     */
    static protected void writeXref(JFlexXref lxref, Reader in, Writer out, Definitions defs, Annotation annotation, Project project) throws IOException {
        if (lxref != null) {
            lxref.reInit(in);
            lxref.annotation = annotation;
            lxref.project = project;
            lxref.setDefs(defs);
            lxref.write(out);
        }
    }
}
