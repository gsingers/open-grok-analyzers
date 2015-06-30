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
package com.grantingersoll.opengrok.analysis.uue;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

import com.grantingersoll.opengrok.analysis.Definitions;
import com.grantingersoll.opengrok.analysis.StreamSource;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.TextField;
import com.grantingersoll.opengrok.analysis.FileAnalyzerFactory;
import com.grantingersoll.opengrok.analysis.TextAnalyzer;
import com.grantingersoll.opengrok.configuration.Project;
import com.grantingersoll.opengrok.history.Annotation;

/**
 * Analyzes [tn]roff files
 * Created on September 30, 2005
 *
 * @author Chandan
 */
public class UuencodeAnalyzer extends TextAnalyzer {
    private UuencodeXref xref;
    /**
     * Creates a new instance of UuencodeAnalyzer
     */
    protected UuencodeAnalyzer(FileAnalyzerFactory factory) {
        super(factory);
    }

    @Override
    public void analyze(Document doc, StreamSource src, Writer xrefOut) throws IOException {
        doc.add(new TextField("full", getReader(src.getStream())));

        if (xrefOut != null) {
            try (Reader in = getReader(src.getStream())) {
                writeXref(in, xrefOut);
            }
        }
    }

    @Override
    public TokenStreamComponents createComponents(String fieldName) {        
        if ("full".equals(fieldName)) {
            return new TokenStreamComponents(new UuencodeFullTokenizer());
        }
        return super.createComponents(fieldName);
    }

    /**
     * Write a cross referenced HTML file.
     *
     * @param in Input source
     * @param out Writer to write HTML cross-reference
     */
    private void writeXref(Reader in, Writer out) throws IOException {
        if (xref == null) {
            xref = new UuencodeXref(in);
        } else {
            xref.reInit(in);
        }
        xref.project = project;
        xref.write(out);
    }

    /**
     * Write a cross referenced HTML file reads the source from in
     *
     * @param in Input source
     * @param out Output xref writer
     * @param defs definitions for the file (could be null)
     * @param annotation annotation for the file (could be null)
     */
    static void writeXref(Reader in, Writer out, Definitions defs, Annotation annotation, Project project) throws IOException {
        UuencodeXref xref = new UuencodeXref(in);
        xref.annotation = annotation;
        xref.project = project;
        xref.setDefs(defs);
        xref.write(out);
    }
}
