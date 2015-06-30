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
package com.grantingersoll.opengrok.analysis.plain;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.Writer;

import com.grantingersoll.opengrok.analysis.StreamSource;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.StoredField;
import org.apache.lucene.document.TextField;
import com.grantingersoll.opengrok.analysis.Definitions;
import com.grantingersoll.opengrok.analysis.ExpandTabsReader;
import com.grantingersoll.opengrok.analysis.FileAnalyzerFactory;
import com.grantingersoll.opengrok.analysis.IteratorReader;
import com.grantingersoll.opengrok.analysis.JFlexScopeParser;
import com.grantingersoll.opengrok.analysis.JFlexXref;
import com.grantingersoll.opengrok.analysis.Scopes;
import com.grantingersoll.opengrok.analysis.TextAnalyzer;
import com.grantingersoll.opengrok.configuration.Project;
import com.grantingersoll.opengrok.history.Annotation;
import com.grantingersoll.opengrok.search.QueryBuilder;

/**
 * Analyzer for plain text files Created on September 21, 2005
 *
 * @author Chandan
 */
public class PlainAnalyzer extends TextAnalyzer {

    private JFlexXref xref;
    private Definitions defs;

    /**
     * Creates a new instance of PlainAnalyzer
     */
    protected PlainAnalyzer(FileAnalyzerFactory factory) {
        super(factory);
    }

    /**
     * Create an xref for the language supported by this analyzer.
     * @param reader the data to produce xref for
     * @return an xref instance
     */
    protected JFlexXref newXref(Reader reader) {
        return new PlainXref(reader);
    }

    @Override
    protected Reader getReader(InputStream stream) throws IOException {
        return ExpandTabsReader.wrap(super.getReader(stream), project);
    }
    
    /**
     * Create new scope parser for given file type. Default implementation is none.
     * @param reader
     * @return new instance of scope parser
     */
    protected JFlexScopeParser newScopeParser(Reader reader) {
        return null;
    }

    @Override
    public void analyze(Document doc, StreamSource src, Writer xrefOut) throws IOException {
        doc.add(new TextField(QueryBuilder.FULL, getReader(src.getStream())));
        String fullpath = doc.get(QueryBuilder.FULLPATH);
        if (fullpath != null && ctags != null) {
            defs = ctags.doCtags(fullpath + "\n");
            if (defs != null && defs.numberOfSymbols() > 0) {
                doc.add(new TextField(QueryBuilder.DEFS, new IteratorReader(defs.getSymbols())));
                doc.add(new TextField(QueryBuilder.REFS, getReader(src.getStream())));
                byte[] tags = defs.serialize();
                doc.add(new StoredField(QueryBuilder.TAGS, tags));
                
                /*
                 * Parse all scopes for file if we know how
                 */
                if (scopesEnabled) {
                    addScopes(doc, src);
                }
            }
        }

        if (xrefOut != null) {
            try (Reader in = getReader(src.getStream())) {
                writeXref(in, xrefOut);
            }
        }
    }
    
    private void addScopes(Document doc, StreamSource src) throws IOException {
        JFlexScopeParser scopeParser = newScopeParser(getReader(src.getStream()));
        if (scopeParser != null) {
            for (Definitions.Tag tag : defs.getTags()) {
                if (tag.type.startsWith("function") || tag.type.startsWith("method")) {
                    scopeParser.parse(tag, getReader(src.getStream()));
                }
            }

            Scopes scopes = scopeParser.getScopes();
            if (scopes.size() > 0) {
                byte[] scopesSerialized = scopes.serialize();
                doc.add(new StoredField(QueryBuilder.SCOPES, scopesSerialized));
            }
        }
    }

    /**
     * Write a cross referenced HTML file.
     *
     * @param in Input source
     * @param out Writer to write HTML cross-reference
     */
    private void writeXref(Reader in, Writer out) throws IOException {
        if (xref == null) {
            xref = newXref(in);
        } else {
            xref.reInit(in);
        }
        xref.setDefs(defs);
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
        PlainXref xref = new PlainXref(in);
        xref.annotation = annotation;
        xref.project = project;
        xref.setDefs(defs);
        xref.write(out);
    }
}
