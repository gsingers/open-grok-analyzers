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
 * Use is subject to license terms.
 */
package com.grantingersoll.opengrok.analysis;

import java.io.IOException;
import java.io.Writer;
import java.util.logging.Level;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.document.Document;

import com.grantingersoll.opengrok.analysis.plain.PlainFullTokenizer;
import com.grantingersoll.opengrok.analysis.plain.PlainSymbolTokenizer;
import com.grantingersoll.opengrok.configuration.Project;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Base class for all different File Analyzers
 *
 * An Analyzer for a filetype provides
 *<ol>
 * <li>the file extentions and magic numbers it analyzes</li>
 * <li>a lucene document listing the fields it can support</li>
 * <li>TokenStreams for each of the field it said requires tokenizing in 2</li>
 * <li>cross reference in HTML format</li>
 * <li>The type of file data, plain text etc</li>
 *</ol>
 *
 * Created on September 21, 2005
 *
 * @author Chandan
 */
public class FileAnalyzer extends Analyzer {
    private transient static Logger log = LoggerFactory.getLogger(com.grantingersoll.opengrok.analysis.FileAnalyzer.class);

    protected Project project;
    protected boolean scopesEnabled;
    private final com.grantingersoll.opengrok.analysis.FileAnalyzerFactory factory;

    /**
     * What kind of file is this?
     */
    public static enum Genre {
        /** xrefed - line numbered context */
        PLAIN("p"),
        /** xrefed - summarizer context */
        XREFABLE("x"),
        /** not xrefed - no context - used by diff/list */
        IMAGE("i"),
        /** not xrefed - no context */
        DATA("d"),
        /** not xrefed - summarizer context from original file */
        HTML("h")
        ;
        private final String typeName;
        private Genre(String typename) {
            this.typeName = typename;
        }

        /**
         * Get the type name value used to tag lucene documents.
         * @return a none-null string.
         */
        public String typeName() {
            return typeName;
        }

        /**
         * Get the Genre for the given type name.
         * @param typeName name to check
         * @return {@code null} if it doesn't match any genre, the genre otherwise.
         * @see #typeName()
         */
        public static Genre get(String typeName) {
            if (typeName == null) {
                return null;
            }
            for (Genre g : values()) {
                if (g.typeName.equals(typeName)) {
                    return g;
                }
            }
            return null;
        }
    }
    protected com.grantingersoll.opengrok.analysis.Ctags ctags;

    public void setCtags(com.grantingersoll.opengrok.analysis.Ctags ctags) {
        this.ctags = ctags;
    }

    public void setProject(Project project) {
        this.project = project;
    }
    
    public void setScopesEnabled(boolean scopesEnabled) {
        this.scopesEnabled = scopesEnabled;
    }

    /**
     * Get the factory which created this analyzer.
     * @return the {@code FileAnalyzerFactory} which created this analyzer
     */
    public final com.grantingersoll.opengrok.analysis.FileAnalyzerFactory getFactory() {
        return factory;
    }

    public Genre getGenre() {
        return factory.getGenre();
    }    

    /** Creates a new instance of FileAnalyzer
     * @param factory name of factory to be used 
     */
    public FileAnalyzer(com.grantingersoll.opengrok.analysis.FileAnalyzerFactory factory) {
        super(Analyzer.PER_FIELD_REUSE_STRATEGY);
        this.factory = factory;        
                        
    }
    
    /**
     * Returns the normalized name of the analyzer,
     * which should corresponds to the file type.
     * Example: The analyzer for the C language (CAnalyzer) would return “c”.
     * @return Normalized name of the analyzer.
     */
    public String getFileTypeName() {
        String name = this.getClass().getSimpleName().toLowerCase();
        String suffix = "analyzer";
        
        if (name.endsWith(suffix)) {
            return name.substring(0, name.length() - suffix.length());
        }
        
        return name.toLowerCase();
    }

    /**
     * Analyze the contents of a source file. This includes populating the
     * Lucene document with fields to add to the index, and writing the
     * cross-referenced data to the specified destination.
     * @param doc the Lucene document
     * @param src the input data source
     * @param xrefOut where to write the xref (may be {@code null})
     * @throws IOException if any I/O error
     */
    public void analyze(Document doc, StreamSource src, Writer xrefOut) throws IOException {
        // not used
    }
        
    @Override
    public TokenStreamComponents createComponents(String fieldName) {        
        switch (fieldName) {
            case "full":
                return new TokenStreamComponents(new PlainFullTokenizer());
            case "path":
            case "project":
                return new TokenStreamComponents(new com.grantingersoll.opengrok.analysis.PathTokenizer());
            case "hist":
                return new com.grantingersoll.opengrok.analysis.HistoryAnalyzer().createComponents(fieldName);
            case "refs":
            case "defs":
                return new TokenStreamComponents(new PlainSymbolTokenizer());
            default:
                log.warn(
                        "Have no analyzer for: {0}", fieldName);
                return null;
        }
    }
}
