package org.istanbus.core.lucene;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.tokenattributes.TermAttribute;
import org.apache.lucene.util.Version;

import java.io.StringReader;

/**
 * Created with IntelliJ IDEA.
 * User: mustafa
 */
public class PrintToken
{
    public static void main(String[] args) throws Exception
    {
        Analyzer a = new CustomAnalyzer(Version.LUCENE_36);
        TokenStream stream = a.reusableTokenStream("body", new
                StringReader("ÜSKÜDAR CAMİ ÖNÜ"));
        TermAttribute attr = stream.addAttribute(TermAttribute.class);
        while(stream.incrementToken())
            System.out.println(attr.term());
    }
}
