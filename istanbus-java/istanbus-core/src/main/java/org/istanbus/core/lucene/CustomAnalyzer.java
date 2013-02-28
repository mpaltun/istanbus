package org.istanbus.core.lucene;

import org.apache.lucene.analysis.ASCIIFoldingFilter;
import org.apache.lucene.analysis.LowerCaseFilter;
import org.apache.lucene.analysis.ReusableAnalyzerBase;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.Tokenizer;
import org.apache.lucene.analysis.WhitespaceTokenizer;
import org.apache.lucene.analysis.ngram.EdgeNGramTokenFilter;
import org.apache.lucene.analysis.ngram.EdgeNGramTokenFilter.Side;
import org.apache.lucene.analysis.standard.StandardFilter;
import org.apache.lucene.util.Version;

import java.io.Reader;

/**
 * Created with IntelliJ IDEA.
 * User: mustafa
 */
public final class CustomAnalyzer extends ReusableAnalyzerBase
{
    private Version matchVersion;

    public CustomAnalyzer(Version matchVersion)
    {
        this.matchVersion = matchVersion;
    }

    @Override
    protected TokenStreamComponents createComponents(String fieldName, Reader reader)
    {
        Tokenizer src = new WhitespaceTokenizer(matchVersion, reader);


        TokenStream tok = new StandardFilter(matchVersion, src);
        tok = new LowerCaseFilter(matchVersion, tok);
        tok = new ASCIIFoldingFilter(tok);
        tok = new EdgeNGramTokenFilter(
                tok, Side.FRONT, 3, 10);

        return new TokenStreamComponents(src, tok);
    }
}
