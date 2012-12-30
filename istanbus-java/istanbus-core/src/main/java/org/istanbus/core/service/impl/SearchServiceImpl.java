package org.istanbus.core.service.impl;

import com.google.inject.Inject;
import com.google.inject.name.Named;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.IndexNotFoundException;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.queryParser.ParseException;
import org.apache.lucene.queryParser.QueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.Version;
import org.istanbus.core.model.StopSearchResult;
import org.istanbus.core.service.SearchService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class SearchServiceImpl implements SearchService {

    private static final Logger logger = LoggerFactory.getLogger(SearchServiceImpl.class);
    private final IndexSearcher stopSearcher;
    private SearchIndexServiceImpl searchIndexService;
    private String indexRoot;

    @Inject
    public SearchServiceImpl(@Named("search.index.root.path") String indexRoot) {
        this.indexRoot = indexRoot;
        stopSearcher = initSearcher("stop");
    }

    private IndexSearcher initSearcher(String index) {
        File indexFolder = new File(indexRoot + index);
        if (!indexFolder.exists()) {
            indexFolder.mkdirs();
        }

        FSDirectory directory = null;
        try {
            directory = FSDirectory.open(indexFolder);
        } catch (IOException e) {
            logger.error("exception while opening index folder", e);
        }
        IndexReader indexReader = null;
        try {
            indexReader = IndexReader.open(directory);
        }
        catch (IndexNotFoundException e) {
            logger.error("index not found", e);
        }
        catch (IOException e) {
            logger.error("error while opening index reader", e);
        }
        return new IndexSearcher(indexReader);
    }

    @Override
    public List<StopSearchResult> searchStop(String keyword) {
        QueryParser queryParser = new QueryParser(Version.LUCENE_36, "text", new StandardAnalyzer(Version.LUCENE_36));
        Query query = null;
        try {
            query = queryParser.parse(keyword + "*");
        } catch (ParseException e) {
            logger.error("error while parsing query", e);
        }
        TopDocs hits = null;
        logger.info("searching stop for keyword {}", keyword);
        try {
            hits = stopSearcher.search(query, 5);
        } catch (IOException e) {
            logger.error("error while searching", e);
        }

        ScoreDoc[] scoreDocs = hits.scoreDocs;
        List<StopSearchResult> results = getStopsFromDocs(scoreDocs);

        return results;
    }

    private List<StopSearchResult> getStopsFromDocs(ScoreDoc[] docs) {
        List<StopSearchResult> results = new ArrayList<StopSearchResult>();
        for (ScoreDoc doc : docs) {
            StopSearchResult result = getStopFromDoc(doc);
            results.add(result);
        }
        return results;
    }

    private StopSearchResult getStopFromDoc(ScoreDoc scoreDoc) {
        Document doc = null;
        try {
            doc = stopSearcher.doc(scoreDoc.doc);
        } catch (IOException e) {
            logger.error("error while getting doc from stopSearcher", e);
        }

        StopSearchResult result = null;
        if (doc != null) {
            String id = doc.get("id");
            String name = doc.get("name");

            result = new StopSearchResult(id, name);
        }

        return result;
    }
}
