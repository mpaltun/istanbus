package org.istanbus.core.service.impl;

import com.google.inject.Inject;
import com.google.inject.name.Named;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.IndexNotFoundException;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.queryParser.MultiFieldQueryParser;
import org.apache.lucene.queryParser.ParseException;
import org.apache.lucene.queryParser.QueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.Version;
import org.istanbus.core.lucene.CustomAnalyzer;
import org.istanbus.core.model.SearchIndex;
import org.istanbus.core.model.SearchResult;
import org.istanbus.core.model.StopSearchResult;
import org.istanbus.core.service.SearchService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SearchServiceImpl implements SearchService {

    private static final Logger logger = LoggerFactory.getLogger(SearchServiceImpl.class);
    private Map<SearchIndex, IndexSearcher> searchers;
    private SearchIndexServiceImpl searchIndexService;
    private String indexRoot;

    @Inject
    public SearchServiceImpl(@Named("search.index.root.path") String indexRoot) {
        this.indexRoot = indexRoot;

        IndexSearcher stopSearcher = initSearcher("stop");
        IndexSearcher busSearcher = initSearcher("bus");

        searchers = new HashMap<SearchIndex, IndexSearcher>();
        searchers.put(SearchIndex.stop, stopSearcher);
        searchers.put(SearchIndex.bus, busSearcher);
    }

    private IndexSearcher initSearcher(String index) {
        File indexFolder = new File(indexRoot + index);
        if (!indexFolder.exists()) {
            indexFolder.mkdirs();
        }

        FSDirectory directory = null;
        try {
            directory = FSDirectory.open(indexFolder);
        }
        catch (IOException e) {
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
    public List<SearchResult> search(String index, String keyword) {

        SearchIndex searchIndex = getSearchIndex(index);
        if (searchIndex != null) {
            Query query = getQuery(keyword);
            if (query != null) {
                return searchAndCollectResults(searchIndex, query);
            }
        }

        return Collections.EMPTY_LIST;
    }

    private List<SearchResult> searchAndCollectResults(SearchIndex searchIndex, Query query) {

        IndexSearcher searcher = searchers.get(searchIndex);

        TopDocs hits = null;
        try {
            hits = searcher.search(query, 5);
        }
        catch (IOException e) {
            logger.error("error while searching", e);
            return Collections.EMPTY_LIST;
        }

        logger.info("{} results found. index: {}, query: {}", hits.totalHits, searchIndex, query);
        return getResultsFromDocs(searchIndex, searcher, hits.scoreDocs);
    }

    private Query getQuery(String keyword) {
        MultiFieldQueryParser queryParser = new MultiFieldQueryParser(Version.LUCENE_36 ,
                                                                      new String[] {"id", "name"},
                                                                      new CustomAnalyzer(Version.LUCENE_36));
        queryParser.setDefaultOperator(QueryParser.Operator.AND);

        Query query = null;
        try {
            query = queryParser.parse(keyword);
        }
        catch (ParseException e) {
            logger.error("error while parsing query", e);
        }

        return query;
    }

    private SearchIndex getSearchIndex(String index) {
        SearchIndex searchIndex = null;
        try {
            searchIndex =  SearchIndex.valueOf(index);
        }
        catch (IllegalArgumentException exception) {
            logger.warn("index {} not found", index);
        }
        return searchIndex;
    }

    private List<SearchResult> getResultsFromDocs(SearchIndex index, IndexSearcher searcher, ScoreDoc[] docs) {
        List<SearchResult> results = new ArrayList<SearchResult>();
        for (ScoreDoc doc : docs) {
            SearchResult result = getResultFromDoc(index, searcher, doc);
            results.add(result);
        }
        return results;
    }

    private SearchResult getResultFromDoc(SearchIndex index, IndexSearcher searcher, ScoreDoc scoreDoc) {
        Document doc = null;
        try {
            doc = searcher.doc(scoreDoc.doc);
        }
        catch (IOException e) {
            logger.error("error while getting doc from stopSearcher", e);
        }

        if (doc != null) {
            String id = doc.get("id");
            String name = doc.get("name");

            // add district to result for stop search results
            if (index == SearchIndex.stop) {
                return new StopSearchResult(id, name, doc.get("district"));
            }

            return new SearchResult(id, name);
        }

        return null;
    }
}
