package org.istanbus.core.service.impl;

import com.google.inject.Inject;
import com.google.inject.name.Named;
import org.istanbus.core.model.node.Bus;
import org.istanbus.core.model.node.Stop;
import org.istanbus.core.service.SearchIndexService;
import org.istanbus.core.util.BusJsonParser;
import org.istanbus.core.util.FileUtils;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.Version;
import org.neo4j.graphdb.GraphDatabaseService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class SearchIndexServiceImpl implements SearchIndexService {

    private static final Logger logger = LoggerFactory.getLogger(SearchIndexServiceImpl.class);

    private String indexPath;
    private GraphDatabaseService db;
    private BusJsonParser busJsonParser;

    @Inject
    public SearchIndexServiceImpl(@Named("search.index.file.path") String indexPath, BusJsonParser busJsonParser) {
        this.indexPath = indexPath;
        this.busJsonParser = busJsonParser;
    }

    public IndexWriter openWriter() {
        File indexFolder = new File(indexPath);
        if (indexFolder.exists()) {

            logger.info("index folder already exists at {}, deleting", indexPath);
            FileUtils.deleteDirectory(indexFolder);

            logger.info("index folder creating at {}", indexPath);
            indexFolder.mkdirs();
        }
        FSDirectory directory = null;
        try {
            directory = FSDirectory.open(indexFolder);
        } catch (IOException e) {
            logger.error("could not open directory", e);
            return null;
        }

        IndexWriterConfig writerConfig = new IndexWriterConfig(Version.LUCENE_36, new StandardAnalyzer(Version.LUCENE_36));
        IndexWriter indexWriter = null;
        try {
            indexWriter = new IndexWriter(directory, writerConfig);
        } catch (IOException e) {
            logger.error("could not initialize index writer", e);
            return null;
        }
        return indexWriter;
    }

    private boolean closeWriter(IndexWriter indexWriter) {
        try {
            indexWriter.close();
        } catch (IOException e) {
            logger.error("could not close index writer", e);
            return false;
        }
        indexWriter = null;
        return true;
    }

    @Override
    public void indexStopFromBusJson(String jsonPath) {
        List<Bus> busList = busJsonParser.parse(jsonPath);

        Set<String> indexedStops = new HashSet<String>();

        // NPE is ok
        IndexWriter indexWriter = openWriter();
        for (Bus bus : busList) {
            List<Stop> stops = bus.getStopsGo();
            for (Stop stop : stops) {
                // ensure not indexed already
                if (indexedStops.add(stop.getCode()))
                {
                    Document document = getDocument(stop);
                    try {
                        indexWriter.addDocument(document);
                    } catch (IOException e) {
                        logger.error("error while adding document", e);
                    }
                }
            }
        }
        try {
            logger.info("{} stops indexed", indexWriter.numDocs());
        } catch (IOException e) {
            logger.error("error while getting doc count");
        }
        closeWriter(indexWriter);

    }

    private Document getDocument(Stop stop) {
        Field id = new Field("id", stop.getCode(), Field.Store.YES, Field.Index.NOT_ANALYZED);
        Field name = new Field("name", stop.getName(), Field.Store.YES, Field.Index.NOT_ANALYZED);

        Document document = new Document();
        document.add(id);
        document.add(name);

        String text = stop.getName().toLowerCase();
        text = text + " " + text
                .replace('ü', 'u')
                .replace('ı', 'i')
                .replace('ş', 's')
                .replace('ç', 'c')
                .replace('ö', 'o')
                .replace('ğ', 'g');

        Field textField = new Field("text", text, Field.Store.NO, Field.Index.ANALYZED);
        document.add(textField);

        return document;
    }

}
